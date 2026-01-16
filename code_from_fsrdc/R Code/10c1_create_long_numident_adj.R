library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(dtplyr)
library(stringr)

################################################################################
# Read in data.
################################################################################
read_dir <- ""
meta_dir <- ""
out_dir <- ""
cjars_start_age <- 17

numident <-
  read_csv(
    file.path(read_dir, "numident_wide.csv"),
    col_select =
      matches(
        "pik|year|gender|race|pobst|nr_adj_(clean|dirty)|nr_(adj|incar|pro|parole)_total|citizen|nr_flags|contact"
      )
  )

numident_clean <-
  numident %>%
  lazy_dt() %>%
  # Drop individuals where all their adjudications have date errors.
  filter(nr_adj_clean != 0 | nr_adj_dirty == 0) %>%
  # Drop individuals w/ no adj. record but some record of further CJ involvement.
  filter(
    nr_adj_total != 0 | (nr_incar_total == 0 & nr_pro_total == 0 & nr_parole_total == 0)
  ) %>%
  mutate(age = if_else(is.na(death_year), 2023 - birth_year, death_year - birth_year)) %>%
  filter(age >= 17, gender %in% c("M", "F"), !citizenship_flag) %>%
  select(-age, -citizenship_flag) %>%
  as_tibble()

yearly_coverage_long <-
  read_csv(file.path(meta_dir, "cjars_yearly_coverage.csv")) %>%
  select(-coverage)

################################################################################
# Summarize yearly coverage of CJARS.
################################################################################
start_end_years <-
  yearly_coverage_long %>%
  group_by(state, cjars) %>%
  summarise(start_year = min(year), end_year = max(year)) %>%
  ungroup()

yearly_coverage_wide <-
  yearly_coverage_long %>%
  mutate(
    prcnt_coverage_adj_mi = if_else(is.na(prcnt_coverage_adj_mi), 0, prcnt_coverage_adj_mi),
    prcnt_coverage_adj_fe = if_else(is.na(prcnt_coverage_adj_fe), 0, prcnt_coverage_adj_fe),
    coverage =
      if_else(
        cjars %in% c("arr", "inc", "par", "pro"),
        as.character(round(prcnt_coverage, 3)),
        paste0(round(prcnt_coverage_adj_fe, 3), "_", round(prcnt_coverage_adj_mi, 3))
      )
  ) %>%
  pivot_wider(
    id_cols = c("state", "year"),
    names_from = "cjars",
    values_from = "coverage",
    values_fill = "0",
    names_prefix = "coverage_"
  )

################################################################################
# Create long-form data.
################################################################################
args <- commandArgs(trailingOnly = T)
states_long <- as.list(args)
#states_long <- list(c("TX"), c("FL"), c("MI"), c("WI"), c("NC"))
cjars_long <- as.list(rep("adj", length(states_long)))
#cjars_long <- list(c("adj"), c("adj"), c("adj"), c("adj"), c("adj"))

create_long_data <- function(df, coverage_df, start_end_years_df, states, tables, age) {
  # Keep only people in the states we care about.
  df <- df %>% filter(pobst %in% states)
  
  # Find the start year and end year for the long data.
  args <- expand_grid(state = states, cjars = tables)
  yrs_avail_df <-
    inner_join(args, start_end_years) %>%
    summarise(
      start_year = max(start_year),
      end_year = min(end_year)
    )
  
  s_year <- yrs_avail_df$start_year
  e_year <- yrs_avail_df$end_year
  birth_cohort_s <- s_year - age
  birth_cohort_e <- e_year - age
  birth_cohorts <- seq(birth_cohort_s, birth_cohort_e, 1)
  birth_cohort_names <- paste0("cohort_", birth_cohorts)
  
  long_data_list <-
    map(
      birth_cohorts,
      function(birth_cohort, data, start_year, end_year, cov_df, tbls) {
        print(birth_cohort)
        data <- data %>% filter(birth_year == birth_cohort)
        
        long_data <-
          expand_grid(pik = unique(data$pik), year = birth_cohort:end_year) %>%
          lazy_dt() %>%
          full_join(data, by = "pik") %>%
          mutate(
            died =
              case_when(
                # A person is neither alive nor dead before birth and after death.
                birth_year > year | death_year < year ~ NA,
                # If the death date is missing, the person is currently still alive.
                is.na(death_year) ~ F,
                # Only code a person as dead in the year that they die.
                death_year == year ~ T,
                death_year > year ~ F
              )
          ) %>%
          filter(!is.na(died)) %>%
          mutate(age = year - birth_year) %>%
          filter(age >= 17) %>%
          as_tibble()
        
        # Join with coverage data frame.
        cov_df <- cov_df %>% select(state, year, matches(paste0(tbls, collapse = "|")))
        long_data <- long_data %>% left_join(cov_df, by = c("pobst" = "state", "year")) %>% select(-pobst)
        return(long_data)
      },
      start_year = s_year,
      end_year = e_year,
      data = df,
      cov_df = coverage_df,
      tbls = tables
    )
  
  long_data_df <-
    tibble(
      state = paste(states, collapse = "_"),
      cjars = paste(tables, collapse = "_"),
      cohort = birth_cohort_names,
      df = long_data_list
    )
  
  return(long_data_df)
}

# Create long form of data.
long_data_df <-
  pmap(
    list(
      states = states_long,
      tables = cjars_long
    ),
    create_long_data,
    df = numident_clean,
    coverage_df = yearly_coverage_wide,
    start_end_years_df = start_end_years,
    age = cjars_start_age
  ) %>%
  bind_rows()

################################################################################
# Clean adjudication data.
################################################################################
adj_file <-
  file.path("adj_long_numident.rds")

if(!file.exists(adj_file)) {
  adj <-
    read_csv(
      file.path(read_dir, "adj.csv"),
      col_select = matches("pik|DT_YYYY|date[123]_status|ADJ_.*_CD")
    ) %>%
    # NA is an acquittal. ADJ_DISP_CD column has no missings (see CJARS docs).
    mutate(
      ADJ_DISP_CD = if_else(is.na(ADJ_DISP_CD), "NA", ADJ_DISP_CD)
    )
  
  adj_long <-
    adj %>%
    lazy_dt() %>%
    filter(pik %in% numident_clean$pik) %>%
    # Record number of adjudications with at least one different year.
    # Use the year from filing if not missing, otherwise disposition, otherwise sentencing.
    mutate(
      diff_years_filing_disp = ADJ_FILE_DT_YYYY != ADJ_DISP_DT_YYYY & !is.na(ADJ_FILE_DT_YYYY) & !is.na(ADJ_DISP_DT_YYYY),
      diff_years_filing_sent = ADJ_FILE_DT_YYYY != ADJ_SENT_DT_YYYY & !is.na(ADJ_FILE_DT_YYYY) & !is.na(ADJ_SENT_DT_YYYY),
      diff_years_disp_sent = ADJ_DISP_DT_YYYY != ADJ_DISP_DT_YYYY & !is.na(ADJ_FILE_DT_YYYY) & !is.na(ADJ_DISP_DT_YYYY),
      diff_years_date = diff_years_filing_disp | diff_years_filing_sent | diff_years_disp_sent,
      year =
        case_when(
          date1_status != "missing" ~ ADJ_FILE_DT_YYYY,
          date2_status != "missing" ~ ADJ_DISP_DT_YYYY,
          date3_status != "missing" ~ ADJ_SENT_DT_YYYY
        )
    ) %>%
    mutate(
      ADJ_GRD_CD =
        case_when(
          ADJ_GRD_CD == "FE" ~ "felony_grade",
          ADJ_GRD_CD == "MI" ~ "misdemeanor_grade",
          ADJ_GRD_CD == "UU" ~ "unknown_grade"
        ),
      ADJ_DISP_CD =
        case_when(
          ADJ_DISP_CD == "DU" ~ "diversion_disp",
          ADJ_DISP_CD %in% c("GC", "GJ", "GP", "GI", "GU") ~ "guilty_disp",
          ADJ_DISP_CD %in% c("NA", "ND", "NI", "NM", "NU", "NP") ~ "notGuilty_disp",
          ADJ_DISP_CD %in% c("PT", "PU") ~ "proceduralTransferOrUnknown_disp",
          ADJ_DISP_CD == "UU" ~ "unknown_disp"
        ),
      grade_and_disp =
        case_when(
          ADJ_GRD_CD == "felony_grade" & ADJ_DISP_CD == "diversion_disp" ~ "felony_diversion_gradeDisp",
          ADJ_GRD_CD == "felony_grade" & ADJ_DISP_CD == "guilty_disp" ~ "felony_guilty_gradeDisp",
          ADJ_GRD_CD == "felony_grade" & ADJ_DISP_CD == "notGuilty_disp" ~ "felony_notGuilty_gradeDisp",
          ADJ_GRD_CD == "felony_grade" & ADJ_DISP_CD == "proceduralTransferOrUnknown_disp" ~ "felony_procedural_gradeDisp",
          ADJ_GRD_CD == "felony_grade" & ADJ_DISP_CD == "unknown_disp" ~ "felony_unknown_gradeDisp",
          ADJ_GRD_CD == "misdemeanor_grade" & ADJ_DISP_CD == "diversion_disp" ~ "misd_diversion_gradeDisp",
          ADJ_GRD_CD == "misdemeanor_grade" & ADJ_DISP_CD == "guilty_disp" ~ "misd_guilty_gradeDisp",
          ADJ_GRD_CD == "misdemeanor_grade" & ADJ_DISP_CD == "notGuilty_disp" ~ "misd_notGuilty_gradeDisp",
          ADJ_GRD_CD == "misdemeanor_grade" & ADJ_DISP_CD == "proceduralTransferOrUnknown_disp" ~ "misd_procedural_gradeDisp",
          ADJ_GRD_CD == "misdemeanor_grade" & ADJ_DISP_CD == "unknown_disp" ~ "misd_unknown_gradeDisp",
          ADJ_GRD_CD == "unknown_grade" & ADJ_DISP_CD == "diversion_disp" ~ "unknown_diversion_gradeDisp",
          ADJ_GRD_CD == "unknown_grade" & ADJ_DISP_CD == "guilty_disp" ~ "unknown_guilty_gradeDisp",
          ADJ_GRD_CD == "unknown_grade" & ADJ_DISP_CD == "notGuilty_disp" ~ "unknown_notGuilty_gradeDisp",
          ADJ_GRD_CD == "unknown_grade" & ADJ_DISP_CD == "proceduralTransferOrUnknown_disp" ~ "unknown_procedural_gradeDisp",
          ADJ_GRD_CD == "unknown_grade" & ADJ_DISP_CD == "unknown_disp" ~ "unknown_unknown_gradeDisp"
        ),
      across(matches("CHRG_OFF|DISP_OFF"), function(col) {str_sub(col, 1, 1)}),
      across(
        matches("CHRG_OFF|DISP_OFF"),
        function(col) {
          case_when(
            col == 1 ~ "violent",
            col == 2 ~ "property",
            col == 3 ~ "drug",
            col == 4 ~ "dui",
            col == 5 ~ "public_order",
            col == 6 ~ "traffic",
            col == 8 | col == 9 ~ "other"
          )
        }
      ),
      temp = 1,
      id = row_number()
    ) %>%
    pivot_longer(
      cols = matches("GRD_CD|CHRG_OFF|DISP_CD|DISP_OFF|grade_and_disp"),
      names_to = "column",
      values_to = "value",
    ) %>%
    mutate(
      value = if_else(column == "ADJ_CHRG_OFF_CD", paste0(value, "_fileCharge"), value),
      value = if_else(column == "ADJ_DISP_OFF_CD", paste0(value, "_dispCharge"), value)
    ) %>%
    pivot_wider(
      id_cols = c(pik, year, diff_years_date, id),
      names_from = "value",
      values_from = "temp",
      values_fill = 0
    ) %>%
    relocate(matches("_dispCharge"), .after = id) %>%
    relocate(matches("_fileCharge"), .after = id) %>%
    relocate(matches("_gradeDisp"), .after = id) %>%
    relocate(matches("_disp$"), .after = id) %>%
    relocate(matches("_grade$"), .after = id) %>%
    group_by(pik, year) %>%
    summarise(
      nr_adj = n(),
      across(
        matches("_disp|_file|_grade|diff_years_date"), function(col) {sum(col)}
      )
    ) %>%
    ungroup() %>%
    rename(nr_diff_years_adj = diff_years_date) %>%
    as_tibble()
  
  saveRDS(adj_long, adj_file)
} else {
  adj_long <- readRDS(adj_file)
}

################################################################################
# Merge long form data with adjudication data and save the results.
################################################################################
long_data_df <-
  long_data_df %>%
  mutate(
    df =
      map(
        df,
        function(numident_cohort, cjars) {
          print(nrow(numident_cohort))
          cjars %>%
            right_join(numident_cohort, by = c("year", "pik")) %>%
            arrange(pik, year) %>%
            mutate(
              across(
                matches("nr_adj$|diff_years|_grade|_disp|Charge"),
                function(col) {if_else(is.na(col), 0, col)}
              )
            )
        },
        cjars = adj_long
      )
  )

pwalk(
  list(long_data_df$state, long_data_df$cjars, long_data_df$cohort, long_data_df$df),
  function(states, cjars, cohort, df, file_path) {
    file_name <- paste0(states, "-", cjars, "-", cohort)
    write_csv(df, file.path(file_path, paste0(file_name, ".csv")))
    write_dta(df, file.path(file_path, paste0(file_name, ".dta")))
  },
  file_path = out_dir
)
