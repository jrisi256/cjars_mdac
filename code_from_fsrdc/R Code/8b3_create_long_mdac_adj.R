library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(haven)
library(dtplyr)
library(stringr)
library(lubridate)

################################################################################
# Read in data.
################################################################################
out_dir <- ""
input_dir <- ""
long_dir <- ""
wide_dir <- ""
focal_states <- ""

focal_states_sample <-
  read_csv(
    file.path(out_dir, "8_mdac_wide_focal_states.csv"),
    col_select = c(pik, cmid, pnum, dob, dod, state),
    col_types = cols(cmid = "c", pik = "c")
  )

# NA is an acquittal. ADJ_DISP_CD column has no missing values (see CJARS docs).
adj <-
  read_csv(file.path(wide_dir, "adj_cj-event-level.csv")) %>%
  mutate(
    ADJ_DISP_CD = if_else(is.na(ADJ_DISP_CD), "NA", ADJ_DISP_CD)
  )

mdac_cjars_wide <-
  read_csv(
    file.path(wide_dir, "mdac_cjars_wide.csv"),
    col_select = matches("pik|nr_adj_(dirty|clean)|nr_(adj|incar|pro|par)_total")
  )

focal_states_sample_clean <-
  full_join(focal_states_sample, mdac_cjars_wide, by = "pik") %>%
  mutate(across(matches("nr"), function(col) {if_else(is.na(col), 0, col)})) %>%
  # Drop individuals who only had adjudications with problematic dates.
  filter(nr_adj_clean != 0 | nr_adj_dirty == 0) %>%
  # Drop individuals with no adjudication record but some record of further CJ involvement.
  filter(nr_adj_total != 0 | (nr_incar_total == 0 & nr_pro_total == 0 & nr_par_total == 0)) %>%
  select(-matches("nr_"))

################################################################################
# Create long-form data.
################################################################################
create_long_data <- function(df, coverage_df, start_end_years_df, states, tables) {
  # Keep only people in the states we care about.
  df <- df %>% filter(state %in% states)
  
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
  
  long_data <-
    expand_grid(pik = unique(df$pik), year = s_year:e_year) %>%
    lazy_dt() %>%
    full_join(df, by = "pik") %>%
    mutate(
      died =
        case_when(
          # A person is neither alive nor dead before birth and after death.
          year(dob) > year | year(dod) < year ~ NA,
          # If the death date is missing, the person is currently still alive.
          is.na(dod) ~ F,
          # Only code a person as dead in the year that they die.
          year(dod) == year ~ T,
          year(dod) > year ~ F
        )
    ) %>%
    filter(!is.na(died)) %>%
    mutate(age = year - year(dob)) %>%
    select(-dob, -dod) %>%
    as_tibble()
  
  # Join with coverage data frame.
  coverage_df <-
    coverage_df %>%
    select(state, year, matches(paste0(tables, collapse = "|")))
  
  long_data <- long_data %>% left_join(coverage_df, by = c("state", "year"))
  return(long_data)
}

states_long <-
  list(
    c("TX"), c("FL"), c("MI"), c("WI"), c("NC"), c("TX", "FL", "MI", "WI", "NC")
  )

cjars_long <- list(c("adj"), c("adj"), c("adj"), c("adj"), c("adj"), c("adj"))

long_data_names <-
  pmap(
    list(states_long, cjars_long),
    function(states_vector, cjars_vector) {
      paste0(
        paste0(states_vector, collapse = "_"),
        "-",
        paste0(cjars_vector, collapse = "_")
      )
    }
  ) %>%
  unlist()

# Read in CJARS yearly coverage table.
yearly_coverage_long <-
  read_csv(
    file.path("cjars_yearly_coverage.csv")
  ) %>%
  select(-coverage)

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

# Create long form of data.
long_data <-
  pmap(
    list(
      states = states_long,
      tables = cjars_long
    ),
    create_long_data,
    df = focal_states_sample_clean,
    coverage_df = yearly_coverage_wide,
    start_end_years_df = start_end_years
  )
names(long_data) <- long_data_names

################################################################################
# Merge CJARS data with long data tables.
################################################################################
adj_mdac <-
  adj %>%
  lazy_dt() %>%
  # Drop problematic entries.
  filter(!dob_dod_error & !missing_date) %>%
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
    across(
      matches("CHRG_OFF|DISP_OFF"),
      function(col) {str_sub(col, 1, 1)}
    ),
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
    temp = 1
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
    id_cols = c(CJARS_ID, pik, cmid, pnum, year, ADJ_ID, diff_years_date),
    names_from = "value",
    values_from = "temp",
    values_fill = 0
  ) %>%
  relocate(matches("_dispCharge"), .after = ADJ_ID) %>%
  relocate(matches("_fileCharge"), .after = ADJ_ID) %>%
  relocate(matches("_gradeDisp"), .after = ADJ_ID) %>%
  relocate(matches("_disp$"), .after = ADJ_ID) %>%
  relocate(matches("_grade$"), .after = ADJ_ID) %>%
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
write_csv(adj_mdac, file.path(input_dir, "adj_long_mdac.csv"))

adj_long <-
  map(
    long_data,
    function(mdac_long, cjars) {
      cjars %>%
        right_join(mdac_long, by = c("year", "pik")) %>%
        arrange(pik, year) %>%
        mutate(
          across(
            matches("nr_adj|_grade|_disp|Charge|diff_years"),
            function(col) {if_else(is.na(col), 0, col)}
          )
        ) %>%
        select(-state)
    },
    cjars = adj_mdac
  )
pwalk(
  list(adj_long, names(adj_long)),
  function(df, name, file_path) {
    write_csv(df, file.path(file_path, paste0(name, ".csv")))
    write_dta(df, file.path(file_path, paste0(name, ".dta")))
  },
  file_path = long_dir
)
