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
        "pik|year|gender|race|pobst|nr_arr_(clean|dirty)|nr_(arr|adj|incar|pro|parole)_total|citizen|nr_flags|contact"
      )
  )

numident_clean <-
  numident %>%
  lazy_dt() %>%
  # Drop individuals where all their arrests have date errors.
  filter(nr_arr_clean != 0 | nr_arr_dirty == 0) %>%
  # Drop individuals with no arrest record but some record of further CJ involvement.
  filter(
   nr_arr_total != 0 | (nr_adj_total == 0 & nr_incar_total == 0 & nr_pro_total == 0 & nr_parole_total == 0)
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
#states_long <- list(c("TX"), c("FL"))
cjars_long <- as.list(rep("arr", length(states_long)))
#cjars_long <- list(c("arr"), c("arr"))

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
        long_data <- long_data %>% left_join(cov_df, by = c("pobst" = "state", "year"))
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
# Clean arrests data.
################################################################################
arrests_file <-
  file.path("arrests_long_numident.rds")

if(!file.exists(arrests_file)) {
  arrests <-
    read_csv(
      file.path(read_dir, "arr.csv"),
      col_select = matches("pik|OFF_CD|DT_YYYY|date[12]_status")
    )
  
  arrests_long <-
    arrests %>%
    lazy_dt() %>%
    # Drop individuals not in our focal sample.
    filter(pik %in% numident_clean$pik) %>%
    # Record number of arrests with different years for arrest vs. booking.
    # Use the year from arrest if not missing, otherwise use booking.
    mutate(
      diff_years_date = ARR_ARR_DT_YYYY != ARR_BOOK_DT_YYYY & !is.na(ARR_ARR_DT_YYYY) & !is.na(ARR_BOOK_DT_YYYY),
      year = if_else(date1_status != "missing", ARR_ARR_DT_YYYY, ARR_BOOK_DT_YYYY)
    ) %>%
    mutate(
      offense_category = str_sub(ARR_OFF_CD, 1, 1),
      offense_category_bucket =
        case_when(
          offense_category == 1 ~ "violent",
          offense_category == 2 ~ "property",
          offense_category == 3 ~ "drug",
          offense_category == 4 ~ "dui",
          offense_category == 5 ~ "public_order",
          offense_category == 6 ~ "traffic",
          offense_category == 8 | offense_category == 9 ~ "other"
        ),
      temp = 1,
      id = row_number()
    ) %>%
    pivot_wider(
      id_cols = c(pik, year, diff_years_date, id),
      names_from = offense_category_bucket,
      values_from = temp,
      values_fill = 0
    ) %>%
    group_by(pik, year) %>%
    summarise(
      nr_arr = n(),
      nr_violent_arr = sum(violent),
      nr_property_arr = sum(property),
      nr_drug_arr = sum(drug),
      nr_dui_arr = sum(dui),
      nr_public_order_arr = sum(public_order),
      nr_traffic_arr = sum(traffic),
      nr_other_arr = sum(other),
      nr_diff_years_arr = sum(diff_years_date)
    ) %>%
    ungroup() %>%
    as_tibble()
  
  saveRDS(arrests_long, arrests_file)
} else {
  arrests_long <- readRDS(arrests_file)
}

################################################################################
# Merge long form data with arrest data and save the results.
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
                matches("arr$"),
                function(col) {if_else(is.na(col), 0, as.numeric(col))}
              )
            )
        },
        cjars = arrests_long
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
