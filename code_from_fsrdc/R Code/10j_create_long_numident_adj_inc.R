library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(haven)
library(dtplyr)
library(lubridate)

out_dir <- ""
meta_dir <- ""
long_dir <- ""
wide_dir <- ""
save_dir <- ""
cjars_start_age <- 17

################################################################################
# Read in data.
################################################################################
adj_long <- readRDS(file.path(long_dir, "adj_long_numident.rds"))
inc_long <- readRDS(file.path(long_dir, "inc_long_numident.rds"))

numident <-
  read_csv(
    file.path(wide_dir, "numident_wide.csv"),
    col_select =
      matches(
        "pik|year|gender|race|pobst|nr_(adj|incar|pro|parole)_(clean|dirty|total)|citizen|nr_flags|contact"
      )
  )

numident_clean <-
  numident %>%
  lazy_dt() %>%
  # Drop individuals who had only problematic dates for at least one type of CJ contact.
  filter(
    (nr_adj_dirty != nr_adj_total | nr_adj_total == 0) &
    (nr_incar_dirty != nr_incar_total | nr_incar_total == 0)
  ) %>%
  # Drop individuals who had no incarcerations but had a parole OR who had
  # no adjudication but had an incarceration, parole, or probation.
  filter(
    (nr_adj_total != 0 | (nr_incar_total == 0 & nr_pro_total == 0 & nr_parole_total == 0)) &
    (nr_incar_total != 0 | nr_parole_total == 0)
  ) %>%
  mutate(age = if_else(is.na(death_year), 2023 - birth_year, death_year - birth_year)) %>%
  filter(age >= 17, gender %in% c("M", "F"), !citizenship_flag) %>%
  select(-age, -citizenship_flag, -matches("nr_")) %>%
  as_tibble()

################################################################################
# Summarize yearly coverage of CJARS.
################################################################################
yearly_coverage_long <-
  read_csv(file.path(meta_dir, "cjars_yearly_coverage.csv")) %>%
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

################################################################################
# Create long-form data.
################################################################################
args <- commandArgs(trailingOnly = T)
states_long <- as.list(args)
#states_long <- list(c("TX"), c("FL"), c("MI"), c("NC"), c("WI"))
cjars_long <- rep(list(c("adj", "inc")), length(states_long))

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
# Merge CJARS data with long data tables.
################################################################################
cjars_tables_all <-
  list(adj_long, inc_long) %>%
  reduce(.f = function(x, y) {full_join(x, y, by = c("pik", "year"))}) %>%
  mutate(across(where(is.numeric), function(col) {if_else(is.na(col), 0, col)})) %>%
  relocate(
    c("nr_adj", "nr_inc_days_all_inc", "nr_inc_spells_all_inc"),
    .after = pik
  )

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
                -matches("pik|^year$|race|gender|birth|death|cj_contact|died|age|coverage"),
                function(col) {if_else(is.na(col), 0, col)}
              )
            )
        },
        cjars = cjars_tables_all
      )
  )

pwalk(
  list(long_data_df$state, long_data_df$cjars, long_data_df$cohort, long_data_df$df),
  function(states, cjars, cohort, df, file_path) {
    file_name <- paste0(states, "-", cjars, "-", cohort)
    write_csv(df, file.path(file_path, paste0(file_name, ".csv")))
    write_dta(df, file.path(file_path, paste0(file_name, ".dta")))
  },
  file_path = save_dir
)
