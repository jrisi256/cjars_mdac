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
focal_states <- c("FL", "MI", "NC", "TX", "WI")

focal_states_sample <-
  read_csv(
    file.path(out_dir, "8_mdac_wide_focal_states.csv"),
    col_select = c(pik, cmid, pnum, dob, dod, state),
    col_types = cols(cmid = "c", pik = "c")
  )

arr <- read_csv(file.path(wide_dir, "arr_cj-event-level.csv"))

mdac_cjars_wide <-
  read_csv(
    file.path(wide_dir, "mdac_cjars_wide.csv"),
    col_select = matches("pik|nr_arr_clean|nr_arr_dirty|nr.*total")
  )

focal_states_sample_clean <-
  full_join(focal_states_sample, mdac_cjars_wide, by = "pik") %>%
  mutate(across(matches("nr"), function(col) {if_else(is.na(col), 0, col)})) %>%
  # Drop individuals who only had arrests with problematic dates.
  filter(nr_arr_clean != 0 | nr_arr_dirty == 0) %>%
  # Drop individuals with no arrest record but some record of further CJ involvement.
  filter(nr_arr_total != 0 | (nr_adj_total == 0 & nr_incar_total == 0 & nr_pro_total == 0 & nr_par_total == 0)) %>%
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

states_long <- list(c("TX"), c("FL"), c("TX", "FL"))
cjars_long <- list(c("arr"), c("arr"), c("arr"))

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
arrests_mdac <-
  arr %>%
  lazy_dt() %>%
  # Drop problematic entries.
  filter(!dob_dod_error & !missing_date) %>%
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
    temp = 1
  ) %>%
  pivot_wider(
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
write_csv(arrests_mdac, file.path(input_dir, "arrests_long_mdac.csv"))

arrests_long <-
  map(
    long_data,
    function(mdac_long, cjars) {
      cjars %>%
        right_join(mdac_long, by = c("year", "pik")) %>%
        arrange(pik, year) %>%
        mutate(
          across(
            matches("nr.*arr"), function(col) {if_else(is.na(col), 0, col)}
          )
        ) %>%
        select(-state)
    },
    cjars = arrests_mdac
  )
pwalk(
  list(arrests_long, names(arrests_long)),
  function(df, name, file_path) {
    write_csv(df, file.path(file_path, paste0(name, ".csv")))
    write_dta(df, file.path(file_path, paste0(name, ".dta")))
  },
  file_path = long_dir
)
