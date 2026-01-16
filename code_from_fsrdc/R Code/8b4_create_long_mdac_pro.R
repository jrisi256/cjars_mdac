library(ivs)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(haven)
library(dtplyr)
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

pro <- read_csv(file.path(wide_dir, "pro_cj-event-level.csv"))

mdac_cjars_wide <-
  read_csv(
    file.path(wide_dir, "mdac_cjars_wide.csv"),
    col_select = matches("pik|nr_pro_(dirty|clean)")
  )

focal_states_sample_clean <-
  full_join(focal_states_sample, mdac_cjars_wide, by = "pik") %>%
  mutate(across(matches("nr"), function(col) {if_else(is.na(col), 0, col)})) %>%
  # Drop individuals who only had probations with problematic dates.
  filter(nr_pro_clean != 0 | nr_pro_dirty == 0) %>%
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
  list(c("TX"), c("FL"), c("MI"), c("WI"), c("NC"), c("TX", "MI", "WI", "NC"))

cjars_long <- list(c("pro"), c("pro"), c("pro"), c("pro"), c("pro"), c("pro"))

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
pro_mdac <-
  pro %>%
  lazy_dt() %>%
  # Drop problematic entries
  filter(!dob_dod_error & !missing_date & !missing_entry_or_exit_not_both & !dates_out_of_order) %>%
  as_tibble()

pro_long <-
  pro_mdac %>%
  group_by(pick(-matches("YYYY"))) %>%
  # Transform each row into a series of rows with each new row corresponding to
  # a year in the range of years that that person was on probation. I.e.,
  # 2019/01/01 - 2021/02/03 becomes three rows for each year (2019, 2020, 2021).
  reframe(year = seq(PRO_BGN_DT_YYYY, PRO_END_DT_YYYY)) %>%
  lazy_dt() %>%
  group_by(PRO_ID) %>%
  mutate(start_year = min(year), end_year = max(year)) %>%
  ungroup() %>%
  # Calculate the begin and ending date for a specific year. E.g., if a person is
  # on probation from 2019/01/01 - 2021/02/03, we will need to create begin and
  # end dates for the three new rows. This will look like (2019/01/01 - 2019/12/31),
  # (2020/01/01 - 2020/12/31), (2021/01/01 - 2021/02/03).
  mutate(
    begin_date =
      case_when(
        # If the start/end date are the same, does not matter which date is used.
        date1 == date2 ~ date1,
        # If start/end year are the same, use the real begin date.
        start_year == end_year ~ date1,
        # If the year of the row we are on corresponds to the beginning of the
        # probation, use the real begin date.
        year == start_year ~ date1,
        # If the year of the row we are on corresponds to the end of the
        # probation, set the start date as the beginning of the year.
        year == end_year ~ ymd(paste0(year, "-01-01")),
        # If the year of the row we are on corresponds to neither the end nor
        # the beginning, set the start date as the beginning of the year.
        year != start_year & year != end_year ~ ymd(paste0(year, "-01-01"))
      ),
    end_date =
      case_when(
        # If the start/end date are the same, does not matter which date is used.
        date1 == date2 ~ date1,
        # If start/end year are the same, use the real end date.
        start_year == end_year ~ date2,
        # If the year of the row we are on corresponds to the beginning of the
        # probation, set the end date as the end of the year.
        year == start_year ~ ymd(paste0(year, "-12-31")),
        # If the year of the row we are on corresponds to the end of the
        # probation, set the start date as the real end date.
        year == end_year ~ date2,
        # If the year of the row we are on corresponds to neither the end nor
        # the beginning, set the end date as the end of the year.
        year != start_year & year != end_year ~ ymd(paste0(year, "-12-31"))
      )
  ) %>%
  arrange(pik, year, begin_date) %>%
  as_tibble() %>%
  # Turn our dates officially into a new column of type "interval vector".
  mutate(range = new_iv(begin_date, end_date), temp = 1) %>%
  pivot_wider(
    names_from = PRO_END_CD,
    values_from = temp,
    values_fill = 0,
    names_prefix = "pro_end_cd_"
  ) %>%
  # Find overlapping probation spells for a given person for a given year.
  group_by(pik, year) %>%
  mutate(group = iv_identify_group(range)) %>%
  group_by(pik, year, group) %>%
  summarise(n = n(), across(matches("pro_end_cd"), function(col) {sum(col)})) %>%
  ungroup() %>%
  mutate(
    begin_date = iv_start(group),
    end_date = iv_end(group),
    nr_days_pro = as.numeric(end_date - begin_date) + 1,
    # If there is only 1 probation spell for a given range, there is no overlap.
    overlap = if_else(n == 1, 0, n)
  ) %>%
  # For each person for each year, count the number of days on probation, the
  # number of unique probation spells, and the number of overlapping probation spells..
  group_by(pik, year) %>%
  summarise(
    nr_pro_spells = sum(n),
    nr_pro_days = sum(nr_days_pro),
    overlap_pro = sum(overlap),
    across(matches("pro_end_cd"), function(col) {sum(col)})
  ) %>%
  ungroup()
write_csv(pro_long, file.path(input_dir, "pro_long_mdac.csv"))

pro_long_final <-
  map(
    long_data,
    function(mdac_long, cjars) {
      cjars %>%
        right_join(mdac_long, by = c("year", "pik")) %>%
        arrange(pik, year) %>%
        mutate(
          across(
            matches("nr_pro|overlap|pro_end_cd"),
            function(col) {if_else(is.na(col), 0, col)}
          )
        ) %>%
        select(-state)
    },
    cjars = pro_long
  )
pwalk(
  list(pro_long_final, names(pro_long_final)),
  function(df, name, file_path) {
    write_csv(df, file.path(file_path, paste0(name, ".csv")))
    write_dta(df, file.path(file_path, paste0(name, ".dta")))
  },
  file_path = long_dir
)
