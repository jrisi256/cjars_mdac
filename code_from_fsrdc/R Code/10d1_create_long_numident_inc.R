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
read_dir <- ""
meta_dir <- ""
out_dir <- ""
cjars_start_age <- 17

numident <-
  read_csv(
    file.path(read_dir, "numident_wide.csv"),
    col_select =
      matches(
        "pik|year|gender|race|pobst|nr_incar_(clean|dirty)|nr_(incar|parole)_total|citizen|nr_flags|contact"
      )
  )

numident_clean <-
  numident %>%
  lazy_dt() %>%
  # Drop individuals where all their incarcerations have date errors.
  filter(nr_incar_clean != 0 | nr_incar_dirty == 0) %>%
  # Drop individuals w/ no inc. record but some record of further CJ involvement.
  filter(nr_incar_total != 0 | nr_parole_total == 0) %>%
  mutate(age = if_else(is.na(death_year), 2023 - birth_year, death_year - birth_year)) %>%
  filter(age >= 17, gender %in% c("M", "F"), !citizenship_flag) %>%
  select(-age, -citizenship_flag, -nr_parole_total) %>%
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
cjars_long <- as.list(rep("inc", length(states_long)))

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
    df = select(numident_clean, -matches("nr_incar")),
    coverage_df = yearly_coverage_wide,
    start_end_years_df = start_end_years,
    age = cjars_start_age
  ) %>%
  bind_rows()

################################################################################
# Clean incarceration data.
################################################################################
inc_file <-
  file.path("inc_long_numident.rds")

if(!file.exists(inc_file)) {
  inc <-
    read_csv(
      file.path(read_dir, "incar.csv"),
      col_select = matches("pik|INC_ID|DT_YYYY|date[12]$|INC_FCL_CD")
    )
  
  overlap_across_incar_fcl <-
    inc %>%
    filter(pik %in% numident_clean$pik) %>%
    group_by(pick(-matches("YYYY"))) %>%
    # Transform each row into a series of rows with each new row corresponding to
    # a year in the range of years that that person for that incarceration spell
    # was incarcerated. I.e., 2019/01/01 - 2021/02/03 becomes three rows for each
    # year (2019, 2020, 2021).
    reframe(year = seq(INC_ENTRY_DT_YYYY, INC_EXIT_DT_YYYY)) %>%
    lazy_dt() %>%
    group_by(INC_ID) %>%
    mutate(start_year = min(year), end_year = max(year)) %>%
    ungroup() %>%
    # Calculate the begin and ending date for a specific year. E.g., if a person is
    # incarcerated from 2019/01/01 - 2021/02/03, we will need to create begin and
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
          # incarceration spell, use the real begin date.
          year == start_year ~ date1,
          # If the year of the row we are on corresponds to the end of the
          # incarceration spell, set the start date as the beginning of the year.
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
          # incarceration spell, set the end date as the end of the year.
          year == start_year ~ ymd(paste0(year, "-12-31")),
          # If the year of the row we are on corresponds to the end of the
          # incarceration spell, set the start date as the real end date.
          year == end_year ~ date2,
          # If the year of the row we are on corresponds to neither the end nor
          # the beginning, set the end date as the end of the year.
          year != start_year & year != end_year ~ ymd(paste0(year, "-12-31"))
        )
    ) %>%
    arrange(pik, year, begin_date) %>%
    as_tibble() %>%
    # Turn our dates officially into a new column of type "interval vector".
    mutate(range = new_iv(begin_date, end_date)) %>%
    group_by(pik, year, INC_FCL_CD) %>%
    # Find overlapping interval vectors (i.e., incarceration spells) for a given
    # person, for a given year, for a given incarceration facility type. Note, that
    # each range will already only cover one year. Also note, overlapping
    # incarceration spells for a specific person for a specific year ACROSS
    # different types of incarceration facilities will not be counted as such (for
    # now).
    mutate(group = iv_identify_group(range)) %>%
    count(pik, year, INC_FCL_CD, group) %>%
    mutate(
      begin_date = iv_start(group),
      end_date = iv_end(group)
    )
  
  overlap_total <-
    overlap_across_incar_fcl %>%
    mutate(range = new_iv(begin_date, end_date)) %>%
    # Find overlapping incarceration spells for a given person for a given year
    # regardless of if they were in the same incarceration facility or not. I.e.,
    # overlapping incarceration spells across different facilities will now be
    # counted as overlap.
    group_by(pik, year) %>%
    mutate(group = iv_identify_group(range)) %>%
    count(pik, year, group, wt = n) %>%
    mutate(
      begin_date = iv_start(group),
      end_date = iv_end(group),
      INC_FCL_CD = "all_inc"
    )
  
  inc_long <-
    bind_rows(overlap_across_incar_fcl, overlap_total) %>%
    lazy_dt() %>%
    mutate(
      nr_days_inc = as.numeric(end_date - begin_date) + 1,
      # If there is only 1 incarceration spell for a given range, there is no overlap.
      overlap = if_else(n == 1, 0, n)
    ) %>%
    # For each person, for each year, for each facility type, count the number of
    # days incarcerated, the number of times incarcerated, and the number of
    # overlapping incarceration spells.
    group_by(pik, year, INC_FCL_CD) %>%
    summarise(
      nr_inc_spells = sum(n),
      nr_inc_days = sum(nr_days_inc),
      overlap = sum(overlap)
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c("pik", "year"),
      names_from = "INC_FCL_CD",
      values_from = c("nr_inc_days", "nr_inc_spells", "overlap"),
      values_fill = 0
    ) %>%
    # Drop any overlap columns that do not track overlap across different incarceration facility types.
    select(-matches("overlap_[A-Z]{2}$")) %>%
    relocate(c("nr_inc_days_all_inc", "nr_inc_spells_all_inc", "overlap_all_inc"), .after = "year") %>%
    as_tibble()
  
  saveRDS(inc_long, inc_file)
} else {
  inc_long <- readRDS(inc_file)
}

################################################################################
# Merge long form data with incarceration data and save the results.
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
                matches("nr_inc|overlap"),
                function(col) {if_else(is.na(col), 0, col)}
              )
            )
        },
        cjars = inc_long
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
