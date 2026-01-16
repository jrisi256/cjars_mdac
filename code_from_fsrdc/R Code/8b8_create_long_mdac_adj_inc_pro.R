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

adj_long <- read_csv(file.path(input_dir, "adj_long_mdac.csv"))
incar_long <- read_csv(file.path(input_dir, "incar_long_mdac.csv"))
pro_long <- read_csv(file.path(input_dir, "pro_long_mdac.csv"))

focal_states_sample <-
  read_csv(
    file.path(out_dir, "8_mdac_wide_focal_states.csv"),
    col_select = c(pik, cmid, pnum, dob, dod, state),
    col_types = cols(cmid = "c", pik = "c")
  )

mdac_cjars_wide <-
  read_csv(
    file.path(wide_dir, "mdac_cjars_wide.csv"),
    col_select = matches("pik|nr_(adj|incar|pro|par)_(dirty|clean|total)")
  )

focal_states_sample_clean <-
  full_join(focal_states_sample, mdac_cjars_wide, by = "pik") %>%
  mutate(across(matches("nr"), function(col) {if_else(is.na(col), 0, col)})) %>%
  # Drop individuals who had only problematic dates for at least one type of CJ contact.
  filter(
    (nr_adj_dirty != nr_adj_total | nr_adj_total == 0) &
    (nr_incar_dirty != nr_incar_total | nr_incar_total == 0) &
    (nr_pro_dirty != nr_pro_total | nr_pro_total == 0)
  ) %>%
  # Drop individuals who had no incarcerations but had a parole OR who had
  # no adjudication but had an incarceration, parole, or probation.
  filter(
    (nr_adj_total != 0 | (nr_incar_total == 0 & nr_pro_total == 0 & nr_par_total == 0)) &
    (nr_incar_total != 0 | nr_par_total == 0)
  ) %>%
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
  list(c("TX"), c("MI"), c("WI"), c("NC"), c("TX", "MI", "WI", "NC"))

cjars_long <-
  list(
    c("adj", "inc", "pro"), c("adj", "inc", "pro"), c("adj", "inc", "pro"),
    c("adj", "inc", "pro"), c("adj", "inc", "pro")
  )

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
cjars_tables_all <-
  list(adj_long, incar_long, pro_long) %>%
  reduce(.f = function(x, y) {full_join(x, y, by = c("pik", "year"))}) %>%
  mutate(across(where(is.numeric), function(col) {if_else(is.na(col), 0, col)})) %>%
  relocate(
    c(
      "nr_adj", "nr_inc_days_all_inc", "nr_inc_spells_all_inc", "nr_pro_days",
      "nr_pro_spells"
    ),
    .after = pik
  )

all_long <-
  map(
    long_data,
    function(mdac_long, cjars) {
      cjars %>%
        right_join(mdac_long, by = c("year", "pik")) %>%
        select(-state) %>%
        arrange(pik, year) %>%
        mutate(
          across(
            -matches("pik|^year$|cmid|pnum|alive|age|coverage"),
            function(col) {if_else(is.na(col), 0, col)})
        )
    },
    cjars = cjars_tables_all
  )

pwalk(
  list(all_long, names(all_long)),
  function(df, name, file_path) {
    write_csv(df, file.path(file_path, paste0(name, ".csv")))
    write_dta(df, file.path(file_path, paste0(name, ".dta")))
  },
  file_path = long_dir
)
