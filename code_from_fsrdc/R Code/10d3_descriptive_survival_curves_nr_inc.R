library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(dtplyr)
library(ggplot2)
library(stringr)

read_dir <- ""
inc_path <- file.path("inc_long_numident.rds")
cjars_coverage_path <- ""
save_dir <- ""

################################################################################
# Read in data.
################################################################################
# Read in CJARS coverage data and find adequate years of coverage for adj table.
cjars_yearly_coverage <-
  read_csv(file.path(cjars_coverage_path, "cjars_yearly_coverage.csv")) %>%
  filter(cjars == "inc", prcnt_coverage >= 75) %>%
  group_by(state) %>%
  summarise(min_year = min(year), max_year = max(year))

# Read in the Numident file.
numident <-
  read_csv(
    file.path(read_dir, "numident_wide.csv"),
    col_select =
      matches(
        "pik|year|gender|race|pobst|nr_incar_(clean|dirty)|nr_(incar|parole)_total|citizen"
      )
  )

# Read in the long adjudication data.
inc_long <- readRDS(inc_path) %>% select(pik, year, nr_inc_days_all_inc)

################################################################################
# Clean Numident data so it only includes relevant observations.
################################################################################
numident_clean <-
  numident %>%
  full_join(cjars_yearly_coverage, by = c("pobst" = "state")) %>%
  mutate(
    age = if_else(is.na(death_year), 2024 - birth_year, death_year - birth_year),
    adult_year = birth_year + 17
  ) %>%
  # Drop individuals from cohorts with spotty adjudication coverage from CJARS.
  filter(adult_year >= min_year & adult_year <= max_year) %>%
  # Drop individuals where all their incarcerations have date errors.
  filter(nr_incar_clean != 0 | nr_incar_dirty == 0) %>%
  # Drop individuals w/ no inc. record but some record of further CJ involvement.
  filter(nr_incar_total != 0 | nr_parole_total == 0) %>%
  # Drop those who died b4 17, are missing gender, or have bad citizen data.
  filter(age >= 17, gender %in% c("M", "F"), !citizenship_flag) %>%
  select(-matches("parole|incar|citizen|incar_(total|clean|dirty)")) %>%
  mutate(
    adult_year_cat =
      case_when(
        adult_year >= 1973 & adult_year <= 1982 ~ "1973 - 1982",
        adult_year >= 1983 & adult_year <= 1990 ~ "1983 - 1990",
        adult_year >= 1991 & adult_year <= 1998 ~ "1991 - 1998",
        adult_year >= 1999 & adult_year <= 2006 ~ "1999 - 2006",
        adult_year >= 2007 & adult_year <= 2014 ~ "2007 - 2014",
        adult_year >= 2015 & adult_year <= 2022 ~ "2015 - 2022"
      )
  ) %>%
  as_tibble()

################################################################################
# Plot survival curves based on number of adjudications.
################################################################################
# Create survival samples.
create_inc_sample <- function(df, target_age, full_sample_df) {
  df %>%
    lazy_dt() %>%
    left_join(select(full_sample_df, pik, birth_year), by = "pik") %>%
    mutate(age_at_inc = year - birth_year) %>%
    # Count inc. after they become an adult but before they reach the cutoff age.
    filter(age_at_inc >= 17 & age_at_inc <= target_age) %>%
    # Count the cumulative number of days an individual has been incarcerated.
    count(pik, wt = nr_inc_days_all_inc, name = "total_nr_days_inc") %>%
    mutate(total_nr_years_inc = total_nr_days_inc / 365.25) %>%
    right_join(full_sample_df, by = "pik") %>%
    mutate(
      total_nr_years_inc = if_else(is.na(total_nr_years_inc), 0, total_nr_years_inc)
    ) %>%
    select(
      adult_year, death_year, age, gender, pobst, total_nr_years_inc, min_year,
      max_year, adult_year_cat
    ) %>%
    # Keep individuals who lived past the target age.
    filter(age >= target_age) %>%
    as_tibble()
}

ages <- list(17, 18, 19, 21, 24, 30)

inc_samples <-
  map(ages, create_inc_sample, df = inc_long, full_sample_df = numident_clean)

################################################################################
# Create survival tables.
create_survival_table <- function(.df, .age, ...) {
  .df <-
    .df %>%
    mutate(
      nr_years_inc_cat =
        case_when(
          total_nr_years_inc == 0 ~ "No incarceration",
          total_nr_years_inc > 0 & total_nr_years_inc <= 0.5 ~ "6 months or less",
          # Leap years.
          total_nr_years_inc > 0.5 & total_nr_years_inc <= (366/365.25) ~ "6 months (+1 day) to 1 year",
          total_nr_years_inc > (366/365.25) & total_nr_years_inc <= 2 ~ "1 year (+1 day) to 2 years",
          total_nr_years_inc > 2 & total_nr_years_inc <= 3 ~ "2 years (+1 day) to 3 years",
          total_nr_years_inc > 3 & total_nr_years_inc <= 6 ~ "3 years (+1 day) to 6 years",
          total_nr_years_inc > 6 ~ "6 (+1 day) or more years"
        ),
      nr_years_inc_cat =
        factor(
          nr_years_inc_cat,
          levels =
            c(
              "No incarceration", "6 months or less", "6 months (+1 day) to 1 year",
              "1 year (+1 day) to 2 years", "2 years (+1 day) to 3 years",
              "3 years (+1 day) to 6 years", "6 (+1 day) or more years"
            )
        )
    )
  
  # Count the total number of people (alive and dead) in each category.
  survival_n <- .df %>% count(nr_years_inc_cat, pick(...), name = "n_total")
  
  # Find the maximum age in each categorical grouping.
  max_age <-
    .df %>%
    # If an individual died beyond CJARS coverage, consider them alive.
    mutate(death_year = if_else(death_year > max_year, NA_real_, death_year)) %>%
    # Drop everyone who is alive.
    filter(!is.na(death_year)) %>%
    group_by(pick(...)) %>%
    summarise(max_age = max(age))
  
  # Find the survival rate at each age for each category by the amount of inc.
  survival_curves_df <-
    .df %>%
    # If an individual died beyond CJARS coverage, consider them alive.
    mutate(death_year = if_else(death_year > max_year, NA_real_, death_year)) %>%
    # Drop everyone who is alive.
    filter(!is.na(death_year)) %>%
    mutate(
      age = factor(age),
      gender = factor(gender),
      adult_year = factor(adult_year),
      pobst = factor(pobst)
    ) %>%
    # Count the # of deaths by the amount of inc. for each category grouping.
    count(nr_years_inc_cat, pick(...), age, name = "n_died", .drop = F) %>%
    mutate(
      across(
        matches("age|adult_year$"),
        function(col) {as.numeric(as.character(col))})
    ) %>%
    right_join(survival_n) %>%
    arrange(nr_years_inc_cat, pick(...), age) %>%
    group_by(nr_years_inc_cat, pick(...)) %>%
    # Count the cumulative number of deaths across age.
    mutate(cum_died = cumsum(n_died)) %>%
    rowwise() %>%
    # Calculate the survival rate.
    mutate(survival = 100 * (1 - (cum_died / n_total))) %>%
    ungroup() %>%
    mutate(age_first_contact = paste0("Total time inc. by age ", .age))
  
  # Drop observations that go beyond when we have coverage.
  if(nrow(max_age) == 1) {
    survival_curves_df <- survival_curves_df %>% mutate(max_age = max_age$max_age)
  } else {
    survival_curves_df <- survival_curves_df %>% full_join(max_age)
  }
  
  survival_curves_df <- survival_curves_df %>% filter(age <= max_age)
  return(survival_curves_df)
}

survival_tables <-
  pmap(list(.df = inc_samples, .age = ages), create_survival_table) |>
  bind_rows()

survival_tables_cohort <-
  pmap(
    list(.df = inc_samples, .age = ages),
    create_survival_table,
    "adult_year"
  ) |>
  bind_rows()

survival_tables_cohort_cat <-
  pmap(
    list(.df = inc_samples, .age = ages),
    create_survival_table,
    "adult_year_cat"
  ) |>
  bind_rows()

survival_tables_pobst <-
  pmap(
    list(.df = inc_samples, .age = ages),
    create_survival_table,
    "pobst"
  ) |>
  bind_rows()

survival_tables_gender <-
  pmap(
    list(.df = inc_samples, .age = ages),
    create_survival_table,
    "gender"
  ) |>
  bind_rows()

################################################################################
# Create survival graphs.
create_graph <- function(df, color_var, facet_var, facet_var2) {
  ncol <- length(unique(df[[facet_var2]]))
  
  if(facet_var2 %in% c("adult_year_cat", "pobst")) {
    scale_var <- "free"
  } else {
    scale_var <- "free_x"
  }
  
  graph <-
    ggplot(df, aes(x = age, y = survival)) +
    geom_line(aes(color = .data[[color_var]], group = .data[[color_var]])) +
    theme_bw() +
    labs(
      x = "Age",
      y = "Survival %",
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only incarcerations which happened after an individual turned 17 are counted.\n",
          "Only individuals who survived to the age of first contact are counted for survival purposes."
        )
    )
  
  if(facet_var == "") {
    graph <-
      graph +
      facet_wrap(~.data[[facet_var2]], ncol = ncol, scale = scale_var)
  } else {
    formula <- as.formula(paste0("~", facet_var, "+", facet_var2))
    graph <- graph + facet_wrap(formula, ncol = ncol, scale = scale_var)
  }
  
  if(is.numeric(df[[color_var]])) {
    graph <-
      graph +
      scale_color_viridis_c(option = "turbo", guide = guide_colorbar(reverse = T))
  } else if(color_var == "nr_years_inc_cat") {
    graph <-
      graph +
      scale_color_viridis_d(option = "turbo")
  }
  
  if(color_var != "adult_year") {
    graph <- graph + geom_point(aes(color = .data[[color_var]]))
  }
  
  return(graph)
}

dfs <-
  list(
    survival_tables, survival_tables_cohort_cat, survival_tables_gender, survival_tables_pobst,
    survival_tables_cohort, survival_tables_gender, survival_tables_pobst
  )
color_vars <-
  list(
    "nr_years_inc_cat", "nr_years_inc_cat", "nr_years_inc_cat", "nr_years_inc_cat",
    "adult_year", "gender", "pobst"
  )
facet_vars <-
  list(
    "", "age_first_contact", "age_first_contact", "age_first_contact",
    "nr_years_inc_cat", "nr_years_inc_cat", "nr_years_inc_cat"
  )
facet_vars2 <-
  list(
    "age_first_contact", "adult_year_cat", "gender", "pobst",
    "age_first_contact", "age_first_contact", "age_first_contact"
  )
names <-
  as.list(
    paste0(
      "Color: ", color_vars, ", Facet 1: ", facet_vars, ", Facet 2: ", facet_vars2
    )
  )

graphs <- pmap(list(dfs, color_vars, facet_vars, facet_vars2), create_graph)

pwalk(
  list(graphs, names),
  function(graph, name, dir) {
    if(name == "Color: nr_years_inc_cat, Facet 1: age_first_contact, Facet 2: gender") {
      height_var = 13
      width_var = 10
    } else if (name == "Color: nr_years_inc_cat, Facet 1: , Facet 2: age_first_contact") {
      height_var = 6
      width_var = 16
    } else {
      height_var = 12
      width_var = 15
    }
    
    ggsave(
      file.path(dir, paste0(name, ".png")),
      graph,
      height = height_var,
      width = width_var
    )
  },
  dir = save_dir
)
