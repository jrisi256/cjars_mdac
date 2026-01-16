library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(dtplyr)
library(ggplot2)
library(stringr)

read_dir <- ""
adj_path <- "adj_long_numident.rds"
cjars_coverage_path <- ""
save_dir <- ""

################################################################################
# Read in data.
################################################################################
# Read in CJARS coverage data and find adequate years of coverage for adj table.
cjars_yearly_coverage <-
  read_csv(file.path(cjars_coverage_path, "cjars_yearly_coverage.csv")) %>%
  filter(
    cjars == "adj", prcnt_coverage_adj_fe >= 75, prcnt_coverage_adj_mi >= 75
  ) %>%
  group_by(state) %>%
  summarise(min_year = min(year), max_year = max(year))

# Read in the Numident file.
numident <-
  read_csv(
    file.path(read_dir, "numident_wide.csv"),
    col_select =
      matches(
        "pik|year|gender|race|pobst|nr_adj_(clean|dirty)|nr_(adj|incar|pro|parole)_total|citizen"
      )
  )

# Read in the long adjudication data.
adj_long <-
  readRDS(adj_path) %>%
  select(pik, year, nr_adj, guilty_disp, notGuilty_disp, felony_guilty_gradeDisp)

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
  # Drop individuals where all their adjudications have date errors.
  filter(nr_adj_clean != 0 | nr_adj_dirty == 0) %>%
  # Drop individuals w/ no adj. record but some record of further CJ involvement.
  filter(
    nr_adj_total != 0 | (nr_incar_total == 0 & nr_pro_total == 0 & nr_parole_total == 0)
  ) %>%
  # Drop those who died b4 17, are missing gender, or have bad citizen data.
  filter(age >= 17, gender %in% c("M", "F"), !citizenship_flag) %>%
  select(-matches("pro|parole|incar|citizen|adj_(total|clean|dirty)")) %>%
  mutate(
    adult_year_cat =
      case_when(
        adult_year >= 1993 & adult_year <= 1997 ~ "1993 - 1997",
        adult_year >= 1998 & adult_year <= 2002 ~ "1998 - 2002",
        adult_year >= 2003 & adult_year <= 2007 ~ "2003 - 2007",
        adult_year >= 2008 & adult_year <= 2012 ~ "2008 - 2012",
        adult_year >= 2013 & adult_year <= 2021 ~ "2013 - 2021"
      )
  ) %>%
  as_tibble()

################################################################################
# Plot survival curves based on number of adjudications.
################################################################################
# Create survival samples.
create_adj_sample <- function(df, target_age, adj_col, col_name, full_sample_df) {
  df %>%
    lazy_dt() %>%
    left_join(select(full_sample_df, pik, birth_year), by = "pik") %>%
    mutate(age_at_adj = year - birth_year) %>%
    # Count adj. after they become an adult but before they reach the cutoff age.
    filter(age_at_adj >= 17 & age_at_adj <= target_age) %>%
    # Only count if an individual had at least one adjudication in a given year.
    filter(.data[[adj_col]] >= 1) %>%
    distinct(pik, year) %>%
    # Count the number of years each individual had an adjudication.
    count(pik, name = col_name) %>%
    right_join(full_sample_df, by = "pik") %>%
    mutate(
      "{col_name}" := if_else(is.na(.data[[col_name]]), 0, .data[[col_name]])
    ) %>%
    select(
      adult_year, death_year, age, gender, pobst, matches(col_name), min_year,
      max_year, adult_year_cat
    ) %>%
    # Keep individuals who lived past the target age.
    filter(age >= target_age) %>%
    as_tibble()
}

ages <- list(17, 18, 19, 21, 24, 17, 18, 19, 21, 24)
adj_cols <-
  list(
    "nr_adj", "nr_adj", "nr_adj", "nr_adj", "nr_adj",
    "felony_guilty_gradeDisp", "felony_guilty_gradeDisp", "felony_guilty_gradeDisp", "felony_guilty_gradeDisp", "felony_guilty_gradeDisp"
  )
col_names <-
  list(
    "nr_years_with_adj", "nr_years_with_adj", "nr_years_with_adj", "nr_years_with_adj", "nr_years_with_adj",
    "nr_years_with_gfa", "nr_years_with_gfa", "nr_years_with_gfa", "nr_years_with_gfa", "nr_years_with_gfa"
  )

adj_samples <-
  pmap(
    list(target_age = ages, adj_col = adj_cols, col_name = col_names),
    create_adj_sample,
    df = adj_long,
    full_sample_df = numident_clean
  )

################################################################################
# Create survival tables.
create_survival_table <- function(.df, .adj_col_name, .age, ...) {
  if(.adj_col_name == "nr_years_with_adj") {
    top <- 6
  } else if(.adj_col_name == "nr_years_with_gfa") {
    top <- 4
  }
  
  # If an individual had an adjudication in 6 or more years, code it as 6.
  .df <-
    .df %>%
    mutate(
      "{.adj_col_name}" :=
        if_else(.data[[.adj_col_name]] >= top, top, .data[[.adj_col_name]])
    )
  
  # Count the total number of people (alive and dead) in each category.
  survival_n <-
    .df %>%
    rename("adj_nr" = .adj_col_name) %>%
    count(adj_nr, pick(...), name = "n_total")
  
  # Find the maximum age in each categorical grouping.
  max_age <-
    .df %>%
    # If an individual died beyond CJARS coverage, consider them alive.
    mutate(death_year = if_else(death_year > max_year, NA_real_, death_year)) %>%
    # Drop everyone who is alive.
    filter(!is.na(death_year)) %>%
    group_by(pick(...)) %>%
    summarise(max_age = max(age))
  
  # Find the survival rate at each age for each category by the number of adj.
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
      adj_nr = factor(.data[[.adj_col_name]]),
      pobst = factor(pobst)
    ) %>%
    # Count the # of deaths by the # of adj. for each category grouping.
    count(adj_nr, pick(...), age, name = "n_died", .drop = F) %>%
    mutate(
      across(
        matches("age|adult_year$|adj_nr"),
        function(col) {as.numeric(as.character(col))})
    ) %>%
    right_join(survival_n) %>%
    arrange(adj_nr, pick(...), age) %>%
    group_by(adj_nr, pick(...)) %>%
    # Count the cumulative number of deaths across age.
    mutate(cum_died = cumsum(n_died)) %>%
    rowwise() %>%
    # Calculate the survival rate.
    mutate(survival = 100 * (1 - (cum_died / n_total))) %>%
    ungroup() %>%
    mutate(
      age_first_contact = paste0("Nr. of adjs. at age ", .age),
      adj_category = .adj_col_name
    )
  
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
  pmap(
    list(
      .df = adj_samples,
      .adj_col_name = col_names,
      .age = ages
    ),
    create_survival_table
  ) |>
  bind_rows()

survival_tables_cohort <-
  pmap(
    list(
      .df = adj_samples,
      .adj_col_name = col_names,
      .age = ages
    ),
    create_survival_table,
    "adult_year_cat"
  ) |>
  bind_rows()

survival_tables_pobst <-
  pmap(
    list(
      .df = adj_samples,
      .adj_col_name = col_names,
      .age = ages
    ),
    create_survival_table,
    "pobst"
  ) |>
  bind_rows()

survival_tables_gender <-
  pmap(
    list(
      .df = adj_samples,
      .adj_col_name = col_names,
      .age = ages
    ),
    create_survival_table,
    "gender"
  ) |>
  bind_rows()

################################################################################
# Create survival graphs.
create_graph <- function(df, adj_cat_str, color_var, facet_var, facet_var2) {
  ncol <- length(unique(df[[facet_var2]]))
  
  if(adj_cat_str != "") {df <- df |> filter(adj_category == adj_cat_str)}
  
  if(adj_cat_str == "nr_years_with_adj") {
    title_str <- "All adjudications"
  } else if(adj_cat_str == "nr_years_with_gfa") {
    title_str <- "Guilty felonies"
  } else if(adj_cat_str == "") {
    title_str <- "All adjudications and guilty felonies"
  }
  
  graph <-
    ggplot(df, aes(x = age, y = survival)) +
    geom_point(aes(color = .data[[color_var]])) +
    geom_line(aes(color = .data[[color_var]], group = .data[[color_var]])) +
    theme_bw() +
    labs(
      x = "Age",
      y = "Survival %",
      title = title_str,
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only adjudications which happened after an individual turned 17 are counted.\n",
          "Only individuals who survived to the age of first contact are counted for survival purposes.\n",
          "For all adjudications, individuals with 6 or more are grouped together. For guilty felonies, 4 or more are grouped together."
        )
    )
  
  if(adj_cat_str == "") {
    graph <-
      graph +
      facet_wrap(~adj_category + age_first_contact, ncol = ncol, scale = "free_x")
  } else {
    formula <- as.formula(paste0("~", facet_var, "+", facet_var2))
    graph <- graph + facet_wrap(formula, ncol = ncol, scale = "free_x")
  }

  if(is.numeric(df[[color_var]])) {
    graph <-
      graph +
      scale_color_viridis_c(option = "turbo", guide = guide_colorbar(reverse = T))
  }
  
  return(graph)
}

dfs <-
  list(
    survival_tables, survival_tables_cohort, survival_tables_gender, survival_tables_pobst,
    survival_tables_cohort, survival_tables_gender, survival_tables_pobst,
    survival_tables_cohort, survival_tables_gender, survival_tables_pobst,
    survival_tables_cohort, survival_tables_gender, survival_tables_pobst
  )
adj_cat_strs <-
  list(
    "", "nr_years_with_adj", "nr_years_with_adj", "nr_years_with_adj",
    "nr_years_with_gfa", "nr_years_with_gfa", "nr_years_with_gfa",
    "nr_years_with_adj", "nr_years_with_adj", "nr_years_with_adj",
    "nr_years_with_gfa", "nr_years_with_gfa", "nr_years_with_gfa"
  )
color_vars <-
  list(
    "adj_nr", "adj_nr", "adj_nr", "adj_nr",
    "adj_nr", "adj_nr", "adj_nr",
    "adult_year_cat", "gender", "pobst",
    "adult_year_cat", "gender", "pobst"
  )
facet_vars <-
  list(
    "adj_category", "age_first_contact", "age_first_contact", "age_first_contact",
    "age_first_contact", "age_first_contact", "age_first_contact",
    "adj_nr", "adj_nr", "adj_nr",
    "adj_nr", "adj_nr", "adj_nr"
  )
facet_vars2 <-
  list(
    "age_first_contact", "adult_year_cat", "gender", "pobst",
    "adult_year_cat", "gender", "pobst",
    "age_first_contact", "age_first_contact", "age_first_contact",
    "age_first_contact", "age_first_contact", "age_first_contact"
  )
names <-
  as.list(
    paste0(
      "Color: ", color_vars, ", Facet 1: ", facet_vars, ", Facet 2: ", facet_vars2, ", Adj. type: ", adj_cat_strs
    )
  )
  
graphs <-
  pmap(
    list(dfs, adj_cat_strs, color_vars, facet_vars, facet_vars2),
    create_graph
  )

pwalk(
  list(graphs, names),
  function(graph, name, dir) {
    ggsave(
      file.path(dir, paste0(name, ".png")),
      graph,
      height = 12,
      width = 11
    )
  },
  dir = save_dir
)
