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

# Read in the long incarceration data.
inc_long <- readRDS(inc_path) %>% select(pik, year, nr_inc_spells_all_inc)

################################################################################
# Record 1st instance of an adj as an adult.
################################################################################
inc_first_age <-
  inc_long %>%
  lazy_dt() %>%
  left_join(select(numident, pik, birth_year, pobst), by = "pik") %>%
  left_join(cjars_yearly_coverage, by = c("pobst" = "state")) %>%
  # Drop all incarcerations which happened before an individual turned 17.
  filter(year - birth_year >= 17) %>%
  # Drop incarcerations from years with spotty adjudication coverage from CJARS.
  filter(year >= min_year & year <= max_year) %>%
  group_by(pik) %>%
  summarise(
    nr_inc = sum(nr_inc_spells_all_inc),
    first_inc_year = min(year)
  ) %>%
  ungroup() %>%
  as_tibble()

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
  left_join(inc_first_age, by = "pik") %>%
  mutate(
    age_first_inc = first_inc_year - birth_year,
    across(matches("nr_"), function(col) {if_else(is.na(col), 0, col)})
  ) %>%
  select(-matches("parole|incar|first.*year|citizen")) %>%
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
# Plot rates of CJ contact across cohort.
################################################################################
# Function for finding rates of cumulative CJ contact.
find_age_first_inc_contact <- function(df, ...) {
  df %>%
    mutate(age_first_contact = factor(age_first_inc, exclude = NULL)) %>%
    count(age_first_contact, pick(...)) %>%
    arrange(pick(...), age_first_contact) %>%
    group_by(pick(...)) %>%
    mutate(prcnt = n / sum(n) * 100, cumsum = cumsum(prcnt)) %>%
    ungroup() %>%
    mutate(age_first_contact = as.numeric(as.character(age_first_contact))) %>%
    filter(!is.na(age_first_contact))
}

# Create graph when only using one variable.
create_graph_cj_contact_one_var <- function(df, color_var) {
  ggplot(df, aes(x = age_first_contact, y = cumsum)) +
    geom_point(aes(color = .data[[color_var]])) +
    geom_line(aes(group = .data[[color_var]], color = .data[[color_var]])) +
    labs(
      x = "Age of first incarceration",
      y = "Cumulative percent with incarceration",
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only incarcerations which happened after an individual turned 17 are counted.\n",
          "Sample is limited to those individuals who turned 17 within a coverage year."
        )
    ) +
    theme_bw()
}

####################################################### One variable tables.
age_first_contact <- find_age_first_inc_contact(numident_clean)
age_first_contact_cohort <- find_age_first_inc_contact(numident_clean, "adult_year")
age_first_contact_sex <- find_age_first_inc_contact(numident_clean, "gender")
age_first_contact_state <- find_age_first_inc_contact(numident_clean, "pobst")

####################################################### One variable graphs.
g_age <-
  ggplot(age_first_contact, aes(x = age_first_contact, y = cumsum)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Age of first incarceration",
    y = "Cumulative percent with incarceration",
    color = "Type of contact",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year."
      )
  ) +
  theme_bw()
ggsave(
  file.path(save_dir, "age_of_first_contact.png"),
  g_age,
  width = 12,
  height = 9
)

g_age_cohort <-
  create_graph_cj_contact_one_var(age_first_contact_cohort, "adult_year") +
  scale_color_continuous(type = "viridis")
ggsave(
  file.path(save_dir, "age_of_first_contact_by_cohort.png"),
  g_age_cohort,
  width = 12,
  height = 9
)

g_age_sex <- create_graph_cj_contact_one_var(age_first_contact_sex, "gender")
ggsave(
  file.path(save_dir, "age_of_first_contact_by_sex.png"),
  g_age_sex,
  width = 12,
  height = 9
)

g_age_state <- create_graph_cj_contact_one_var(age_first_contact_state, "pobst")
ggsave(
  file.path(save_dir, "age_of_first_contact_by_state.png"),
  g_age_state,
  width = 12,
  height = 9
)

####################################################### Two variable tables.
age_first_contact_cohort_state <- find_age_first_inc_contact(numident_clean, "adult_year", "pobst")
age_first_contact_cohort_sex <- find_age_first_inc_contact(numident_clean, "adult_year", "gender")
age_first_contact_sex_state <- find_age_first_inc_contact(numident_clean, "gender", "pobst")

####################################################### Two variable graphs.
g_age_sex_state <-
  ggplot(age_first_contact_sex_state, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = pobst)) +
  geom_line(aes(group = pobst, color = pobst)) +
  labs(
    x = "Age of first incarceration",
    y = "Cumulative percent with incarceration",
    color = "State of birth",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only incarcerations which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year."
      )
  ) +
  facet_wrap(~gender) +
  theme_bw()
ggsave(
  file.path(save_dir, "age_of_first_contact_by_sex_state.png"),
  g_age_sex_state,
  width = 9,
  height = 10
)

g_age_cohort_state <-
  ggplot(age_first_contact_cohort_state, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = adult_year)) +
  geom_line(aes(color = adult_year, group = adult_year)) +
  facet_wrap(~pobst) +
  scale_color_continuous(type = "viridis") +
  labs(
    x = "Age of first incarceration",
    y = "Cumulative percent with incarceration",
    color = "Cohort (year turned 17)",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only incarcerations which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year."
      )
  ) +
  theme_bw()
ggsave(
  file.path(save_dir, "age_of_first_contact_by_cohort_state.png"),
  g_age_cohort_state,
  width = 16,
  height = 10
)

g_age_cohort_sex <-
  ggplot(age_first_contact_cohort_sex, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = adult_year)) +
  geom_line(aes(color = adult_year, group = adult_year)) +
  facet_wrap(~gender) +
  scale_color_continuous(type = "viridis") +
  labs(
    x = "Age of first incarceration",
    y = "Cumulative percent with incarceration",
    color = "Cohort (year turned 17)",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only incarcerations which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year."
      )
  ) +
  theme_bw()
ggsave(
  file.path(save_dir, "age_of_first_contact_by_cohort_sex.png"),
  g_age_cohort_sex,
  width = 13,
  height = 10
)

############################################################## Three variables.
age_first_contact_cohort_sex_state <- find_age_first_inc_contact(numident_clean, "adult_year", "gender", "pobst")

g_age_cohort_sex_state <-
  ggplot(age_first_contact_cohort_sex_state, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = adult_year, shape = gender)) +
  geom_line(aes(color = adult_year, group = paste0(adult_year, gender), lty = gender)) +
  facet_wrap(~pobst) +
  scale_color_continuous(type = "viridis") +
  labs(
    x = "Age of first contact",
    y = "Cumulative percent with contact",
    color = "Cohort (year turned 17)",
    shape = "Sex",
    lty = "Sex",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year."
      )
  ) +
  theme_bw()
ggsave(
  file.path(save_dir, "age_of_first_contact_by_cohort_sex_state.png"),
  g_age_cohort_sex_state,
  width = 16,
  height = 12
)

################################################################################
# Plot survival curves based on timing of first adjudication.
################################################################################
create_survival_sample <- function(df, age_inc) {
  df %>%
    # Drop individuals who died or who are from cohorts that are not old enough.
    # Essentially, select on individuals who have made it this far in life.
    filter(age >= age_inc) %>%
    mutate(
      inc_cat =
        case_when(
          is.na(age_first_inc) ~ "No incarceration in coverage window",
          age_first_inc == age_inc ~ "First incarceration",
          age_first_inc > age_inc ~ "Future incarceration",
          age_first_inc < age_inc ~ "Prior incarceration"
        )
    ) %>%
    select(-pik, -bestrace, -birth_year, -matches("nr_|age_first"))
}

######################## Samples of individuals who lived up to the target age.
survival_samples <-
  map(
    list(
      "Age 17" = 17, "Age 18" = 18, "Age 19" = 19, "Age 21" = 21, "Age 24" = 24,
      "Age 30" = 30
    ),
    create_survival_sample,
    df = numident_clean
  )

######################################################### Survival curve tables.
create_survival_curve_tables <- function(.df, .age_inc, ...) {
  # Count the total number of people in each category across age.
  survival_n <- .df %>% count(inc_cat, pick(...), name = "n_total")
  
  # Find the maximum age in each categorical grouping.
  max_age <-
    .df %>%
    # If an individual died beyond CJARS coverage, consider them alive.
    mutate(death_year = if_else(death_year > max_year, NA_real_, death_year)) %>%
    # Drop everyone who is alive.
    filter(!is.na(death_year)) %>%
    group_by(pick(...)) %>%
    summarise(max_age = max(age))
  
  # Find the survival rate at each age by adj. type, by each category grouping.
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
      inc_cat = factor(inc_cat),
      pobst = factor(pobst)
    ) %>%
    # Count the number of deaths by adj. type. for each category grouping.
    count(inc_cat, pick(...), age, name = "n_died", .drop = F) %>%
    mutate(
      across(
        matches("age|adult_year$"),
        function(col) {as.numeric(as.character(col))})
    ) %>%
    right_join(survival_n) %>%
    arrange(inc_cat, pick(...), age) %>%
    group_by(inc_cat, pick(...)) %>%
    # Count the cumulative number of deaths across age.
    mutate(cum_died = cumsum(n_died)) %>%
    rowwise() %>%
    # Calculate the survival rate.
    mutate(survival = 100 * (1 - (cum_died / n_total))) %>%
    ungroup() %>%
    mutate(age_first_inc = paste0("First contact: ", .age_inc))
  
  # Drop observations that go beyond when we have coverage.
  if(nrow(max_age) == 1) {
    survival_curves_df <- survival_curves_df %>% mutate(max_age = max_age$max_age)
  } else {
    survival_curves_df <- survival_curves_df %>% full_join(max_age)
  }
  
  survival_curves_df <- survival_curves_df %>% filter(age <= max_age)
  return(survival_curves_df)
}

survival_curves <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables
  ) |>
  bind_rows()

survival_curves_cohort <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables,
    "adult_year"
  ) |>
  bind_rows()

survival_curves_cohort_sex <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables,
    "adult_year", "gender"
  ) |>
  bind_rows()

survival_curves_cohort_state <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables,
    "adult_year", "pobst"
  ) |>
  bind_rows()

survival_curves_cohort_cat <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables,
    "adult_year_cat"
  ) |>
  bind_rows()

survival_curves_sex <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables,
    "gender"
  ) |>
  bind_rows()

survival_curves_state <-
  pmap(
    list(survival_samples, names(survival_samples)),
    create_survival_curve_tables,
    "pobst"
  ) |>
  bind_rows()

######################################################### Survival curve graphs.
create_graph_inc <- function(df, color_var, facet_var, inc_cat_type) {
  if(inc_cat_type != "") {
    df <- df %>% filter(inc_cat == inc_cat_type)
    title <- paste0("Effect of incarceration: ", inc_cat_type)
  } else {
    title <- "Effect of incarceration: All incarceration statuses"
  }
  
  nrow <- length(unique(df[[facet_var]]))
  if(nrow == 0) {nrow = 1}
  
  if(facet_var %in% c("adult_year_cat", "pobst")) {
    scale = "free"
  } else {
    scale = "free_x"
  }
  
  graph <-
    df |>
    ggplot(aes(x = age, y = survival)) +
    geom_line(aes(color = .data[[color_var]], group = .data[[color_var]])) +
    theme_bw() +
    labs(
      x = "Age",
      y = "Survival %",
      title = title,
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only incarcerations which happened after an individual turned 17 are counted.\n",
          "Only individuals who survived to the age of first contact are counted for survival purposes."
        )
    )
  
  if(facet_var == "") {
    graph <- graph + facet_wrap(~age_first_inc, nrow = nrow, scale = scale)
  } else {
    graph <-
      graph +
      facet_wrap(~.data[[facet_var]] + age_first_inc, nrow = nrow, scale = scale)
  }
  
  if(color_var == "adult_year") {
    graph <-
      graph +
      scale_color_viridis_c(option = "viridis", guide = guide_colorbar(reverse = T))
  } else {
    graph <-
      graph +
      geom_point(aes(color = .data[[color_var]]))
  }
  
  return(graph)
}

dfs <-
  list(
    survival_curves,
    survival_curves_cohort_cat, survival_curves_state, survival_curves_sex,
    survival_curves_cohort, survival_curves_state, survival_curves_sex,
    survival_curves_cohort_sex, survival_curves_cohort_state
  )
facet_vars <-
  list(
    "",
    "adult_year_cat", "pobst", "gender",
    "inc_cat", "inc_cat", "inc_cat",
    "gender", "pobst"
  )
color_vars <-
  list(
    "inc_cat",
    "inc_cat", "inc_cat", "inc_cat",
    "adult_year", "pobst", "gender",
    "adult_year", "adult_year"
  )
inc_cat_types <-
  list(
    "",
    "", "", "",
    "First incarceration", "First incarceration", "First incarceration", "First incarceration", "First incarceration"
  )
names <- as.list(paste0("Color: ", color_vars, ", Facet: ", facet_vars, ", Inc. type: ", inc_cat_types))

graphs_inc <- pmap(list(dfs, color_vars, facet_vars, inc_cat_types), create_graph_inc)

pwalk(
  list(graphs_inc, names),
  function(graph, name, dir) {
    ggsave(
      file.path(dir, paste0(name, ".png")),
      graph,
      width = 16,
      height = 10
    )
  },
  dir = save_dir
)

##################################### Survival curves - time since adjudication.
create_graph_ttinc <- function(df, color_var, facet_var, facet_var2) {
  df <- df |> filter(inc_cat == "First incarceration")
  
  if(facet_var2 != "") {ncol <- length(unique(df[[facet_var2]]))}
  
  graph <-
    df |>
    mutate(
      age_first = as.numeric(str_extract(age_first_inc, "[0-9]{2}")),
      years_since_first_inc = age - age_first
    ) |>
    ggplot(aes(x = years_since_first_inc, y = survival)) +
    geom_line(aes(color = .data[[color_var]], group = .data[[color_var]])) +
    theme_bw() +
    labs(
      x = "Years since first incarceration",
      y = "Survival %",
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only incarcerations which happened after an individual turned 17 are counted.\n",
          "Only individuals who survived to the age of first contact are counted for survival purposes."
        )
    )
  
  if(color_var == "age_first") {
    graph <-
      graph +
      geom_point(aes(color = .data[[color_var]])) +
      scale_color_viridis_b(option = "viridis", guide = guide_colorbar(reverse = T)) +
      labs(color = "Age of first incarceration")
  } else if(color_var == "inc_cat") {
    graph <-
      graph +
      labs(color = "Incarceration type") +
      geom_point(aes(color = .data[[color_var]]))
  } else if(color_var == "adult_year") {
    graph <-
      graph +
      scale_color_viridis_b(option = "viridis", guide = guide_colorbar(reverse = T)) +
      labs(color = "Cohort (year turned 17)") 
  }
  
  if(facet_var2 == "") {
    graph <- graph + facet_wrap(~.data[[facet_var]], nrow = 1, scale = "free_x")
  } else {
    formula <- as.formula(paste0("~", facet_var, " + ", facet_var2))
    graph <- graph + facet_wrap(formula, ncol = ncol, scale = "free_x")
  }
  
  return(graph)
}

dfs_tt <- list(survival_curves, survival_curves_cohort_cat, survival_curves_state, survival_curves_sex)
color_vars_tt <- list("age_first", "age_first", "age_first", "age_first")
facet_vars_tt <- list("inc_cat", "inc_cat", "inc_cat", "inc_cat")
facet_vars2_tt <- list("", "adult_year_cat", "pobst", "gender")
names_tt <-
  as.list(
    paste0(
      "Color: ", color_vars_tt, ", Facet 1: ", facet_vars_tt, ", Facet 2: ", facet_vars2_tt)
  )

graphs_tt <- pmap(list(dfs_tt, color_vars_tt, facet_vars_tt, facet_vars2_tt), create_graph_ttinc)
pwalk(
  list(graphs_tt, names_tt),
  function(graph, name, dir) {
    ggsave(
      file.path(dir, paste0(name, ".png")),
      graph,
      width = 14,
      height = 11
    )
  },
  dir = save_dir
)

################################################################################
# Save support and disclosure tables.
################################################################################
# Have to get rid of cells with 2 or fewer obs. because of disclosure rules.
survival_curves_disclosure_table <-
  survival_curves |>
  filter(inc_cat == "First incarceration" & (cum_died == 0 | cum_died >= 3)) |>
  mutate(
    age_first = as.numeric(str_extract(age_first_inc, "[0-9]{2}")),
    years_since_first_inc = age - age_first,
    survival = signif(survival, 4)
  ) |>
  select(-inc_cat, -max_age, -age_first_inc, -n_died, -n_total, -cum_died)

write_csv(
  survival_curves_disclosure_table,
  file.path("years_since_first_inc_disclosure.csv")
)

# Have to get rid of cells with 2 or fewer obs. because of disclosure rules.
survival_curves_support_table <-
  survival_curves |>
  filter(inc_cat == "First incarceration" & (cum_died == 0 | cum_died >= 3)) |>
  mutate(
    age_first = as.numeric(str_extract(age_first_inc, "[0-9]{2}")),
    years_since_first_inc = age - age_first
  ) |>
  select(-inc_cat, -max_age, -age_first_inc, -n_died)

write_csv(
  survival_curves_support_table,
  file.path("years_since_first_inc_support.csv")
)

################################################################################
# New cross-tabs asked for by disclosure officer.
mdac_dir <- ""

inc_sample <-
  numident_clean |>
  filter(age_first_inc %in% c(17, 18, 19, 21, 24, 30))

state_fl <-
  read_csv(file.path(mdac_dir, "FL-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_inc_sample = if_else(pik %in% inc_sample$pik, T, F),
    state = "FL"
  )

state_nc <-
  read_csv(file.path(mdac_dir, "NC-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_inc_sample = if_else(pik %in% inc_sample$pik, T, F),
    state = "NC"
  )

state_tx <-
  read_csv(file.path(mdac_dir, "TX-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_inc_sample = if_else(pik %in% inc_sample$pik, T, F),
    state = "TX"
  )

state_mi <-
  read_csv(file.path(mdac_dir, "MI-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_inc_sample = if_else(pik %in% inc_sample$pik, T, F),
    state = "MI"
  )

state_wi <-
  read_csv(file.path(mdac_dir, "WI-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_inc_sample = if_else(pik %in% inc_sample$pik, T, F),
    state = "WI"
  )

all_state_samples <- bind_rows(state_fl, state_nc, state_tx, state_mi, state_wi)
