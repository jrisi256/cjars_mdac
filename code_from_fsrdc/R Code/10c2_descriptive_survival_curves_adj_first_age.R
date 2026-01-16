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
  select(
    pik, year, nr_adj, guilty_disp, notGuilty_disp, felony_guilty_gradeDisp,
    matches("dispCharge")
  )

################################################################################
# Record 1st instance of an adj as an adult.
################################################################################
adj_first_age <-
  adj_long %>%
  lazy_dt() %>%
  left_join(select(numident, pik, birth_year, pobst), by = "pik") %>%
  left_join(cjars_yearly_coverage, by = c("pobst" = "state")) %>%
  # Drop all adjudications which happened before an individual turned 17.
  filter(year - birth_year >= 17) %>%
  # Drop adjudications from years with spotty adjudication coverage from CJARS.
  filter(year >= min_year & year <= max_year) %>%
  mutate(
    year_guilty_felony = if_else(felony_guilty_gradeDisp >= 1, year, NA_real_),
    year_guilty = if_else(guilty_disp >= 1, year, NA_real_)
  ) %>%
  group_by(pik) %>%
  summarise(
    nr_adj = sum(nr_adj),
    nr_notGuilty = sum(notGuilty_disp),
    nr_guilty = sum(guilty_disp),
    nr_guilty_felony = sum(felony_guilty_gradeDisp),
    first_adj_year = min(year),
    first_guilty_year = min(year_guilty, na.rm = T),
    first_guilty_felony_year = min(year_guilty_felony, na.rm = T)
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
  # Drop individuals where all their adjudications have date errors.
  filter(nr_adj_clean != 0 | nr_adj_dirty == 0) %>%
  # Drop individuals w/ no adj. record but some record of further CJ involvement.
  filter(
    nr_adj_total != 0 | (nr_incar_total == 0 & nr_pro_total == 0 & nr_parole_total == 0)
  ) %>%
  # Drop those who died b4 17, are missing gender, or have bad citizen data.
  filter(age >= 17, gender %in% c("M", "F"), !citizenship_flag) %>%
  left_join(adj_first_age, by = "pik") %>%
  mutate(
    age_first_adj = first_adj_year - birth_year,
    age_first_guilty = first_guilty_year - birth_year,
    age_first_felony_guilty = first_guilty_felony_year - birth_year,
    across(matches("nr_"), function(col) {if_else(is.na(col), 0, col)})
  ) %>%
  select(-matches("pro|parole|incar|first.*year|citizen|adj_(total|clean|dirty)")) %>%
  mutate(
    adult_year_cat =
      case_when(
        adult_year >= 1993 & adult_year <= 1997 ~ "1993 - 1997",
        adult_year >= 1998 & adult_year <= 2002 ~ "1998 - 2002",
        adult_year >= 2003 & adult_year <= 2007 ~ "2003 - 2007",
        adult_year >= 2008 & adult_year <= 2012 ~ "2008 - 2012",
        adult_year >= 2013 & adult_year <= 2021 ~ "2013 - 2021",
      )
  ) %>%
  as_tibble()

################################################################################
# Plot adjudications by state by charge type.
################################################################################
charge_type_age_df <-
  adj_long %>%
  inner_join(
    select(numident_clean, pik, pobst, birth_year, min_year, max_year),
    by = "pik"
  ) %>%
  # Drop all adjudications which happened before an individual turned 17.
  filter(year - birth_year >= 17) %>%
  # Drop adjudications from years with spotty adjudication coverage from CJARS.
  filter(year >= min_year & year <= max_year) %>%
  mutate(age_at_adj = year - birth_year) %>%
  pivot_longer(
    cols = matches("dispCharge"), names_to = "disp_charge", values_to = "n"
  ) %>%
  count(pobst, age_at_adj, disp_charge, wt = n) %>%
  group_by(pobst, age_at_adj) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup()

graph_charge_type_age <-
  ggplot(charge_type_age_df, aes(x = age_at_adj, y = prcnt)) +
  geom_point(aes(color = pobst)) +
  geom_line(aes(group = pobst, color = pobst)) +
  facet_wrap(~disp_charge, scale = "free_y") +
  theme_bw() +
  labs(
    x = "Age at adjudication",
    y = "Percent",
    title = "At each age (within each state), what is the most common charge type at disposition?",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year.\n",
        "Values sum to 100 within each age category within each state."
      )
  )
ggsave(
  file.path(save_dir, "charge_type_age.png"),
  graph_charge_type_age,
  width = 12,
  height = 9
)

graph_charge_type_state <-
  ggplot(charge_type_age_df, aes(x = age_at_adj, y = prcnt)) +
  geom_point(aes(color = disp_charge)) +
  geom_line(aes(group = disp_charge, color = disp_charge)) +
  facet_wrap(~pobst, scale = "free_x") +
  theme_bw() +
  labs(
    x = "Age at adjudication",
    y = "Percent",
    title = "At each age (within each state), what is the most common charge type at disposition?",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year.\n",
        "Values sum to 100 within each age category within each state."
      )
  )
ggsave(
  file.path(save_dir, "charge_type_state.png"),
  graph_charge_type_state,
  width = 12,
  height = 9
)

charge_type_df <-
  charge_type_age_df %>%
  count(pobst, disp_charge, wt = n) %>%
  group_by(pobst) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup()

graph_charge_type <-
  ggplot(charge_type_df, aes(x = pobst, y = prcnt)) +
  geom_bar(stat = "identity") +
  facet_wrap(~disp_charge) +
  theme_bw() +
  labs(
    x = "State of birth",
    y = "Percent",
    title = "Within each state, what is the most common charge type at disposition?",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year.\n",
        "Values sum to 100 across charge types within each state."
      )
  )
ggsave(
  file.path(save_dir, "charge_type.png"),
  graph_charge_type,
  width = 12,
  height = 9
)

################################################################################
# Plot rates of CJ contact across cohort.
################################################################################
# Function for finding rates of cumulative CJ contact.
find_age_first_adj_contact <- function(df, cj_col, cj_group, ...) {
  df %>%
    mutate(age_first_contact = factor(.data[[cj_col]], exclude = NULL)) %>%
    count(age_first_contact, pick(...)) %>%
    arrange(pick(...), age_first_contact) %>%
    group_by(pick(...)) %>%
    mutate(prcnt = n / sum(n) * 100, cumsum = cumsum(prcnt)) %>%
    ungroup() %>%
    mutate(age_first_contact = as.numeric(as.character(age_first_contact))) %>%
    filter(!is.na(age_first_contact)) %>%
    mutate(group = cj_group)
}

# Create graph when only using one variable.
create_graph_cj_contact_one_var <- function(df, color_var) {
  ggplot(df, aes(x = age_first_contact, y = cumsum)) +
    geom_point(aes(color = .data[[color_var]])) +
    geom_line(aes(group = .data[[color_var]], color = .data[[color_var]])) +
    labs(
      x = "Age of first contact",
      y = "Cumulative percent with contact",
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only adjudications which happened after an individual turned 17 are counted.\n",
          "Sample is limited to those individuals who turned 17 within a coverage year."
        )
    ) +
    facet_wrap(~group) +
    theme_bw()
}

adj_cols <- c("age_first_adj", "age_first_guilty", "age_first_felony_guilty")
group_names <- c("Any adjudication", "Guilty adjudication", "Guilty felony adjudication")

####################################################### One variable tables.
age_first_contact <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean
  ) %>%
  bind_rows()

age_first_contact_cohort <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "adult_year"
  ) %>%
  bind_rows()

age_first_contact_sex <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "gender"
  ) %>%
  bind_rows()

age_first_contact_state <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "pobst"
  ) %>%
  bind_rows()

####################################################### One variable graphs.
g_age <-
  ggplot(age_first_contact, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = group)) +
  geom_line(aes(color = group, group = group)) +
  labs(
    x = "Age of first contact",
    y = "Cumulative percent with contact",
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
age_first_contact_cohort_state <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "adult_year", "pobst"
  ) %>%
  bind_rows()

age_first_contact_cohort_sex <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "adult_year", "gender"
  ) %>%
  bind_rows()

age_first_contact_sex_state <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "gender", "pobst"
  ) %>%
  bind_rows()

####################################################### Two variable graphs.
g_age_sex_state <-
  ggplot(age_first_contact_sex_state, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = pobst)) +
  geom_line(aes(group = pobst, color = pobst)) +
  labs(
    x = "Age of first contact",
    y = "Cumulative percent with contact",
    color = "State of birth",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
        "Sample is limited to those individuals who turned 17 within a coverage year."
      )
  ) +
  facet_wrap(~group+gender, ncol = 2) +
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
  facet_wrap(~group+pobst, nrow = 3) +
  scale_color_continuous(type = "viridis") +
  labs(
    x = "Age of first contact",
    y = "Cumulative percent with contact",
    color = "Cohort (year turned 17)",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
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
  facet_wrap(~group+gender, nrow = 3) +
  scale_color_continuous(type = "viridis") +
  labs(
    x = "Age of first contact",
    y = "Cumulative percent with contact",
    color = "Cohort (year turned 17)",
    caption =
      paste0(
        "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
        "Only adjudications which happened after an individual turned 17 are counted.\n",
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
age_first_contact_cohort_sex_state <-
  pmap(
    list(cj_col = adj_cols, cj_group = group_names),
    find_age_first_adj_contact,
    df = numident_clean, "adult_year", "gender", "pobst"
  ) %>%
  bind_rows()

g_age_cohort_sex_state <-
  ggplot(age_first_contact_cohort_sex_state, aes(x = age_first_contact, y = cumsum)) +
  geom_point(aes(color = adult_year, shape = gender)) +
  geom_line(aes(color = adult_year, group = paste0(adult_year, gender), lty = gender)) +
  facet_wrap(~group+pobst, nrow = 3) +
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
create_survival_sample <- function(df, age_adj) {
  df %>%
    # Drop individuals who died or who are from cohorts that are not old enough.
    # Essentially, select on individuals who have made it this far in life.
    filter(age >= age_adj) %>%
    # Drop individuals from cohorts from periods where CJARS has poor coverage.
    filter(adult_year >= min_year & adult_year <= max_year) %>%
    mutate(
      any_adj_cat =
        case_when(
          is.na(age_first_adj) ~ "None_None",
          age_first_adj == age_adj ~ "First_Any adj.",
          age_first_adj > age_adj ~ "Future_Any adj.",
          age_first_adj < age_adj ~ "Prior_Any adj."
        ),
      guilty_adj_cat =
        case_when(
          is.na(age_first_adj) ~ "None_None",
          is.na(age_first_guilty) & !is.na(age_first_adj) & age_first_adj == age_adj ~ "First_Non-guilty only",
          is.na(age_first_guilty) & !is.na(age_first_adj) & age_first_adj > age_adj ~ "Future_Non-guilty only",
          is.na(age_first_guilty) & !is.na(age_first_adj) & age_first_adj < age_adj ~ "Prior_Non-guilty only",
          age_first_guilty == age_adj ~ "First_Guilty",
          age_first_guilty > age_adj ~ "Future_Guilty",
          age_first_guilty < age_adj ~ "Prior_Guilty"
        ),
      guilty_felony_adj_cat =
        case_when(
          is.na(age_first_adj) ~ "None_None",
          is.na(age_first_guilty) & !is.na(age_first_adj) & age_first_adj == age_adj ~ "First_Non-guilty only",
          is.na(age_first_guilty) & !is.na(age_first_adj) & age_first_adj > age_adj ~ "Future_Non-guilty only",
          is.na(age_first_guilty) & !is.na(age_first_adj) & age_first_adj < age_adj ~ "Prior_Non-guilty only",
          is.na(age_first_felony_guilty) & !is.na(age_first_guilty) & age_first_guilty == age_adj ~ "First_Guilty non-felony only",
          is.na(age_first_felony_guilty) & !is.na(age_first_guilty) & age_first_guilty > age_adj ~ "Future_Guilty non-felony only",
          is.na(age_first_felony_guilty) & !is.na(age_first_guilty) & age_first_guilty < age_adj ~ "Prior_Guilty non-felony only",
          age_first_felony_guilty == age_adj ~ "First_Guilty felony",
          age_first_felony_guilty > age_adj ~ "Future_Guilty felony",
          age_first_felony_guilty < age_adj ~ "Prior_Guilty felony",
        )
    ) %>%
    select(-bestrace, -birth_year, -matches("nr_|age_first"))
}

######################## Samples of individuals who lived up to the target age.
survival_samples <-
  map(
    list(
      "Age 17" = 17, "Age 18" = 18, "Age 19" = 19, "Age 21" = 21, "Age 24" = 24
    ),
    create_survival_sample,
    df = numident_clean
  )

######################################################### Survival curve tables.
create_survival_curve_tables <- function(.df, .age_adj, .adj_cat, ...) {
  # Count the total number of people in each category across age.
  survival_n <-
    .df%>%
    rename("adj_cat" = .adj_cat) %>%
    count(adj_cat, pick(...), name = "n_total")
  
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
      adj_cat = factor(.data[[.adj_cat]]),
      pobst = factor(pobst)
    ) %>%
    # Count the number of deaths by adj. type. for each category grouping.
    count(adj_cat, pick(...), age, name = "n_died", .drop = F) %>%
    mutate(
      across(
        matches("age|adult_year$"),
        function(col) {as.numeric(as.character(col))})
    ) %>%
    right_join(survival_n) %>%
    arrange(adj_cat, pick(...), age) %>%
    group_by(adj_cat, pick(...)) %>%
    # Count the cumulative number of deaths across age.
    mutate(cum_died = cumsum(n_died)) %>%
    rowwise() %>%
    # Calculate the survival rate.
    mutate(survival = 100 * (1 - (cum_died / n_total))) %>%
    ungroup() %>%
    mutate(
      age_first_contact = paste0("First contact: ", .age_adj),
      adj_group = .adj_cat
    )
  
  # Drop observations that go beyond when we have coverage.
  if(nrow(max_age) == 1) {
    survival_curves_df <- survival_curves_df %>% mutate(max_age = max_age$max_age)
  } else {
    survival_curves_df <- survival_curves_df %>% full_join(max_age)
  }

  survival_curves_df <-
    survival_curves_df %>%
    filter(age <= max_age) %>%
    separate_wider_delim(adj_cat, delim = "_", names = c("Timing", "Contact"))
  
  return(survival_curves_df)
}

survival_curves_test <-
  pmap(
    list(survival_samples, names(survival_samples)),
    function(.df, .age_adj) {
      map(
        list("any_adj_cat", "guilty_adj_cat", "guilty_felony_adj_cat"),
        create_survival_curve_tables,
        .df = .df,
        .age_adj = .age_adj
      ) |>
        bind_rows()
    }
  ) |>
  bind_rows()

survival_curves_cohort <-
  pmap(
    list(survival_samples, names(survival_samples)),
    function(.df, .age_adj) {
      map(
        list("any_adj_cat", "guilty_adj_cat", "guilty_felony_adj_cat"),
        create_survival_curve_tables,
        .df = .df,
        .age_adj = .age_adj,
        "adult_year_cat"
      ) |>
        bind_rows()
    }
  ) |>
  bind_rows()

survival_curves_pobst <-
  pmap(
    list(survival_samples, names(survival_samples)),
    function(.df, .age_adj) {
      map(
        list("any_adj_cat", "guilty_adj_cat", "guilty_felony_adj_cat"),
        create_survival_curve_tables,
        .df = .df,
        .age_adj = .age_adj,
        "pobst"
      ) |>
        bind_rows()
    }
  ) |>
  bind_rows()

survival_curves_sex <-
  pmap(
    list(survival_samples, names(survival_samples)),
    function(.df, .age_adj) {
      map(
        list("any_adj_cat", "guilty_adj_cat", "guilty_felony_adj_cat"),
        create_survival_curve_tables,
        .df = .df,
        .age_adj = .age_adj,
        "gender"
      ) |>
        bind_rows()
    }
  ) |>
  bind_rows()

######################################################### Survival curve graphs.
create_graph_adj <- function(df, adj_group_var, color_var, facet_var) {
  nrow <- length(unique(df[[facet_var]]))
  if(nrow == 0) {nrow = 1}
  
  if(adj_group_var == "guilty_felony_adj_cat") {
    df <- df |> filter(adj_group == adj_group_var, Contact %in% c("Guilty felony", "None"))
  } else {
    df <- df |> filter(adj_group == adj_group_var)
  }
  
  graph <-
    df |>
    ggplot(aes(x = age, y = survival)) +
    geom_point(aes(color = .data[[color_var]])) + 
    geom_line(aes(color = .data[[color_var]], group = .data[[color_var]])) +
    theme_bw() +
    labs(
      x = "Age",
      y = "Survival %",
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only adjudications which happened after an individual turned 17 are counted.\n",
          "Only individuals who survived to the age of first contact are counted for survival purposes."
        )
    )
  
  if(facet_var == "") {
    graph <- graph + facet_wrap(~age_first_contact + adj_group, nrow = nrow, scale = "free_x")
  } else {
    graph <-
      graph +
      facet_wrap(~.data[[facet_var]] + age_first_contact + adj_group, nrow = nrow, scale = "free_x")
  }
  
  return(graph)
}

dfs <-
  list(
    survival_curves, survival_curves_cohort, survival_curves_pobst, survival_curves_sex,
    survival_curves_cohort, survival_curves_pobst, survival_curves_sex,
    survival_curves, survival_curves_cohort, survival_curves_pobst, survival_curves_sex,
    survival_curves_cohort, survival_curves_pobst, survival_curves_sex
  )
groups <- as.list(c(rep("any_adj_cat", 7), rep("guilty_felony_adj_cat", 7)))
facet_vars <-
  list(
    "", "adult_year_cat", "pobst", "gender", "Timing", "Timing", "Timing",
    "", "adult_year_cat", "pobst", "gender", "Timing", "Timing", "Timing"
  )
color_vars <-
  list(
    "Timing", "Timing", "Timing", "Timing", "adult_year_cat", "pobst", "gender",
    "Timing", "Timing", "Timing", "Timing", "adult_year_cat", "pobst", "gender"
  )
names <-
  as.list(
    paste0(
      "Color: ", color_vars, ", Facet: ", facet_vars, ", Adj. type: ", groups)
    )

graphs_adj <- pmap(list(dfs, groups, color_vars, facet_vars), create_graph_adj)
names(graphs_adj) <- names
pwalk(
  list(graphs_adj, names(graphs_adj)),
  function(graph, name, dir) {
    ggsave(
      file.path(dir, paste0(name, ".png")),
      graph,
      width = 14,
      height = 12
    )
  },
  dir = save_dir
)

##################################### Survival curves - time since adjudication.
create_graph_ttadj <- function(df, color_var, facet_var, facet_var2) {
  df <-
    df |>
    filter(adj_group == "guilty_felony_adj_cat", Timing == "First")
  
  if(facet_var2 != "") { ncol <- length(unique(df[[facet_var2]]))}
  
  graph <-
    df |>
    mutate(
      age_first = as.numeric(str_extract(age_first_contact, "[0-9]{2}")),
      years_since_first_adj = age - age_first
    ) |>
    ggplot(aes(x = years_since_first_adj, y = survival)) +
    geom_point(aes(color = .data[[color_var]])) +
    geom_line(aes(color = .data[[color_var]], group = .data[[color_var]])) +
    theme_bw() +
    labs(
      x = "Years since first adjudication",
      y = "Survival %",
      caption =
        paste0(
          "Coverage limited to those states+years with more than 75% of county-months reporting.\n",
          "Only adjudications which happened after an individual turned 17 are counted.\n",
          "Only individuals who survived to the age of first contact are counted for survival purposes.\n",
          "Individuals in the non-guilty only or guilty non-felony only categories are only ever involved in adjudications of that type."
        )
    )
  
  if(color_var == "age_first") {
    graph <-
      graph +
      scale_color_continuous(type = "viridis") +
      labs(color = "Age of first adjudication")
  } else if(color_var == "Contact") {
    graph <- graph + labs(color = "Adjudication type")
  }

  if(facet_var2 == "") {
    graph <- graph + facet_wrap(~.data[[facet_var]], nrow = 1, scale = "free_x")
  } else {
    formula <- as.formula(paste0("~", facet_var, " + ", facet_var2))
    graph <- graph + facet_wrap(formula, ncol = ncol, scale = "free_x")
  }

  return(graph)
}

dfs_tt <-
  list(
    survival_curves, survival_curves_cohort, survival_curves_pobst, survival_curves_sex,
    survival_curves, survival_curves_cohort, survival_curves_pobst, survival_curves_sex,
    survival_curves_cohort, survival_curves_pobst, survival_curves_sex
  )
color_vars_tt <-
  list(
    "Contact", "Contact", "Contact", "Contact",
    "age_first", "age_first", "age_first", "age_first",
    "adult_year_cat", "pobst", "gender"
  )
facet_vars_tt <-
  list(
    "age_first_contact", "age_first_contact", "age_first_contact", "age_first_contact",
    "Contact", "Contact", "Contact", "Contact",
    "Contact", "Contact", "Contact"
  )
facet_vars2_tt <-
  list(
    "", "adult_year_cat", "pobst", "gender",
    "", "adult_year_cat", "pobst", "gender",
    "age_first_contact", "age_first_contact", "age_first_contact"
  )
names_tt <-
  as.list(
    paste0(
      "Color: ", color_vars_tt, ", Facet 1: ", facet_vars_tt, ", Facet 2: ", facet_vars2_tt)
  )

graphs_tt <- pmap(list(dfs_tt, color_vars_tt, facet_vars_tt, facet_vars2_tt), create_graph_ttadj)
names(graphs_tt) <- names_tt
pwalk(
  list(graphs_tt, names(graphs_tt)),
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
create_disclosure_table <- function(df) {
  df |>
    filter(
      adj_group == "guilty_felony_adj_cat" &
      Timing == "First" &
      (cum_died == 0 | cum_died >= 3)
    ) |>
    mutate(
      age_first = as.numeric(str_extract(age_first_contact, "[0-9]{2}")),
      years_since_first_adj = age - age_first,
      survival = signif(survival, 4)
    ) |>
    select(
      -Timing, -adj_group, -max_age, -age_first_contact, -n_died, -n_total,
      -cum_died
    )
}

disclosure_tables <-
  map(
    list(survival_curves, survival_curves_cohort),
    create_disclosure_table
  )

disclosure_names <-
  c(
    "years_since_first_adj_disclosure.csv",
    "years_since_first_adj_by_cohort_disclosure.csv"
  )

pwalk(
  list(disclosure_tables, disclosure_names),
  function(df, name) {
    write_csv(
      df,
      file.path(name)
    )
  }
)

# Have to get rid of cells with 2 or fewer obs. because of disclosure rules.
create_support_table <- function(df) {
  df |>
    filter(
      adj_group == "guilty_felony_adj_cat" &
      Timing == "First" &
      (cum_died == 0 | cum_died >= 3)
    ) |>
    mutate(
      age_first = as.numeric(str_extract(age_first_contact, "[0-9]{2}")),
      years_since_first_adj = age - age_first
    ) |>
    select(-Timing, -adj_group, -max_age, -age_first_contact, -n_died)
}

support_tables <-
  map(
    list(survival_curves, survival_curves_cohort),
    create_support_table
  )

support_names <-
  c(
    "years_since_first_adj_support.csv",
    "years_since_first_adj_by_cohort_support.csv"
  )

pwalk(
  list(support_tables, support_names),
  function(df, name) {
    write_csv(
      df,
      file.path(name)
    )
  }
)

################################################################################
# New request for disclosure officer.
mdac_dir <- ""

adjudication_sample <-
  numident_clean |>
  filter(
    age_first_felony_guilty %in% c(17, 18, 19, 21, 24) |
      (age_first_guilty %in% c(17, 18, 19, 21, 24) & is.na(age_first_felony_guilty)) |
      (age_first_adj %in% c(17, 18, 19, 21, 24) & is.na(age_first_guilty) & is.na(age_first_felony_guilty))
  )

state_fl <-
  read_csv(file.path(mdac_dir, "FL-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_adj_sample = if_else(pik %in% adjudication_sample$pik, T, F),
    state = "FL"
  )

state_nc <-
  read_csv(file.path(mdac_dir, "NC-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_adj_sample = if_else(pik %in% adjudication_sample$pik, T, F),
    state = "NC"
  )

state_tx <-
  read_csv(file.path(mdac_dir, "TX-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_adj_sample = if_else(pik %in% adjudication_sample$pik, T, F),
    state = "TX"
  )

state_mi <-
  read_csv(file.path(mdac_dir, "MI-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_adj_sample = if_else(pik %in% adjudication_sample$pik, T, F),
    state = "MI"
  )

state_wi <-
  read_csv(file.path(mdac_dir, "WI-adj.csv"), col_select = matches("pik")) |>
  distinct(pik) |>
  mutate(
    in_adj_sample = if_else(pik %in% adjudication_sample$pik, T, F),
    state = "WI"
  )

all_state_samples <- bind_rows(state_fl, state_nc, state_tx, state_mi, state_wi)
