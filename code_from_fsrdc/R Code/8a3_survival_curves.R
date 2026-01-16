library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)

################################################################################
# Read in data.
################################################################################
data_dir <- ""
table_dir <- ""
graph_dir <- ""
disclosure_dir <- ""
support_dir <- ""

sample <-
  read_csv(
    file.path(data_dir, "8_sample_wide_focal_states.csv"),
    col_select = c(dob, dod, race_short, sex, nr_cjars_ids, mdac_wgt),
  ) %>%
  mutate(
    alive = if_else(is.na(dod), T, F),
    start_age = as.numeric((ymd("2008-01-01") - dob) / 365.25),
    end_age =
      if_else(
        is.na(dod),
        as.numeric((ymd("2024-03-01") - dob) / 365.25),
        as.numeric((dod - dob) / 365.25)
      ),
    start_age_bucket =
      case_when(
        round(start_age) <= 16 ~ "16 and under",
        round(start_age) >= 17 & round(start_age) <= 25 ~ "17 - 25",
        round(start_age) >= 26 & round(start_age) <= 35 ~ "26 - 35",
        round(start_age) >= 36 & round(start_age) <= 45 ~ "36 - 45",
        round(start_age) >= 46 & round(start_age) <= 55 ~ "46 - 55",
        round(start_age) >= 56 & round(start_age) <= 65 ~ "56 - 65",
        round(start_age) >= 66 & round(start_age) <= 75 ~ "66 - 75",
        round(start_age) >= 76 ~ "76 and over"
      ),
    end_age_bucket =
      case_when(
        round(end_age) <= 16 ~ "16 and under",
        round(end_age) >= 17 & round(end_age) <= 25 ~ "17 - 25",
        round(end_age) >= 26 & round(end_age) <= 35 ~ "26 - 35",
        round(end_age) >= 36 & round(end_age) <= 45 ~ "36 - 45",
        round(end_age) >= 46 & round(end_age) <= 55 ~ "46 - 55",
        round(end_age) >= 56 & round(end_age) <= 65 ~ "56 - 65",
        round(end_age) >= 66 & round(end_age) <= 75 ~ "66 - 75",
        round(end_age) >= 76 ~ "76 and over"
      ),
    cj_contact = if_else(nr_cjars_ids == 0, F, T),
    race =
      case_when(
        race_short == "white_nh" ~ "white",
        race_short == "black_nh" ~ "black",
        race_short == "hispanic" ~ "hisp",
        T ~ "other"
      ),
    death_year = year(dod)
  ) %>%
  select(-dob, -dod, -nr_cjars_ids, -race_short, -end_age, -start_age) %>%
  filter(end_age_bucket != "16 and under", race != "other")

################################################################################
# Generate age weights using the CJ-involved sub-sample as the target pop.
################################################################################
target_age_distribution <-
  sample %>%
  filter(cj_contact == T) %>%
  count(end_age_bucket, wt = mdac_wgt) %>%
  mutate(age_weight = n / sum(n)) %>%
  select(-n)

################################################################################
# Create age-adjusted survival rate tables.
################################################################################
calc_prop_variance <- function(p, n, weighted, type) {
  if(weighted & type == "diff") {
    # In this case, n is the effective base.
    variance <- p * (1 - p) * n
  } else if(!weighted & type == "diff") {
    variance <- p * (1 - p) / n
  } else if(weighted & type == "ratio") {
    # In this case, n is the effective base.
    variance <- (1 - p) / p * n
  } else if (!weighted & type == "ratio") {
    variance <- (1 - p) / (p * n)
  }
}

calculate_year_specific_mortality <- function(.df, .target_age_dist, .year, ...) {
  # Calculate the effective base for each age group + grouping (regardless of mortality status).
  effective_base_per_age_group <-
    .df %>%
    group_by(end_age_bucket, pick(...)) %>%
    summarise(effective_base = sum(mdac_wgt ^ 2) / sum(mdac_wgt) ^ 2)
  
  # Calculate the total number of people in the ... grouping variables (regardless of mortality status).
  n_per_group <-
    .df %>%
    group_by(pick(...)) %>%
    summarise(n_total = n(), survey_weighted_total = sum(mdac_wgt))
  
  .df %>%
    # Determine if individual was dead or alive in target year.
    mutate(
      death_status = if_else(death_year %in% 2008:.year, "Dead", "Alive")
    ) %>%
    group_by(death_status, end_age_bucket, pick(...)) %>%
    # Find the number of dead/alive.
    summarise(n_event = n(), survey_weighted_event = sum(mdac_wgt)) %>%
    group_by(end_age_bucket, pick(...)) %>%
    # Calculate the proportion dead/alive.
    mutate(weighted_prop = survey_weighted_event / sum(survey_weighted_event)) %>%
    ungroup() %>%
    # Keep only the dead.
    filter(death_status == "Dead") %>%
    full_join(effective_base_per_age_group) %>%
    # If an age bucket had no one die, record the number of deaths as zero.
    mutate(
      death_status = if_else(is.na(death_status), "Dead", death_status),
      n_event = if_else(is.na(n_event), 0, n_event),
      survey_weighted_event = if_else(is.na(survey_weighted_event), 0, survey_weighted_event),
      weighted_prop = if_else(is.na(weighted_prop), 0, weighted_prop)
    ) %>%
    # Calculate the variance for each age bucket + ... grouping variable.
    mutate(variance_weighted = calc_prop_variance(weighted_prop, effective_base, T, "diff")) %>%
    full_join(.target_age_dist, by = "end_age_bucket") %>%
    # Re-calculate the variance taking into account the age-weights.
    mutate(
      age_specific_mortality = weighted_prop * age_weight,
      age_specific_variance = variance_weighted * age_weight ^ 2,
    ) %>%
    group_by(pick(...)) %>%
    # Sum across age-buckets to find age-adjusted mortality.
    summarise(
      age_adjusted_mortality = sum(age_specific_mortality),
      n_died = sum(n_event),
      survey_weighted_died = sum(survey_weighted_event),
      age_adjusted_variance = sum(age_specific_variance),
    ) %>%
    ungroup() %>%
    mutate(age_adjusted_survival = 1 - age_adjusted_mortality, year = .year) %>%
    full_join(n_per_group) %>%
    # "n" is specific to each ... group (regardless of mortality status).
    mutate(
      naive_variance = calc_prop_variance(age_adjusted_mortality, n_total, F, "diff"),
    )
}

calc_mortality <- function(.df, .target_age_dist, .end_year, ...) {
  result_df <- tibble()
  
  for(i in 2008:.end_year) {
    result_df <-
      bind_rows(
        result_df,
        calculate_year_specific_mortality(.df, .target_age_dist, i, ...)
      )
  }
  
  return(result_df)
}

survival_adjusted_all <- calc_mortality(sample, target_age_distribution, 2023, "cj_contact")
survival_adjusted_sex <- calc_mortality(sample, target_age_distribution, 2023, "cj_contact", "sex")
survival_adjusted_race <- calc_mortality(sample, target_age_distribution, 2023, "cj_contact", "race")
survival_adjusted_race_sex <- calc_mortality(sample, target_age_distribution, 2023, "cj_contact", "race", "sex")

################################################################################
# Graph the survival rates.
################################################################################
# All
g_survival_all <-
  ggplot(survival_adjusted_all, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) + 
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "CJ contact?"
  )
ggsave(
  file.path(graph_dir, "survival_all.png"),
  g_survival_all,
  height = 7,
  width = 12
)

############################################## Sex
g_survival_sex <-
  ggplot(survival_adjusted_sex, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  facet_wrap(~sex) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "CJ contact?"
  )
ggsave(
  file.path(graph_dir, "survival_sex.png"),
  g_survival_sex,
  height = 7,
  width = 12
)

g_survival_sex_2 <-
  ggplot(survival_adjusted_sex, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  facet_wrap(~cj_contact) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "Sex"
  )
ggsave(
  file.path(graph_dir, "survival_sex_2.png"),
  g_survival_sex_2,
  height = 7,
  width = 12
)

############################################## Race
g_survival_race <-
  ggplot(survival_adjusted_race, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  facet_wrap(~race) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "CJ contact?"
  )
ggsave(
  file.path(graph_dir, "survival_race.png"),
  g_survival_race,
  height = 7,
  width = 12
)

g_survival_race_2 <-
  ggplot(survival_adjusted_race, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  facet_wrap(~cj_contact) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "Race"
  )
ggsave(
  file.path(graph_dir, "survival_race_2.png"),
  g_survival_race_2,
  height = 7,
  width = 12
)

############################################## Race and sex
g_survival_race_sex <-
  ggplot(survival_adjusted_race_sex, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  facet_wrap(~race+sex, ncol = 2) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "CJ contact?"
  )
ggsave(
  file.path(graph_dir, "survival_race_sex.png"),
  g_survival_race_sex,
  height = 9,
  width = 12
)

g_survival_race_sex_2 <-
  ggplot(survival_adjusted_race_sex, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = paste0(sex, cj_contact))) +
  geom_line(
    aes(
      color = paste0(sex, cj_contact),
      group = paste0(sex, cj_contact))
  ) +
  facet_wrap(~race) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "Sex + CJ Contact"
  )
ggsave(
  file.path(graph_dir, "survival_race_sex_2.png"),
  g_survival_race_sex_2,
  height = 9,
  width = 12
)

g_survival_race_sex_3 <-
  ggplot(survival_adjusted_race_sex, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  facet_wrap(~sex+cj_contact) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "Race"
  )
ggsave(
  file.path(graph_dir, "survival_race_sex_3.png"),
  g_survival_race_sex_3,
  height = 9,
  width = 12
)

g_survival_race_sex_4 <-
  ggplot(survival_adjusted_race_sex, aes(x = year, y = age_adjusted_survival)) +
  geom_point(aes(color = paste0(race, sex))) +
  geom_line(
    aes(
      color = paste0(race, sex),
      group = paste0(race, sex))
  ) +
  facet_wrap(~cj_contact) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Age-adjusted survival (Numident)",
    color = "Race + Sex"
  )
ggsave(
  file.path(graph_dir, "survival_race_sex_4.png"),
  g_survival_race_sex_4,
  height = 9,
  width = 12
)

################################################################################
# Save tables with comparisons of statistical significance.
################################################################################
calc_pvalue_norm <- function(estimate, type, var1, var2 = 0) {
  p_value <-
    case_when(
      type == "diff" ~ pnorm(abs(estimate / sqrt(var1 + var2)), lower.tail = F) * 2,
      type == "ratio" ~ pnorm(abs(log(estimate) / sqrt(var1 + var2)), lower.tail = F) * 2
    )
}

create_df_stat_test <- function(.df, ...) {
  .df %>%
    pivot_wider(
      id_cols = c(year, ...),
      names_from = "cj_contact",
      values_from = matches("n_|survival|variance")
    ) %>%
    mutate(
      diff = age_adjusted_survival_FALSE - age_adjusted_survival_TRUE,
    ) %>%
    mutate(
      pvalue_naive = calc_pvalue_norm(diff, "diff", naive_variance_TRUE, naive_variance_FALSE),
      pvalue_ageAdjusted = calc_pvalue_norm(diff, "diff", age_adjusted_variance_TRUE, age_adjusted_variance_FALSE)
    ) %>%
    select(-matches("variance"))
}

wide_survival_all <- create_df_stat_test(survival_adjusted_all)
wide_survival_race <- create_df_stat_test(survival_adjusted_race, "race")
wide_survival_sex <- create_df_stat_test(survival_adjusted_sex, "sex")
wide_survival_race_sex <- create_df_stat_test(survival_adjusted_race_sex, "race", "sex")

################################################################################
# Save tables for disclosure.
################################################################################
create_disclosure_table <- function(df) {
  df %>%
    select(-matches("naive")) %>%
    mutate(
      across(matches("survival|diff|pvalue"), function(col) {formatC(signif(col, 4), digits = 4)}),
      across(
        matches("n_died|n_total"),
        function(col) {
          case_when(
            col < 15 ~ "N < 15",
            col >= 15 & col <= 99 ~ as.character(plyr::round_any(col, 10)),
            col >= 100 & col <= 999 ~ as.character(plyr::round_any(col, 50)),
            col >= 1000 & col <= 9999 ~ as.character(plyr::round_any(col, 100)),
            col >= 10000 & col <= 99999 ~ as.character(plyr::round_any(col, 500)),
            col >= 100000 & col <= 999999 ~ as.character(plyr::round_any(col, 1000)),
            col >= 1000000 ~ as.character(signif(col, 4))
          )
        }
      )
    )%>%
    rename(
      "Non-JII (died)" = "n_died_FALSE",
      "JII (died)" = "n_died_TRUE",
      "Total non-JII" = "n_total_FALSE",
      "Total JII" = "n_total_TRUE",
      "Age-adjusted survival (Non-JII)" = "age_adjusted_survival_FALSE",
      "Age-adjusted survival (JII)" = "age_adjusted_survival_TRUE"
    )
}

disclosure_tables <-
  map(
    list(
      "11_S2_survival_cj" = wide_survival_all,
      "12_S2_survival_race" = wide_survival_race,
      "13_S2_survival_sex" = wide_survival_sex
    ),
    create_disclosure_table
  )
pwalk(
  list(disclosure_tables, names(disclosure_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = disclosure_dir
)

create_support_table <- function(df) {
  df %>%
    select(-matches("pvalue|diff|survival")) %>%
    rename(
      "Non-JII (died)" = "n_died_FALSE",
      "JII (died)" = "n_died_TRUE",
      "Total non-JII" = "n_total_FALSE",
      "Total JII" = "n_total_TRUE"
    )
}

support_tables <-
  map(
    list(
      "11_SU_S2_survival_cj" = wide_survival_all,
      "12_SU_S2_survival_race" = wide_survival_race,
      "13_SU_S2_survival_sex" = wide_survival_sex
    ),
    create_support_table
  )
pwalk(
  list(support_tables, names(support_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = support_dir
)

################################################################################
# Create tables displaying statistical significance.
################################################################################
create_sig_table <- function(df) {
  df %>%
    mutate(
      diff = round(diff, 4),
      across(matches("age_adjusted"), function(col) {round(col, 4)})
    ) %>%
    mutate(
      across(
        matches("pvalue"),
        function(col) {
          case_when(
            col > 0.1 ~ "Not significant",
            col <= 0.1 & col > 0.05 ~ "+",
            col <= 0.05 & col > 0.01 ~ "*",
            col <= 0.01 & col > 0.001 ~ "**",
            col <= 0.001 ~ "***"
          )
        }
      )
    )
}

sig_tables <-
  map(
    list(
      "survival_cj" = wide_survival_all,
      "survival_race" = wide_survival_race,
      "survival_sex" = wide_survival_sex,
      "survival_race_sex" = wide_survival_race_sex
    ),
    create_sig_table
  )

pwalk(
  list(sig_tables, names(sig_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = table_dir
)

################################################################################
# Calculate survival rates for each age group (thus cannot age adjust).
################################################################################
calculate_year_and_age_specific_mortality <- function(.df, .year, ...) {
  # Calculate the total number of people in the ... grouping variables (regardless of mortality status).
  n_per_group <-
    .df %>%
    group_by(pick(...)) %>%
    summarise(
      n_total = n(),
      survey_weighted_total = sum(mdac_wgt),
      effective_base = sum(mdac_wgt ^ 2) / sum(mdac_wgt) ^ 2
    )
    
  .df %>%
    # Determine if individual was dead or alive in target year.
    mutate(
      death_status = if_else(death_year %in% 2008:.year, "Dead", "Alive")
    ) %>%
    group_by(death_status, pick(...)) %>%
    # Find the number of dead/alive.
    summarise(n_event = n(), survey_weighted_event = sum(mdac_wgt)) %>%
    group_by(pick(...)) %>%
    # Calculate the proportion dead/alive.
    mutate(weighted_prop = survey_weighted_event / sum(survey_weighted_event)) %>%
    ungroup() %>%
    # Keep only the dead.
    filter(death_status == "Dead") %>%
    full_join(n_per_group) %>%
    # Calculate the variance for each age bucket (and survival rate).
    mutate(
      variance_weighted = calc_prop_variance(weighted_prop, effective_base, T, "diff"),
      # "n" is specific to each ... group (regardless of mortality status).
      naive_variance = calc_prop_variance(weighted_prop, n_total, F, "diff"),
      survival_rate = 1 - weighted_prop,
      year = .year
    )
}

################################################################################
# Calculate survival curves based on end age.
################################################################################
create_df_stat_test_noAgeAdjust <- function(.df, ...) {
  .df %>%
    pivot_wider(
      id_cols = c(year, ...),
      names_from = "cj_contact",
      values_from = matches("n_|survival|variance")
    ) %>%
    mutate(diff = survival_rate_FALSE - survival_rate_TRUE) %>%
    mutate(
      pvalue_naive = calc_pvalue_norm(diff, "diff", naive_variance_TRUE, naive_variance_FALSE),
      pvalue_weighted = calc_pvalue_norm(diff, "diff", variance_weighted_TRUE, variance_weighted_FALSE)
    ) %>%
    select(-matches("variance"))
}

create_sig_table_noAgeAdjust <- function(df) {
  df %>%
    mutate(
      diff = round(diff, 4),
      across(matches("survival|diff"), function(col) {round(col, 4)})
    ) %>%
    mutate(
      across(
        matches("pvalue"),
        function(col) {
          case_when(
            col > 0.1 ~ "Not significant",
            col <= 0.1 & col > 0.05 ~ "+",
            col <= 0.05 & col > 0.01 ~ "*",
            col <= 0.01 & col > 0.001 ~ "**",
            col <= 0.001 ~ "***"
          )
        }
      )
    )
}

survival_end_age <- tibble()
for(i in 2008:2023) {
  survival_end_age <-
    bind_rows(
      survival_end_age,
      calculate_year_and_age_specific_mortality(sample, i, "end_age_bucket", "cj_contact")
    )
}

g_survival_end_age <-
  ggplot(survival_end_age, aes(x = year, y = survival_rate)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  facet_wrap(~end_age_bucket, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Survival (Numident)",
    color = "CJ contact?",
    title = "Survival rate broken down by end age (age as of end of Numident - 2024/03/01)"
  )
ggsave(
  file.path(graph_dir, "survival_end_age.png"),
  g_survival_end_age,
  height = 9,
  width = 12
)

wide_survival_end_age <- create_df_stat_test_noAgeAdjust(survival_end_age, "end_age_bucket")
sig_table_end_age <- create_sig_table_noAgeAdjust(wide_survival_end_age) %>% arrange(end_age_bucket, year)
write_csv(sig_table_end_age, file.path(table_dir, "survival_end_age.csv"))

################################################################################
# Calculate survival curves based on start age.
################################################################################
survival_start_age <- tibble()
for(i in 2008:2023) {
  survival_start_age <-
    bind_rows(
      survival_start_age,
      calculate_year_and_age_specific_mortality(sample, i, "start_age_bucket", "cj_contact")
    )
}

g_survival_start_age <-
  ggplot(survival_start_age, aes(x = year, y = survival_rate)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  facet_wrap(~start_age_bucket, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Survival (Numident)",
    color = "CJ contact?",
    title = "Survival rate broken down by start age (age at start of MDAC - 2008/01/01)"
  )
ggsave(
  file.path(graph_dir, "survival_start_age.png"),
  g_survival_start_age,
  height = 9,
  width = 12
)

wide_survival_start_age <- create_df_stat_test_noAgeAdjust(survival_start_age, "start_age_bucket")
sig_table_start_age <- create_sig_table_noAgeAdjust(wide_survival_start_age) %>% arrange(start_age_bucket, year)
write_csv(sig_table_start_age, file.path(table_dir, "survival_start_age.csv"))
