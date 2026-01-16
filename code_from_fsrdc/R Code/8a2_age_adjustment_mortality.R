library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
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
    age =
      if_else(
        is.na(dod),
        as.numeric((ymd("2024-03-01") - dob) / 365.25),
        as.numeric((dod - dob) / 365.25)
      ),
    age_bucket =
      case_when(
        round(age) <= 16 ~ "16 and under",
        round(age) >= 17 & round(age) <= 25 ~ "17 - 25",
        round(age) >= 26 & round(age) <= 35 ~ "26 - 35",
        round(age) >= 36 & round(age) <= 45 ~ "36 - 45",
        round(age) >= 46 & round(age) <= 55 ~ "46 - 55",
        round(age) >= 56 & round(age) <= 65 ~ "56 - 65",
        round(age) >= 66 & round(age) <= 75 ~ "66 - 75",
        round(age) >= 76 ~ "76 and over"
      ),
    cj_contact = if_else(nr_cjars_ids == 0, F, T),
    race =
      case_when(
        race_short == "white_nh" ~ "white",
        race_short == "black_nh" ~ "black",
        race_short == "hispanic" ~ "hisp",
        T ~ "other"
      )
  ) %>%
  select(-dob, -dod, -nr_cjars_ids, -race_short, -age) %>%
  filter(age_bucket != "16 and under", race != "other")

################################################################################
# Generate age weights using the CJ-involved sub-sample as the target pop.
################################################################################
target_age_distribution <-
  sample %>%
  filter(cj_contact == T) %>%
  count(age_bucket, wt = mdac_wgt) %>%
  mutate(age_weight = n / sum(n)) %>%
  select(-n)

################################################################################
# Calculate age-adjusted mortality.
################################################################################
calc_prop_variance <- function(p = 0, n, weighted, type = "", distribution, deaths = 0) {
  if(weighted & type == "diff" & distribution == "binom") {
    # In this case, n is the effective base.
    variance <- p * (1 - p) * n
  } else if(!weighted & type == "diff" & distribution == "binom") {
    variance <- p * (1 - p) / n
  } else if(weighted & type == "ratio" & distribution == "binom") {
    # In this case, n is the effective base.
    variance <- (1 - p) / p * n
  } else if (!weighted & type == "ratio" & distribution == "binom") {
    variance <- (1 - p) / (p * n)
  } else if(weighted & distribution == "poisson") {
    # In this case, n is the effective base.
    variance <- deaths * n ^ 2
  } else if(!weighted & distribution == "poisson") {
    variance <- deaths / n ^ 2
  }
}

calc_age_adjusted_mortality <- function(.df, .target_age_df, .cod_col, ...) {
  # Calculate the effective base for each age group.
  effective_base_per_age_group <-
    .df %>%
    group_by(age_bucket, pick(...)) %>%
    summarise(effective_base = sum(mdac_wgt ^ 2) / sum(mdac_wgt) ^ 2)
  
  # Calculate the total number of people in the ... grouping variables.
  n_per_group <-
    .df %>%
    group_by(pick(...)) %>%
    summarise(n_total = n(), survey_weighted_total = sum(mdac_wgt))
  
  if(.cod_col != "none") {
    df <- .df %>% group_by(alive, age_bucket, .data[[.cod_col]], pick(...))
  } else {
    df <- .df %>% group_by(alive, age_bucket, pick(...))
  }
  
  df <-
    df %>%
    summarise(n_event = n(), survey_weighted_event = sum(mdac_wgt)) %>%
    group_by(age_bucket, pick(...)) %>%
    mutate(
      weighted_prop = survey_weighted_event / sum(survey_weighted_event)
    ) %>%
    ungroup() %>%
    filter(alive != 1) %>%
    # Effective base is specific to each age group + grouping (regardless of mortality status).
    full_join(effective_base_per_age_group) %>%
    mutate(
      variance_weighted_diff_binom = calc_prop_variance(weighted_prop, effective_base, T, "diff", "binom"),
      variance_weighted_ratio_binom = calc_prop_variance(weighted_prop, effective_base, T, "ratio", "binom"),
      variance_weighted_poisson = calc_prop_variance(n = effective_base, weighted = T, distribution = "poisson", deaths = n_event)
    ) %>%
    full_join(.target_age_df, by = "age_bucket") %>%
    mutate(
      age_specific_mortality = weighted_prop * age_weight,
      age_specific_variance_diff_binom = variance_weighted_diff_binom * age_weight ^ 2,
      age_specific_variance_ratio_binom = variance_weighted_ratio_binom * age_weight ^ 2,
      # Poisson and Fay-Feuer should match (same variance formula).
      # Fay-Feuer needs to be implemented in a special fashion to find the max weight.
      age_specific_variance_poisson = variance_weighted_poisson * age_weight ^ 2,
      fayfeuer_weight = age_weight * effective_base,
      age_specific_variance_fayfeuer = n_event * fayfeuer_weight ^ 2
    )
  
  my_group_by <- function(data, cols) {group_by(data, pick({{ cols }}))}
  
  # Find max weights for Fay-Feuer.
  max_weight_df <-
    df %>%
    pivot_wider(
      id_cols = matches("age_bucket|race|sex|cause_of_death"),
      names_from = cj_contact,
      values_from = c(n_event, fayfeuer_weight),
      values_fill = 0
    ) %>%
    # Essentially ignore any weights where the number of people who died is 0.
    mutate(
      fayfeuer_weight_max_FALSE = if_else(n_event_TRUE == 0, NA, fayfeuer_weight_FALSE),
      fayfeuer_weight_max_TRUE = if_else(n_event_FALSE == 0, NA, fayfeuer_weight_TRUE),
    ) %>%
    my_group_by(matches("sex|race|cause_of_death")) %>%
    summarise(
      fayfeuer_weight_max_FALSE = max(fayfeuer_weight_max_FALSE, na.rm = T),
      fayfeuer_weight_max_TRUE = max(fayfeuer_weight_max_TRUE, na.rm = T)
    ) %>%
    pivot_longer(
      matches("fayfeuer_weight_max"),
      names_to = "cj_contact",
      values_to = "fayfeuer_weight_max"
    ) %>%
    mutate(cj_contact = as.logical(str_extract(cj_contact, "TRUE|FALSE")))
  
  if(.cod_col != "none") {
    df <- df %>% group_by(.data[[.cod_col]], pick(...))
  } else {
    df <- df %>% group_by(pick(...))
  }
  
  df <-
    df %>%
    summarise(
      age_adjusted_mortality = sum(age_specific_mortality),
      n_died = sum(n_event),
      survey_weighted_died = sum(survey_weighted_event),
      age_adjusted_variance_diff_binom = sum(age_specific_variance_diff_binom),
      age_adjusted_variance_ratio_binom = sum(age_specific_variance_ratio_binom),
      age_adjusted_variance_poisson = sum(age_specific_variance_poisson),
      age_adjusted_variance_fayfeuer = sum(age_specific_variance_fayfeuer)
    ) %>%
    ungroup() %>%
    full_join(max_weight_df) %>%
    full_join(n_per_group) %>%
    # "n" is specific to each ... group (regardless of mortality status).
    mutate(
      naive_variance_diff_binom = calc_prop_variance(age_adjusted_mortality, n_total, F, "diff", "binom"),
      naive_variance_ratio_binom = calc_prop_variance(age_adjusted_mortality, n_total, F, "ratio", "binom")
    )
}

all_cause_cj <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_contact")
all_cause_race <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_contact", "race")
all_cause_sex <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_contact", "sex")
all_cause_race_sex <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_contact", "race", "sex")

################################################################################
# Graph differences in age-adjusted mortality for CJ vs. non-CJ.
################################################################################
# All-cause CJ contact
g_all_cause_cj <-
  ggplot(all_cause_cj, aes(x = cj_contact, y = age_adjusted_mortality)) +
  geom_point() +
  geom_line(group = 1) +
  theme_bw() +
  labs(
    x = "CJ contact",
    y = "Age-adjusted mortality (Numident)"
  )
ggsave(
  file.path(graph_dir, "all_cause_cj.png"),
  g_all_cause_cj,
  height = 9,
  width = 9
)

# All-cause race
g_all_cause_race <-
  ggplot(all_cause_race, aes(x = cj_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  theme_bw() +
  labs(
    x = "CJ contact",
    y = "Age-adjusted mortality (Numident)"
  )
ggsave(
  file.path(graph_dir, "all_cause_race.png"),
  g_all_cause_race,
  height = 9,
  width = 9
)

# All-cause sex
g_all_cause_sex <-
  ggplot(all_cause_sex, aes(x = cj_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  theme_bw() +
  labs(
    x = "CJ contact",
    y = "Age-adjusted mortality (Numident)"
  )
ggsave(
  file.path(graph_dir, "all_cause_sex.png"),
  g_all_cause_sex,
  height = 9,
  width = 9
)

# All-cause Race x Sex
g_all_cause_race_sex <-
  ggplot(all_cause_race_sex, aes(x = cj_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  facet_wrap(~race) +
  theme_bw() +
  labs(
    x = "CJ contact",
    y = "Age-adjusted mortality (Numident)"
  )
ggsave(
  file.path(graph_dir, "all_cause_race_sex.png"),
  g_all_cause_race_sex,
  height = 9,
  width = 14
)

g_all_cause_race_sex2 <-
  ggplot(all_cause_race_sex, aes(x = cj_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  facet_wrap(~sex) +
  theme_bw() +
  labs(
    x = "CJ contact",
    y = "Age-adjusted mortality (Numident)"
  )
ggsave(
  file.path(graph_dir, "all_cause_race_sex2.png"),
  g_all_cause_race_sex2,
  height = 9,
  width = 14
)

################################################################################
# Statistical tests on age-adjusted mortality.
################################################################################
calc_pvalue_norm <- function(estimate, type, var1, distr, var2 = 0, p1 = 0, p2 = 0, w1 = 0, w2 = 0) {
  # Fay-Feuer specific. Note how the estimates are inverted from the confidence intervals.
  estimate1 <- (p2 + w2) / p1
  df1 <- 2 * p1^2 / var1
  df2 <- 2 * (p2 + w2) ^ 2 / (var2 + w2 ^ 2)
  estimate2 <- p2 / (p1 + w1)
  df3 <- 2 * (p1 + w1) ^ 2 / (var1 + w1 ^ 2)
  df4 <- 2 * p2 ^ 2 / var2
  
  p_value <-
    case_when(
      type == "diff" & distr %in% c("binom", "poisson") ~ pnorm(abs(estimate / sqrt(var1 + var2)), lower.tail = F) * 2,
      type == "ratio" & distr == "binom" ~ pnorm(abs(log(estimate) / sqrt(var1 + var2)), lower.tail = F) * 2,
      type == "ratio" & distr == "poisson" ~ pnorm(abs(log(estimate) / sqrt((var1 / p1^2) + (var2 / p2^2))), lower.tail = F) * 2,
      type == "ratio" & distr == "fayfeuer" & estimate > 1 ~ (2 - 2 * pf(estimate1, df1 = df1, df2 = df2, lower.tail = F)),
      type == "ratio" & distr == "fayfeuer" & estimate <= 1 ~ (2 * pf(estimate2, df1 = df3, df2 = df4, lower.tail = F))
    )
}

calc_ci_norm <- function(estimate, l_u, type, zscore, var1, var2, distr, p1 = 0, p2 = 0, w1 = 0, w2 = 0, alpha_level = 0) {
  margin_of_error <- zscore * sqrt(var1 + var2)
  moe_p_ratio <- zscore * sqrt((var1 / p1^2) + (var2 / p2^2))
  
  # Fay-Feuer specific
  estimate1 <- p1 / (p2 + w2)
  df1 <- 2 * p1^2 / var1
  df2 <- 2 * (p2 + w2) ^ 2 / (var2 + w2 ^ 2)
  estimate2 <- (p1 + w1) / p2
  df3 <- 2 * (p1 + w1) ^ 2 / (var1 + w1 ^ 2)
  df4 <- 2 * p2 ^ 2 / var2
  
  ci <-
    case_when(
      l_u == "l" & type == "diff" & distr %in% c("binom", "poisson") ~ estimate - margin_of_error,
      l_u == "u" & type == "diff" & distr %in% c("binom", "poisson") ~ estimate + margin_of_error,
      l_u == "l" & type == "ratio" & distr == "binom" ~ exp(log(estimate) - margin_of_error),
      l_u == "u" & type == "ratio" & distr == "binom" ~ exp(log(estimate) + margin_of_error),
      l_u == "l" & type == "ratio" & distr == "poisson" ~ exp(log(estimate) - moe_p_ratio),
      l_u == "u" & type == "ratio" & distr == "poisson" ~ exp(log(estimate) + moe_p_ratio),
      l_u == "l" & type == "ratio" & distr == "fayfeuer" ~ estimate1 * qf(alpha_level, df1 = df1, df2 = df2, lower.tail = F),
      l_u == "u" & type == "ratio" & distr == "fayfeuer" ~ estimate2 * qf(1 - alpha_level, df1 = df3, df2 = df4, lower.tail = F)
    )
}

create_df_stat_test <- function(.df, ci, ...) {
  alpha_level <- (1 - ci) / 2 + ci
  zscore <- qnorm(alpha_level)
  ci_lname_nb <- paste0("ci", ci * 100, "_lower_naive_binom")
  ci_uname_nb <- paste0("ci", ci * 100, "_upper_naive_binom")
  ci_lname_wb <- paste0("ci", ci * 100, "_lower_ageAdjusted_binom")
  ci_uname_wb <- paste0("ci", ci * 100, "_upper_ageAdjusted_binom")
  ci_lname_wp <- paste0("ci", ci * 100, "_lower_ageAdjusted_poisson")
  ci_uname_wp <- paste0("ci", ci * 100, "_upper_ageAdjusted_poisson")
  ci_lname_ff <- paste0("ci", ci * 100, "_lower_ageAdjusted_fayfeuer")
  ci_uname_ff <- paste0("ci", ci * 100, "_upper_ageAdjusted_fayfeuer")
  
  .df %>%
    pivot_wider(
      id_cols = c(...),
      names_from = "cj_contact",
      values_from = c(matches("n_|mortality|variance|survey_weight|weight_max"))
    ) %>%
    mutate(
      diff = age_adjusted_mortality_TRUE - age_adjusted_mortality_FALSE,
      ratio = age_adjusted_mortality_TRUE / age_adjusted_mortality_FALSE
    ) %>%
    pivot_longer(
      cols = c("diff", "ratio"), names_to = "estimate_type", values_to = "estimate"
    ) %>%
    pivot_longer(
      cols = matches("variance"), names_to = "variance_type", values_to = "variance"
    ) %>%
    # Use the same variance for difference and ratio for the the poisson and Fay-Feuer.
    filter(str_detect(variance_type, estimate_type) | str_detect(variance_type, "poisson|fayfeuer")) %>%
    mutate(variance_type = str_remove(variance_type, paste0(estimate_type, "_"))) %>%
    pivot_wider(names_from = variance_type, values_from = variance) %>%
    mutate(
      pvalue_naive_binom = calc_pvalue_norm(estimate, estimate_type, naive_variance_binom_TRUE, "binom", naive_variance_binom_FALSE),
      pvalue_ageAdjusted_binom = calc_pvalue_norm(estimate, estimate_type, age_adjusted_variance_binom_TRUE, "binom", age_adjusted_variance_binom_FALSE),
      pvalue_ageAdjusted_poisson =
        calc_pvalue_norm(
          estimate,
          estimate_type,
          age_adjusted_variance_poisson_TRUE,
          "poisson",
          age_adjusted_variance_poisson_FALSE,
          age_adjusted_mortality_TRUE,
          age_adjusted_mortality_FALSE
        ),
      pvalue_ageAdjusted_fayfeuer =
        calc_pvalue_norm(
          estimate,
          estimate_type,
          age_adjusted_variance_fayfeuer_TRUE,
          "fayfeuer",
          age_adjusted_variance_fayfeuer_FALSE,
          age_adjusted_mortality_TRUE,
          age_adjusted_mortality_FALSE,
          fayfeuer_weight_max_TRUE,
          fayfeuer_weight_max_FALSE
        )
    ) %>%
    mutate(
      "{ci_lname_nb}" := calc_ci_norm(estimate, "l", estimate_type, zscore, naive_variance_binom_TRUE, naive_variance_binom_FALSE, "binom"),
      "{ci_uname_nb}" := calc_ci_norm(estimate, "u", estimate_type, zscore, naive_variance_binom_TRUE, naive_variance_binom_FALSE, "binom"),
      "{ci_lname_wb}" := calc_ci_norm(estimate, "l", estimate_type, zscore, age_adjusted_variance_binom_TRUE, age_adjusted_variance_binom_FALSE, "binom"),
      "{ci_uname_wb}" := calc_ci_norm(estimate, "u", estimate_type, zscore, age_adjusted_variance_binom_TRUE, age_adjusted_variance_binom_FALSE, "binom"),
      "{ci_lname_wp}" :=
        calc_ci_norm(
          estimate,
          "l",
          estimate_type,
          zscore,
          age_adjusted_variance_poisson_TRUE,
          age_adjusted_variance_poisson_FALSE,
          "poisson",
          age_adjusted_mortality_TRUE,
          age_adjusted_mortality_FALSE
        ),
      "{ci_uname_wp}" :=
        calc_ci_norm(
          estimate,
          "u",
          estimate_type,
          zscore,
          age_adjusted_variance_poisson_TRUE,
          age_adjusted_variance_poisson_FALSE,
          "poisson",
          age_adjusted_mortality_TRUE,
          age_adjusted_mortality_FALSE
        ),
      "{ci_lname_ff}" :=
        calc_ci_norm(
          estimate,
          "l",
          estimate_type,
          zscore,
          age_adjusted_variance_fayfeuer_TRUE,
          age_adjusted_variance_fayfeuer_FALSE,
          "fayfeuer",
          age_adjusted_mortality_TRUE,
          age_adjusted_mortality_FALSE,
          fayfeuer_weight_max_TRUE,
          fayfeuer_weight_max_FALSE,
          alpha_level
        ),
      "{ci_uname_ff}" :=
        calc_ci_norm(
          estimate,
          "u",
          estimate_type,
          zscore,
          age_adjusted_variance_fayfeuer_TRUE,
          age_adjusted_variance_fayfeuer_FALSE,
          "fayfeuer",
          age_adjusted_mortality_TRUE,
          age_adjusted_mortality_FALSE,
          fayfeuer_weight_max_TRUE,
          fayfeuer_weight_max_FALSE,
          alpha_level
        )
    ) %>%
    select(-matches("variance|weight"))
}

wide_all_cause_cj <- create_df_stat_test(all_cause_cj, ci = 0.95)
wide_all_cause_race <- create_df_stat_test(all_cause_race, ci = 0.95, "race")
wide_all_cause_sex <- create_df_stat_test(all_cause_sex, ci = 0.95, "sex")
wide_all_cause_race_sex <- create_df_stat_test(all_cause_race_sex, ci = 0.95, "race", "sex")

# Save tables.
create_ci_table <- function(df) {
  df %>%
    select(-matches("naive")) %>%
    mutate(across(where(is.numeric), function(col) {round(col, 4)}))
}

ci_tables <-
  map(
    list(
      "ci_all_cause_cj" = wide_all_cause_cj,
      "ci_all_cause_race" = wide_all_cause_race,
      "ci_all_cause_sex" = wide_all_cause_sex,
      "ci_all_cause_race_sex" = wide_all_cause_race_sex
    ),
    create_ci_table
  )

pwalk(
  list(ci_tables, names(ci_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = table_dir
)

################################################################################
# Graph results of statistical tests.
################################################################################
turn_cis_into_long <- function(df) {
  df %>%
    pivot_longer(
      cols = matches("ci"),
      names_to = "ci_type",
      values_to = "ci"
    ) %>%
    separate_wider_delim(ci_type, delim = "_", names = c("ci_lvl", "l_u", "ci_type", "distr")) %>%
    pivot_wider(names_from = "l_u", values_from = "ci") %>%
    # Need to do this so the bar does not plot the value multiple times.
    mutate(estimate = if_else(ci_type != "naive" | distr != "binom", 0, estimate))
}

# All-cause CJ
g_diff_all_cause_cj <-
  wide_all_cause_cj %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  mutate(comparison = "CJ vs. non-CJ individuals") %>%
  ggplot(aes(x = comparison, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "", y = "Risk difference in mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_cj.png"),
  g_diff_all_cause_cj,
  height = 8,
  width = 8
)

g_ratio_all_cause_cj <-
  wide_all_cause_cj %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  mutate(comparison = "CJ vs. non-CJ individuals") %>%
  ggplot(aes(x = comparison, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "", y = "Risk ratio of mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_cj.png"),
  g_ratio_all_cause_cj,
  height = 8,
  width = 8
)

# All-cause race
g_diff_all_cause_race <-
  wide_all_cause_race %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = race, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "Race", y = "Risk difference in mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_race.png"),
  g_diff_all_cause_race,
  height = 8,
  width = 8
)

g_ratio_all_cause_race <-
  wide_all_cause_race %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = race, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "Race", y = "Risk ratio of mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_race.png"),
  g_ratio_all_cause_race,
  height = 8,
  width = 8
)

# All-cause sex
g_diff_all_cause_sex <-
  wide_all_cause_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = sex, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "Sex", y = "Risk difference in mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_sex.png"),
  g_diff_all_cause_sex,
  height = 8,
  width = 8
)

g_ratio_all_cause_sex <-
  wide_all_cause_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = sex, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "Sex", y = "Risk ratio of mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_sex.png"),
  g_ratio_all_cause_sex,
  height = 8,
  width = 8
)

# All-cause Race x Sex
g_diff_all_cause_race_sex <-
  wide_all_cause_race_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = race, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~sex) +
  labs(x = "Race", y = "Risk difference in mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_race_sex.png"),
  g_diff_all_cause_race_sex,
  height = 9,
  width = 12
)

g_ratio_all_cause_race_sex <-
  wide_all_cause_race_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = race, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~sex) +
  labs(x = "Race", y = "Risk ratio of mortality (Numident)", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_race_sex.png"),
  g_ratio_all_cause_race_sex,
  height = 9,
  width = 12
)

################################################################################
# Create tables displaying statistical significance.
################################################################################
create_sig_table <- function(df) {
  df %>%
    select(matches("race|sex|cause|age_adjusted|n_|estimate|pvalue")) %>%
    mutate(
      estimate = round(estimate, 4),
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
            col <= 0.001 ~ "***",
            is.na(col) ~ ""
          )
        }
      )
    )
}

sig_tables <-
  map(
    list(
      "sig_all_cause_cj" = wide_all_cause_cj,
      "sig_all_cause_race" = wide_all_cause_race,
      "sig_all_cause_sex" = wide_all_cause_sex,
      "sig_all_cause_race_sex" = wide_all_cause_race_sex
    ),
    create_sig_table
  )

pwalk(
  list(sig_tables, names(sig_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = table_dir
)

################################################################################
# Make tables ready for disclosure.
################################################################################
create_disclosure_table <- function(df) {
  df %>%
    filter(estimate_type == "ratio") %>%
    select(-matches("age_adjusted|naive|estimate_type")) %>%
    mutate(
      across(matches("estimate|pvalue|ci95"), function(col) {signif(col, 4)}),
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
      "Ratio" = "estimate"
    )
}

disclosure_tables <-
  map(
    list(
      "21_S2_ci_all_cause_cj" = wide_all_cause_cj,
      "22_S2_ci_all_cause_race" = wide_all_cause_race,
      "23_S2_ci_all_cause_sex" = wide_all_cause_sex,
      "24_S2_ci_all_cause_race_sex" = wide_all_cause_race_sex
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
    filter(estimate_type == "ratio") %>%
    select(
      -matches("age_adjusted|estimate|pvalue|ci95")
    ) %>%
    rename(
      "Non-JII (died)" = "n_died_FALSE",
      "JII (died)" = "n_died_TRUE",
      "Total non-JII" = "n_total_FALSE",
      "Total JII" = "n_total_TRUE",
    )
}

support_tables <-
  map(
    list(
      "21_S2_ci_all_cause_cj" = wide_all_cause_cj,
      "22_SU_S2_ci_all_cause_race" = wide_all_cause_race,
      "23_SU_S2_ci_all_cause_sex" = wide_all_cause_sex,
      "24_SU_S2_ci_all_cause_race_sex" = wide_all_cause_race_sex
    ),
    create_support_table
  )
pwalk(
  list(support_tables, names(support_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = support_dir
)
