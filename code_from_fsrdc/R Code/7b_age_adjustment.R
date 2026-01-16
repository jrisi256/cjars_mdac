library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

################################################################################
# Read in main sample.
################################################################################
focal_states <- c("FL", "MI", "NC", "TX", "WI")

sample_dir <- ""
graph_dir <- ""
table_dir <- ""
disclosure_dir <- ""
support_dir <- ""
main_sample <- read_csv(file.path(sample_dir, "6_main_mortality_sample.csv"))

sample <-
  main_sample %>%
  rename(sex = SEX, cause_of_death = cause113_new_condensed) %>%
  mutate(
    race = 
      case_when(
        race_ethnicity_short == "White alone, not Hispanic" ~ "White",
        race_ethnicity_short == "Hispanic" ~ "Hispanic",
        race_ethnicity_short == "Black alone, not Hispanic" ~ "Black",
        T ~ race_ethnicity_short
      ),
      cause_of_death =
        case_when(
          cause_of_death == "Assault (homicide)" ~ "Homicide",
          cause_of_death == "Intentional self-harm" ~ "Suicide",
          cause_of_death == "Drug overdose" ~ "Drug overdose",
          cause_of_death == "Accidents (unintentional injuries)" ~ "Accident",
          cause_of_death == "Alive" ~ "Alive",
          T ~ "Natural Causes"
        )
  ) %>%
  filter(
    race %in% c("White", "Black", "Hispanic"),
    age_bucket != "16 and under",
    ST %in% focal_states
  ) %>%
  select(cj_pre2015_contact, age_bucket, sex, race, cause_of_death, matchstat, mdac_wgt)

################################################################################
# Generate age weights using the CJ-involved sub-sample as the target pop.
################################################################################
target_age_distribution <-
  sample %>%
  filter(cj_pre2015_contact == T) %>%
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
    df <- .df %>% group_by(matchstat, age_bucket, .data[[.cod_col]], pick(...))
  } else {
    df <- .df %>% group_by(matchstat, age_bucket, pick(...))
  }
    
  df <-
    df %>%
    summarise(n_event = n(), survey_weighted_event = sum(mdac_wgt)) %>%
    group_by(age_bucket, pick(...)) %>%
    mutate(
      weighted_prop = survey_weighted_event / sum(survey_weighted_event)
    ) %>%
    ungroup() %>%
    filter(matchstat != 1) %>%
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
      names_from = cj_pre2015_contact,
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
      names_to = "cj_pre2015_contact",
      values_to = "fayfeuer_weight_max"
    ) %>%
    mutate(cj_pre2015_contact = as.logical(str_extract(cj_pre2015_contact, "TRUE|FALSE")))

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

all_cause_cj <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_pre2015_contact")
all_cause_race <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_pre2015_contact", "race")
all_cause_sex <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_pre2015_contact", "sex")
all_cause_race_sex <- calc_age_adjusted_mortality(sample, target_age_distribution, "none", "cj_pre2015_contact", "race", "sex")
cod_cj <- calc_age_adjusted_mortality(sample, target_age_distribution, "cause_of_death", "cj_pre2015_contact")
cod_race <- calc_age_adjusted_mortality(sample, target_age_distribution, "cause_of_death", "cj_pre2015_contact", "race")
cod_sex <- calc_age_adjusted_mortality(sample, target_age_distribution, "cause_of_death", "cj_pre2015_contact", "sex")

################################################################################
# Graph differences in age-adjusted mortality for CJ vs. non-CJ.
################################################################################
# All-cause CJ contact
g_all_cause_cj <-
  ggplot(all_cause_cj, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point() +
  geom_line(group = 1) +
  theme_bw() +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "all_cause_cj.png"),
  g_all_cause_cj,
  height = 9,
  width = 9
)

# All-cause race
g_all_cause_race <-
  ggplot(all_cause_race, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  theme_bw() +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "all_cause_race.png"),
  g_all_cause_race,
  height = 9,
  width = 9
)

# All-cause sex
g_all_cause_sex <-
  ggplot(all_cause_sex, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  theme_bw() +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "all_cause_sex.png"),
  g_all_cause_sex,
  height = 9,
  width = 9
)

# All-cause Race x Sex
g_all_cause_race_sex <-
  ggplot(all_cause_race_sex, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  facet_wrap(~race) +
  theme_bw() +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "all_cause_race_sex.png"),
  g_all_cause_race_sex,
  height = 9,
  width = 14
)

g_all_cause_race_sex2 <-
  ggplot(all_cause_race_sex, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  facet_wrap(~sex) +
  theme_bw() +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "all_cause_race_sex2.png"),
  g_all_cause_race_sex2,
  height = 9,
  width = 14
)

# Cause of death - CJ contact
g_cod_cj <-
  ggplot(cod_cj, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point() +
  geom_line(aes(group = cause_of_death)) +
  theme_bw() +
  facet_wrap(~cause_of_death) +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "cod_cj.png"),
  g_cod_cj,
  height = 9,
  width = 14
)

# Cause of death - Race
g_cod_race <-
  ggplot(cod_race, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = race)) +
  geom_line(aes(group = race, color = race)) +
  theme_bw() +
  facet_wrap(~cause_of_death, scale = "free_y") +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "cod_race.png"),
  g_cod_race,
  height = 9,
  width = 14
)

# Cause of death - sex
g_cod_sex <-
  ggplot(cod_sex, aes(x = cj_pre2015_contact, y = age_adjusted_mortality)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(group = sex, color = sex)) +
  theme_bw() +
  facet_wrap(~cause_of_death, scale = "free_y") +
  labs(
    x = "Had CJ contact prior to or during 2015?",
    y = "Age-adjusted mortality"
  )
ggsave(
  file.path(graph_dir, "cod_sex.png"),
  g_cod_sex,
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
      names_from = "cj_pre2015_contact",
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
wide_cod_cj <- create_df_stat_test(cod_cj, ci = 0.95, "cause_of_death")
wide_cod_race <- create_df_stat_test(cod_race, ci = 0.95, "race", "cause_of_death")
wide_cod_sex <- create_df_stat_test(cod_sex, ci = 0.95, "sex", "cause_of_death")

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
      "ci_all_cause_race_sex" = wide_all_cause_race_sex,
      "ci_cod_condensed_cj" = wide_cod_cj,
      "ci_cod_condensed_race" = wide_cod_race,
      "ci_cod_condensed_sex" = wide_cod_sex
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
  labs(x = "", y = "Risk difference", color = "95% CI type") +
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
  labs(x = "", y = "Risk ratio", color = "95% CI type") +
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
  labs(x = "Race", y = "Risk difference", color = "95% CI type") +
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
  labs(x = "Race", y = "Risk ratio", color = "95% CI type") +
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
  labs(x = "Sex", y = "Risk difference", color = "95% CI type") +
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
  labs(x = "Sex", y = "Risk ratio", color = "95% CI type") +
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
  labs(x = "Race", y = "Risk difference", color = "95% CI type") +
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
  labs(x = "Race", y = "Risk ratio", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_race_sex.png"),
  g_ratio_all_cause_race_sex,
  height = 9,
  width = 12
)

# Cause of death - CJ contact
g_diff_cod_cj <-
  wide_cod_cj %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = cause_of_death, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "Cause of death", y = "Risk difference", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_cod_cj.png"),
  g_diff_cod_cj,
  height = 8,
  width = 8
)

g_ratio_cod_cj <-
  wide_cod_cj %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = cause_of_death, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(x = "Cause of death", y = "Risk ratio", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_cod_cj.png"),
  g_ratio_cod_cj,
  height = 8,
  width = 8
)

# Cause of death - Race
g_diff_cod_race <-
  wide_cod_race %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = cause_of_death, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~race) +
  labs(x = "Cause of death", y = "Risk difference", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_cod_race.png"),
  g_diff_cod_race,
  height = 8,
  width = 16
)

g_ratio_cod_race <-
  wide_cod_race %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = cause_of_death, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~race) +
  labs(x = "Cause of death", y = "Risk ratio", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_cod_race.png"),
  g_ratio_cod_race,
  height = 8,
  width = 16
)

# Cause of death - Sex
g_diff_cod_sex <-
  wide_cod_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = cause_of_death, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~sex) +
  labs(x = "Cause of death", y = "Risk difference", color = "95% CI type") +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_cod_sex.png"),
  g_diff_cod_sex,
  height = 8,
  width = 16
)

g_ratio_cod_sex <-
  wide_cod_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = cause_of_death, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~sex) +
  labs(x = "Cause of death", y = "Risk ratio", color = "95% CI type") +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_cod_sex.png"),
  g_ratio_cod_sex,
  height = 8,
  width = 16
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
      "sig_all_cause_race_sex" = wide_all_cause_race_sex,
      "sig_cod_condensed_cj" = wide_cod_cj,
      "sig_cod_condensed_race" = wide_cod_race,
      "sig_cod_condensed_sex" = wide_cod_sex
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
      "14_S1_ci_cod_condensed_cj" = wide_cod_cj,
      "15_S1_ci_cod_condensed_race" = wide_cod_race,
      "16_S1_ci_cod_condensed_sex" = wide_cod_sex,
      "17_S1_ci_all_cause_cj" = wide_all_cause_cj,
      "18_S1_ci_all_cause_race" = wide_all_cause_race,
      "19_S1_ci_all_cause_sex" = wide_all_cause_sex,
      "20_S1_ci_all_cause_race_sex" = wide_all_cause_race_sex
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
      "14_SU_S1_ci_cod_condensed_cj" = wide_cod_cj,
      "15_SU_S1_ci_cod_condensed_race" = wide_cod_race,
      "16_SU_S1_ci_cod_condensed_sex" = wide_cod_sex,
      "17_S1_ci_all_cause_cj" = wide_all_cause_cj,
      "18_SU_S1_ci_all_cause_race" = wide_all_cause_race,
      "19_SU_S1_ci_all_cause_sex" = wide_all_cause_sex,
      "20_SU_S1_ci_all_cause_race_sex" = wide_all_cause_race_sex
    ),
    create_support_table
  )
pwalk(
  list(support_tables, names(support_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = support_dir
)

###############################################################################
# Create ranking tables using relative age-adjusted values.
###############################################################################
sample_ranking <-
  main_sample %>%
  rename(sex = SEX, cause_of_death = cause113_new_condensed) %>%
  mutate(
    race = 
      case_when(
        race_ethnicity_short == "White alone, not Hispanic" ~ "White",
        race_ethnicity_short == "Hispanic" ~ "Hispanic",
        race_ethnicity_short == "Black alone, not Hispanic" ~ "Black",
        T ~ race_ethnicity_short
      )
  ) %>%
  filter(
    race %in% c("White", "Black", "Hispanic"),
    age_bucket != "16 and under",
    ST %in% focal_states
  ) %>%
  select(cj_pre2015_contact, age_bucket, sex, race, cause_of_death, matchstat, mdac_wgt)

################################################################ CJ vs. non-CJ.
rank_cj_age_adjusted <-
  sample_ranking %>%
  filter(cause_of_death != "Alive") %>%
  # Within each age bucket, find the relatively most common/least common CoDs.
  group_by(age_bucket, cause_of_death, cj_pre2015_contact) %>%
  summarise(n_event = n(), survey_weight_event = sum(mdac_wgt)) %>%
  group_by(age_bucket, cj_pre2015_contact) %>%
  mutate(prop_event = survey_weight_event / sum(survey_weight_event)) %>%
  ungroup() %>%
  # Then, these relative rankings get age-adjusted.
  full_join(target_age_distribution, by = "age_bucket") %>%
  mutate(age_specific_mortality = prop_event * age_weight) %>%
  # Sum up the relative age-specific mortalities to create an overall age-adjusted relative rate for each cause of death.
  group_by(cause_of_death, cj_pre2015_contact) %>%
  summarise(
    prop_event = sum(age_specific_mortality),
    n_event = sum(n_event),
    survey_weight_event = sum(survey_weight_event)
  ) %>%
  # Then rank the causes of death within the larger group.
  group_by(cj_pre2015_contact) %>%
  mutate(
    rank = min_rank(-prop_event),
    n_total = sum(n_event),
    survey_weight_total = sum(survey_weight_event)
  ) %>%
  ungroup() %>%
  arrange(cj_pre2015_contact, rank)
write_csv(rank_race_age_adjusted, file.path(table_dir, "rank_cod_cj_ageAdjusted.csv"))

####################################################### Comparing CJ vs. non-CJ.
sort_cod_cj <- rank_cj_age_adjusted %>% filter(rank <= 15) %>% pull(cause_of_death) %>% unique()
sort_cod_cj_f <-
  rank_cj_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_cj, !cj_pre2015_contact) %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_cj <-
  rank_cj_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_cj) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_cj_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 causes of death for JII vs. non-JII (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "rank_cod_cj_ageAdjusted.png"),
  graph_cod_rank_cj,
  height = 10,
  width = 14
)

######################################################## Save correlation tables.
rank_cj_wide <-
  rank_cj_age_adjusted %>%
  select(matches("cj_pre|race|cause|rank")) %>%
  filter(rank != 0) %>%
  mutate(group = cj_pre2015_contact) %>%
  select(-cj_pre2015_contact) %>%
  pivot_wider(id_cols = cause_of_death, names_from = group, values_from = rank) %>%
  mutate(
    across(
      where(is.numeric),
      function(col) {if_else(is.na(col), max(col, na.rm = T) + 1, col)}
    )
  )

corr_cj <- cor(select(rank_cj_wide, -cause_of_death), method = "spearman")
write.csv(corr_cj, file.path(table_dir, "corr_rank_cj_age_adjusted.csv"))

########################################################################### Race
rank_race_age_adjusted <-
  sample_ranking %>%
  filter(cause_of_death != "Alive") %>%
  # Within each age bucket, find the relatively most common/least common CoDs.
  group_by(age_bucket, cause_of_death, cj_pre2015_contact, race) %>%
  summarise(n_event = n(), survey_weight_event = sum(mdac_wgt)) %>%
  group_by(age_bucket, cj_pre2015_contact, race) %>%
  mutate(prop_event = survey_weight_event / sum(survey_weight_event)) %>%
  ungroup() %>%
  # Then, these relative rankings get age-adjusted.
  full_join(target_age_distribution, by = "age_bucket") %>%
  mutate(age_specific_mortality = prop_event * age_weight) %>%
  # Sum up the relative age-specific mortalities to create an overall age-adjusted relative rate for each cause of death.
  group_by(cause_of_death, cj_pre2015_contact, race) %>%
  summarise(
    prop_event = sum(age_specific_mortality),
    n_event = sum(n_event),
    survey_weight_event = sum(survey_weight_event)
  ) %>%
  # Then rank the causes of death within the larger group.
  group_by(cj_pre2015_contact, race) %>%
  mutate(
    rank = min_rank(-prop_event),
    n_total = sum(n_event),
    survey_weight_total = sum(survey_weight_event)
  ) %>%
  ungroup() %>%
  arrange(cj_pre2015_contact, race, rank)
write_csv(rank_race_age_adjusted, file.path(table_dir, "rank_cod_race_ageAdjusted.csv"))

################################################## Comparing white CJ vs. non-CJ.
sort_cod_white <- rank_race_age_adjusted %>% filter(rank <= 15, race == "White") %>% pull(cause_of_death) %>% unique()
sort_cod_white_f <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_white, !cj_pre2015_contact, race == "White") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_white <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_white, race == "White") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_white_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 causes of death for White JIIs vs. White non-JIIs (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "rank_cod_white_ageAdjusted.png"),
  graph_cod_rank_white,
  height = 10,
  width = 14
)

################################################## Comparing Black CJ vs. non-CJ.
sort_cod_black <- rank_race_age_adjusted %>% filter(rank <= 15, race == "Black") %>% pull(cause_of_death) %>% unique()
sort_cod_black_f <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_black, !cj_pre2015_contact, race == "Black") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_black <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_black, race == "Black") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_black_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 causes of death for Black JIIs vs. Black non-JIIs (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "rank_cod_black_ageAdjusted.png"),
  graph_cod_rank_black,
  height = 10,
  width = 14
)

############################################## Comparing Hispanic CJ vs. non-CJ.
sort_cod_hisp <- rank_race_age_adjusted %>% filter(rank <= 15, race == "Hispanic") %>% pull(cause_of_death) %>% unique()
sort_cod_hisp_f <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_hisp, !cj_pre2015_contact, race == "Hispanic") %>%
  arrange(-rank) %>%
  pull(cause_of_death)
sort_cod_hisp_f <- c(sort_cod_hisp[!(sort_cod_hisp %in% sort_cod_hisp_f)], sort_cod_hisp_f)

graph_cod_rank_hisp <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_hisp, race == "Hispanic") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_hisp_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 causes of death for Hisp. JIIs vs. Hisp. non-JIIs (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "rank_cod_hispanic_ageAdjusted.png"),
  graph_cod_rank_hisp,
  height = 10,
  width = 14
)

##################################################### Comparing races within CJ.
sort_cod_race_cj <- rank_race_age_adjusted %>% filter(rank <= 15, cj_pre2015_contact) %>% pull(cause_of_death) %>% unique()
sort_cod_white_t <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_race_cj, cj_pre2015_contact, race == "White") %>%
  arrange(-rank) %>%
  pull(cause_of_death)
sort_cod_white_t <- c(sort_cod_race_cj[!(sort_cod_race_cj %in% sort_cod_white_t)], sort_cod_white_t)

graph_cod_race_cj <-
  rank_race_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_race_cj, cj_pre2015_contact) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_white_t)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = race)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 CoDs for White vs. Black vs. Hisp JIIs (ranked by White JIIs)",
    fill = "Race Ethnicity"
  )

ggsave(
  file.path(graph_dir, "rank_cod_race_cj_ageAdjusted.png"),
  graph_cod_race_cj,
  height = 10,
  width = 14
)

#################################################### Correlation table for race.
rank_race_wide <-
  rank_race_age_adjusted %>%
  select(matches("cj_pre|race|rank|cause")) %>%
  filter(rank != 0) %>%
  mutate(group = paste0(race, cj_pre2015_contact)) %>%
  select(-race, -cj_pre2015_contact) %>%
  pivot_wider(id_cols = cause_of_death, names_from = group, values_from = rank) %>%
  mutate(
    across(
      where(is.numeric),
      function(col) {if_else(is.na(col), max(col, na.rm = T) + 1, col)}
    )
  ) %>%
  arrange(WhiteTRUE)

corr_race <- cor(select(rank_race_wide, -cause_of_death), method = "spearman")
write.csv(corr_race, file.path(table_dir, "corr_rank_race_age_adjusted.csv"))

########################################################################### Sex
rank_sex_age_adjusted <-
  sample_ranking %>%
  filter(cause_of_death != "Alive") %>%
  # Within each age bucket, find the relatively most common/least common CoDs.
  group_by(age_bucket, cause_of_death, cj_pre2015_contact, sex) %>%
  summarise(n_event = n(), survey_weight_event = sum(mdac_wgt)) %>%
  group_by(age_bucket, cj_pre2015_contact, sex) %>%
  mutate(prop_event = survey_weight_event / sum(survey_weight_event)) %>%
  ungroup() %>%
  # Then, these relative rankings get age-adjusted.
  full_join(target_age_distribution, by = "age_bucket") %>%
  mutate(age_specific_mortality = prop_event * age_weight) %>%
  # Sum up the relative age-specific mortalities to create an overall age-adjusted relative rate for each cause of death.
  group_by(cause_of_death, cj_pre2015_contact, sex) %>%
  summarise(
    prop_event = sum(age_specific_mortality),
    n_event = sum(n_event),
    survey_weight_event = sum(survey_weight_event)
  ) %>%
  # Then rank the causes of death within the larger group.
  group_by(cj_pre2015_contact, sex) %>%
  mutate(
    rank = min_rank(-prop_event),
    n_total = sum(n_event),
    survey_weight_total = sum(survey_weight_event)
  ) %>%
  ungroup() %>%
  arrange(cj_pre2015_contact, sex, rank)
write_csv(rank_sex_age_adjusted, file.path(table_dir, "rank_cod_sex_ageAdjusted.csv"))

################################################## Comparing men CJ vs. non-CJ.
sort_cod_male <- rank_sex_age_adjusted %>% filter(rank <= 15, sex == "Male") %>% pull(cause_of_death) %>% unique()
sort_cod_male_f <-
  rank_sex_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_male, !cj_pre2015_contact, sex == "Male") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_male <-
  rank_sex_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_male, sex == "Male") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_male_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 causes of death for JII males vs. non-JII males (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "rank_cod_male_ageAdjusted.png"),
  graph_cod_rank_male,
  height = 10,
  width = 14
)

################################################## Comparing women CJ vs. non-CJ.
sort_cod_female <- rank_sex_age_adjusted%>% filter(rank <= 15, sex == "Female") %>% pull(cause_of_death) %>% unique()
sort_cod_female_f <-
  rank_sex_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_female, !cj_pre2015_contact, sex == "Female") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_female <-
  rank_sex_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_female, sex == "Female") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_female_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 CoDs for JII females vs. non-JII females (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "rank_cod_female_ageAdjusted.png"),
  graph_cod_rank_female,
  height = 10,
  width = 14
)

############################################# Comparing men and women within CJ.
sort_cod_sex_cj <- rank_sex_age_adjusted %>% filter(rank <= 15, cj_pre2015_contact) %>% pull(cause_of_death) %>% unique()
sort_cod_male_t <-
  rank_sex_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_sex_cj, cj_pre2015_contact, sex == "Male") %>%
  arrange(-rank) %>%
  pull(cause_of_death)
sort_cod_male_t <- c(sort_cod_sex_cj[!(sort_cod_sex_cj %in% sort_cod_male_t)], sort_cod_male_t)

graph_cod_sex_cj <-
  rank_sex_age_adjusted %>%
  filter(cause_of_death %in% sort_cod_sex_cj, cj_pre2015_contact) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_male_t)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = sex)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause (age-adjusted)",
    title = "Top 15 causes of death for JII males vs. JII females (ranked by males)",
    fill = "Sex"
  )

ggsave(
  file.path(graph_dir, "rank_cod_sex_cj_ageAdjusted.png"),
  graph_cod_sex_cj,
  height = 10,
  width = 14
)

#################################################### Correlation table for sex.
rank_sex_wide <-
  rank_sex_age_adjusted %>%
  select(matches("cj_pre|sex|rank|cause")) %>%
  filter(rank != 0) %>%
  mutate(group = paste0(sex, cj_pre2015_contact)) %>%
  select(-sex, -cj_pre2015_contact) %>%
  pivot_wider(id_cols = cause_of_death, names_from = group, values_from = rank) %>%
  mutate(
    across(
      where(is.numeric),
      function(col) {if_else(is.na(col), max(col, na.rm = T) + 1, col)}
    )
  ) %>%
  arrange(MaleTRUE)

corr_sex <- cor(select(rank_sex_wide, -cause_of_death), method = "spearman")
write.csv(corr_sex, file.path(table_dir, "corr_rank_sex_age_adjusted.csv"))
