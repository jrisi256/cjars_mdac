library(dplyr)
library(readr)
library(tidyr)
library(purrr)
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
# Calculate all-cause crude mortality.
################################################################################
calc_effective_base <- function(.df, ...) {
  .df %>%
    group_by(pick(...)) %>%
    summarise(effective_base = sum(mdac_wgt ^ 2) / sum(mdac_wgt) ^ 2)
}

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

calc_crude_mortality <- function(.df, .cod_col, ...) {
  eff_base_df <- calc_effective_base(.df, ...)
  
  if(.cod_col == "none") {
    .df <-
      .df %>%
      group_by(alive, pick(...)) %>%
      summarise(n_event = n(), survey_weight_event = sum(mdac_wgt))
  } else {
    .df <-
      .df %>%
      group_by(.data[[.cod_col]], alive, pick(...)) %>%
      summarise(n_event = n(), survey_weight_event = sum(mdac_wgt))
  }
  
  if(length(list(...)) == 0) {
    .df <- .df %>% mutate(effective_base = eff_base_df$effective_base)
  } else {
    .df <- .df %>% full_join(eff_base_df)
  }
  
  .df <-
    .df %>%
    group_by(pick(...)) %>%
    mutate(
      prop_event = survey_weight_event / sum(survey_weight_event),
      n_total = sum(n_event),
      survey_weight_total = sum(survey_weight_event),
      variance_naive_diff_binom = calc_prop_variance(prop_event, n_total, F, "diff", "binom"),
      variance_weighted_diff_binom = calc_prop_variance(prop_event, effective_base, T, "diff", "binom"),
      variance_naive_ratio_binom = calc_prop_variance(prop_event, n_total, F, "ratio", "binom"),
      variance_weighted_ratio_binom = calc_prop_variance(prop_event, effective_base, T, "ratio", "binom"),
      variance_naive_poisson = calc_prop_variance(n = n_total, weighted = F, distribution = "poisson", deaths = n_event),
      variance_weighted_poisson = calc_prop_variance(n = effective_base, weighted = T, distribution = "poisson", deaths = n_event)
    ) %>%
    ungroup() %>%
    filter(alive == 0) %>%
    select(-alive)
}

all_cause_all <- calc_crude_mortality(sample, "none")
all_cause_cj <- calc_crude_mortality(sample, "none", "cj_contact")
all_cause_race <- calc_crude_mortality(sample, "none", "cj_contact", "race")
all_cause_sex <- calc_crude_mortality(sample, "none", "cj_contact", "sex")
all_cause_age <- calc_crude_mortality(sample, "none", "cj_contact", "age_bucket")
all_cause_age_race <- calc_crude_mortality(sample, "none", "cj_contact", "age_bucket", "race")
all_cause_age_sex <- calc_crude_mortality(sample, "none", "cj_contact", "age_bucket", "sex")
all_cause_age_race_sex <- calc_crude_mortality(sample, "none", "cj_contact", "age_bucket", "race", "sex")

###############################################################################
# All-cause mortality graphs (age, sex, race)
###############################################################################
# Age
all_cause_age_graph <-
  all_cause_age %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  theme_bw() +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "CJ contact"
  )
ggsave(
  file.path(graph_dir, "all_cause_age.png"),
  all_cause_age_graph,
  height = 10,
  width = 14
)

# Age x Race
all_cause_age_race_graph <-
  all_cause_age_race %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  theme_bw() +
  facet_wrap(~race) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "CJ contact"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_race.png"),
  all_cause_age_race_graph,
  height = 7,
  width = 17
)

all_cause_age_race_graph2 <-
  all_cause_age_race %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  theme_bw() +
  facet_wrap(~cj_contact) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "Race/ethnicity"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_race2.png"),
  all_cause_age_race_graph2,
  height = 8,
  width = 16
)

# Age by Sex
all_cause_age_sex_graph <-
  all_cause_age_sex %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  theme_bw() +
  facet_wrap(~sex) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "CJ contact"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_sex.png"),
  all_cause_age_sex_graph,
  height = 8,
  width = 16
)

all_cause_age_sex_graph2 <-
  all_cause_age_sex %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  theme_bw() +
  facet_wrap(~cj_contact) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "Sex"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_sex2.png"),
  all_cause_age_sex_graph2,
  height = 8,
  width = 16
)

# Age x Sex x Race
all_cause_age_race_sex_graph <-
  all_cause_age_race_sex %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = cj_contact)) +
  geom_line(aes(color = cj_contact, group = cj_contact)) +
  theme_bw() +
  facet_wrap(~race+sex, ncol = 2) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "CJ contact"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_race_sex.png"),
  all_cause_age_race_sex_graph,
  height = 10,
  width = 14
)

all_cause_age_race_sex_graph2 <-
  all_cause_age_race_sex %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = race)) +
  geom_line(aes(color = race, group = race)) +
  theme_bw() +
  facet_wrap(~cj_contact+sex, ncol = 2) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "Race/ethnicity"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_race_sex2.png"),
  all_cause_age_race_sex_graph2,
  height = 10,
  width = 14
)

all_cause_age_race_sex_graph3 <-
  all_cause_age_race_sex %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = sex)) +
  geom_line(aes(color = sex, group = sex)) +
  theme_bw() +
  facet_wrap(~cj_contact+race, ncol = 3) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality (Numident)",
    color = "Sex"
  )
ggsave(
  file.path(graph_dir, "all_cause_age_race_sex3.png"),
  all_cause_age_race_sex_graph3,
  height = 10,
  width = 16
)

################################################################################
# Statistical tests on crude mortality based on age buckets.
################################################################################
calc_prop_pooled_variance <- function(p, n1, n2, weighted, type) {
  variance <-
    case_when(
      # n1 and n2 are the effective bases.
      weighted & type == "diff" ~ p * (1 - p) * (n1 + n2),
      !weighted & type == "diff" ~ p * (1 - p) * (1 / n1 + 1 / n2),
      # n1 and n2 are the effective bases.
      weighted & type == "ratio" ~ (1 - p) / p * (n1 + n2),
      !weighted & type == "ratio" ~ (1 - p) / p * (1 / n1 + 1 / n2)
    )
}

calc_pvalue_norm <- function(estimate, type, var1, distr, var2 = 0, p1 = 0, p2 = 0, n1 = 0, n2 = 0) {
  p_value <-
    case_when(
      type == "diff" ~ pnorm(abs(estimate / sqrt(var1 + var2)), lower.tail = F) * 2,
      type == "ratio" & distr == "binom" ~ pnorm(abs(log(estimate) / sqrt(var1 + var2)), lower.tail = F) * 2,
      type == "ratio" & distr == "poisson" ~ pnorm(abs(log(estimate) / sqrt((var1 / p1^2) + (var2 / p2^2))), lower.tail = F) * 2,
      # These results come from Tiwari 2006, but they give nonsense results. I am likely doing something wrong.
      # type == "ratio" & distr == "poisson" ~
      #   pnorm(abs(log(estimate) / (1 / (sqrt(n1 + n2) * sqrt((1 / p1^4) * (var1^2 * p2^2 + var2^2 * p1^2))))), lower.tail = F) * 2
    )
}

calc_ci_norm <- function(estimate, l_u, type, zscore, var1, var2, distr, p1 = 0, p2 = 0) {
  margin_of_error <- zscore * sqrt(var1 + var2)
  moe_p_ratio <- zscore * sqrt((var1 / p1^2) + (var2 / p2^2))
  
  ci <-
    case_when(
      l_u == "l" & type == "diff" ~ estimate - margin_of_error,
      l_u == "u" & type == "diff" ~ estimate + margin_of_error,
      l_u == "l" & type == "ratio" & distr == "binom" ~ exp(log(estimate) - margin_of_error),
      l_u == "u" & type == "ratio" & distr == "binom" ~ exp(log(estimate) + margin_of_error),
      l_u == "l" & type == "ratio" & distr == "poisson" ~ exp(log(estimate) - moe_p_ratio),
      l_u == "u" & type == "ratio" & distr == "poisson" ~ exp(log(estimate) + moe_p_ratio)
    )
}

create_df_stat_test <- function(.df, ci, ...) {
  zscore <- qnorm((1 - ci) / 2 + ci)
  ci_lname_nb <- paste0("ci", ci * 100, "_lower_naive_binom")
  ci_uname_nb <- paste0("ci", ci * 100, "_upper_naive_binom")
  ci_lname_wb <- paste0("ci", ci * 100, "_lower_weighted_binom")
  ci_uname_wb <- paste0("ci", ci * 100, "_upper_weighted_binom")
  ci_lname_np <- paste0("ci", ci * 100, "_lower_naive_poisson")
  ci_uname_np <- paste0("ci", ci * 100, "_upper_naive_poisson")
  ci_lname_wp <- paste0("ci", ci * 100, "_lower_weighted_poisson")
  ci_uname_wp <- paste0("ci", ci * 100, "_upper_weighted_poisson")
  
  .df %>%
    pivot_wider(
      id_cols = c("age_bucket", ...),
      names_from = "cj_contact",
      values_from = c(matches("n_|prop_|variance|survey_weight|effective_base"))
    ) %>%
    mutate(
      diff = prop_event_TRUE - prop_event_FALSE,
      ratio = prop_event_TRUE / prop_event_FALSE,
      p = (survey_weight_event_TRUE + survey_weight_event_FALSE) / (survey_weight_total_TRUE + survey_weight_total_FALSE)
    ) %>%
    pivot_longer(
      cols = c("diff", "ratio"), names_to = "estimate_type", values_to = "estimate"
    ) %>%
    pivot_longer(
      cols = matches("variance"), names_to = "variance_type", values_to = "variance"
    ) %>%
    # Use the same variance for difference and ratio for the the poisson.
    filter(str_detect(variance_type, estimate_type) | str_detect(variance_type, "poisson")) %>%
    mutate(variance_type = str_remove(variance_type, paste0(estimate_type, "_"))) %>%
    pivot_wider(names_from = variance_type, values_from = variance) %>%
    mutate(
      pooled_variance_naive_binom = calc_prop_pooled_variance(p, n_total_TRUE, n_total_FALSE, F, estimate_type),
      pooled_variance_weighted_binom = calc_prop_pooled_variance(p, effective_base_TRUE, effective_base_FALSE, T, estimate_type)
    ) %>%
    mutate(
      pvalue_pooled_naive_binom =
        calc_pvalue_norm(estimate, estimate_type, pooled_variance_naive_binom, "binom"),
      pvalue_nonpooled_naive_binom =
        calc_pvalue_norm(estimate, estimate_type, variance_naive_binom_TRUE, "binom", variance_naive_binom_FALSE),
      pvalue_nonpooled_naive_poisson =
        calc_pvalue_norm(
          estimate,
          estimate_type,
          variance_naive_poisson_TRUE,
          "poisson",
          variance_naive_poisson_FALSE,
          prop_event_TRUE,
          prop_event_FALSE,
          n_total_TRUE,
          n_total_FALSE
        ),
      pvalue_pooled_weighted_binom =
        calc_pvalue_norm(estimate, estimate_type, pooled_variance_weighted_binom, "binom"
        ),
      pvalue_nonpooled_weighted_binom =
        calc_pvalue_norm(estimate, estimate_type, variance_weighted_binom_TRUE, "binom", variance_weighted_binom_FALSE
        ),
      pvalue_nonpooled_weighted_poisson =
        calc_pvalue_norm(
          estimate,
          estimate_type,
          variance_weighted_poisson_TRUE,
          "poisson",
          variance_weighted_poisson_FALSE,
          prop_event_TRUE,
          prop_event_FALSE,
          n_total_TRUE,
          n_total_FALSE
        )
    ) %>%
    mutate(
      "{ci_lname_nb}" := calc_ci_norm(estimate, "l", estimate_type, zscore, variance_naive_binom_TRUE, variance_naive_binom_FALSE, "binom"),
      "{ci_uname_nb}" := calc_ci_norm(estimate, "u", estimate_type, zscore, variance_naive_binom_TRUE, variance_naive_binom_FALSE, "binom"),
      "{ci_lname_wb}" := calc_ci_norm(estimate, "l", estimate_type, zscore, variance_weighted_binom_TRUE, variance_weighted_binom_FALSE, "binom"),
      "{ci_uname_wb}" := calc_ci_norm(estimate, "u", estimate_type, zscore, variance_weighted_binom_TRUE, variance_weighted_binom_FALSE, "binom"),
      "{ci_lname_np}" :=
        calc_ci_norm(
          estimate,
          "l",
          estimate_type,
          zscore,
          variance_naive_poisson_TRUE,
          variance_naive_poisson_FALSE,
          "poisson",
          prop_event_TRUE,
          prop_event_FALSE
        ),
      "{ci_uname_np}" :=
        calc_ci_norm(
          estimate,
          "u",
          estimate_type,
          zscore, variance_naive_poisson_TRUE,
          variance_naive_poisson_FALSE,
          "poisson",
          prop_event_TRUE,
          prop_event_FALSE
        ),
      "{ci_lname_wp}" :=
        calc_ci_norm(
          estimate,
          "l",
          estimate_type,
          zscore,
          variance_weighted_poisson_TRUE,
          variance_weighted_poisson_FALSE,
          "poisson",
          prop_event_TRUE,
          prop_event_FALSE
        ),
      "{ci_uname_wp}" :=
        calc_ci_norm(
          estimate,
          "u",
          estimate_type,
          zscore,
          variance_weighted_poisson_TRUE,
          variance_weighted_poisson_FALSE,
          "poisson",
          prop_event_TRUE,
          prop_event_FALSE
        )
    ) %>%
    select(-matches("variance|effective"), -p)
}

wide_all_cause_age <- create_df_stat_test(all_cause_age, ci = 0.95)
wide_all_cause_age_race <- create_df_stat_test(all_cause_age_race, ci = 0.95, "race")
wide_all_cause_age_sex <- create_df_stat_test(all_cause_age_sex, ci = 0.95, "sex")
wide_all_cause_age_race_sex <- create_df_stat_test(all_cause_age_race_sex, ci = 0.95, "race", "sex")

# Save tables.
create_ci_table <- function(df) {
  df %>%
    select(-matches("naive")) %>%
    mutate(across(where(is.numeric), function(col) {round(col, 4)}))
}

ci_tables <-
  map(
    list(
      "ci_all_cause_age" = wide_all_cause_age,
      "ci_all_cause_age_race" = wide_all_cause_age_race,
      "ci_all_cause_age_sex" = wide_all_cause_age_sex,
      "ci_all_cause_age_race_sex" = wide_all_cause_age_race_sex
    ),
    create_ci_table
  )

pwalk(
  list(ci_tables, names(ci_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = table_dir
)

################################################################################
# Make tables ready for disclosure.
################################################################################
create_disclosure_table <- function(df) {
  df %>%
    filter(estimate_type == "ratio") %>%
    select(-matches("survey_weight|prop_event|pvalue_pooled|estimate_type|naive")) %>%
    mutate(
      across(matches("estimate|pvalue|ci95"), function(col) {signif(col, 4)}),
      across(
        matches("n_event|n_total"),
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
      "Non-JII (died)" = "n_event_FALSE",
      "JII (died)" = "n_event_TRUE",
      "Total non-JII" = "n_total_FALSE",
      "Total JII" = "n_total_TRUE",
      "Ratio" = "estimate"
    )
}

disclosure_tables <-
  map(
    list(
      "8_S2_ci_all_cause_age" = wide_all_cause_age,
      "9_S2_ci_all_cause_age_race" = wide_all_cause_age_race,
      "10_S2_ci_all_cause_age_sex" = wide_all_cause_age_sex
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
      -matches("survey_weight|prop_event|pvalue|estimate|ci95")
    ) %>%
    rename(
      "Non-JII (died)" = "n_event_FALSE",
      "JII (died)" = "n_event_TRUE",
      "Total non-JII" = "n_total_FALSE",
      "Total JII" = "n_total_TRUE",
    )
}

support_tables <-
  map(
    list(
      "8_SU_S2_ci_all_cause_age" = wide_all_cause_age,
      "9_SU_S2_ci_all_cause_age_race" = wide_all_cause_age_race,
      "10_SU_S2_ci_all_cause_age_sex" = wide_all_cause_age_sex
    ),
    create_support_table
  )
pwalk(
  list(support_tables, names(support_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = support_dir
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

# All-cause age
g_all_cause_age_diff <-
  wide_all_cause_age %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(
    x = "Age bucket", y = "Risk difference in mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_age.png"),
  g_all_cause_age_diff,
  height = 8,
  width = 8
)

g_all_cause_age_ratio <-
  wide_all_cause_age %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  labs(
    x = "Age bucket", y = "Risk ratio of mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_age.png"),
  g_all_cause_age_ratio,
  height = 8,
  width = 8
)

# All-cause Age x Race
g_all_cause_age_race_diff <-
  wide_all_cause_age_race %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~race, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk difference in mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_age_race.png"),
  g_all_cause_age_race_diff,
  height = 7,
  width = 16
)

g_all_cause_age_race_ratio <-
  wide_all_cause_age_race %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~race, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk ratio of mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_age_race.png"),
  g_all_cause_age_race_ratio,
  height = 7,
  width = 16
)

# All-cause Age x Sex
g_all_cause_age_sex_diff <-
  wide_all_cause_age_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~sex, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk difference in mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_age_sex.png"),
  g_all_cause_age_sex_diff,
  height = 7,
  width = 16
)

g_all_cause_age_sex_ratio <-
  wide_all_cause_age_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~sex, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk ratio of mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_age_sex.png"),
  g_all_cause_age_sex_ratio,
  height = 7,
  width = 16
)

# All-cause Age x Race x Sex
g_all_cause_age_race_sex_diff <-
  wide_all_cause_age_race_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~race+sex, ncol = 2, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk difference in mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_all_cause_age_race_sex.png"),
  g_all_cause_age_race_sex_diff,
  height = 10,
  width = 12
)

g_all_cause_age_race_sex_ratio <-
  wide_all_cause_age_race_sex %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~race+sex, ncol = 2, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk ratio of mortality (Numident)", color = "95% CI type"
  ) +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_age_race_sex.png"),
  g_all_cause_age_race_sex_ratio,
  height = 10,
  width = 12
)

################################################################################
# Create tables displaying statistical significance.
################################################################################
create_sig_table <- function(df) {
  df %>%
    select(matches("age|race|sex|death|n_event|prop_event|n_total|estimate|pvalue")) %>%
    mutate(estimate = round(estimate, 4)) %>%
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
      "sig_all_cause_age" = wide_all_cause_age,
      "sig_all_cause_age_race" = wide_all_cause_age_race,
      "sig_all_cause_age_sex" = wide_all_cause_age_sex,
      "sig_all_cause_age_race_sex" = wide_all_cause_age_race_sex
    ),
    create_sig_table
  )

pwalk(
  list(sig_tables, names(sig_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = table_dir
)
