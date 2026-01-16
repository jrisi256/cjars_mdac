library(boot)
library(purrr)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

################################################################################
# Read in main sample.
################################################################################
focal_states <- c("FL", "MI", "NC", "TX", "WI")

sample_dir <- ""
table_dir <- ""
main_sample <- read_csv(file.path(sample_dir, "6_main_mortality_sample.csv"))

sample <-
  main_sample %>%
  rename(sex = SEX, cause_of_death = cause113_new_condensed) %>%
  mutate(
    sex = tolower(sex),
    race = 
      case_when(
        race_ethnicity_short == "White alone, not Hispanic" ~ "white",
        race_ethnicity_short == "Hispanic" ~ "hispanic",
        race_ethnicity_short == "Black alone, not Hispanic" ~ "black",
        T ~ race_ethnicity_short
      ),
    cause_of_death =
      case_when(
        cause_of_death == "Assault (homicide)" ~ "homicide",
        cause_of_death == "Intentional self-harm" ~ "suicide",
        cause_of_death == "Drug overdose" ~ "overdose",
        cause_of_death == "Accidents (unintentional injuries)" ~ "accident",
        cause_of_death == "Alive" ~ "alive",
        T ~ "naturalcauses"
      )
  ) %>%
  filter(
    race %in% c("white", "black", "hispanic"),
    age_bucket != "16 and under",
    ST %in% focal_states
  ) %>%
  select(
    cj_pre2015_contact, age_bucket, sex, race, cause_of_death, matchstat, mdac_wgt, pik
  )
rm(main_sample)
gc()

args <- commandArgs(trailingOnly = T)
set.seed(as.numeric(args[1]))

################################################################################
# Generate age weights using the CJ-involved sub-sample as the target pop.
################################################################################
# Generate new age distributions based on each random bootstrapped sample.
generate_age_distribution <- function(df) {
  df %>%
    filter(cj_pre2015_contact == T) %>%
    count(age_bucket, wt = mdac_wgt) %>%
    mutate(age_weight = n / sum(n)) %>%
    select(-n)
}

target_age_distribution <- generate_age_distribution(sample)

################################################################################
# Calculate age-adjusted, sample-weighted mortality for each grouping.
################################################################################
# Calculate age-specific mortality rates for each age group + grouping.
calc_ageSpecific_mortality <- function(.df, .cod_col, ...) {
  if(.cod_col != "none") {
    df <- .df %>% group_by(matchstat, age_bucket, .data[[.cod_col]], pick(...))
  } else {
    df <- .df %>% group_by(matchstat, age_bucket, pick(...))
  }
  
  df <-
    df %>%
    summarise(weighted_n = sum(mdac_wgt)) %>%
    group_by(age_bucket, pick(...)) %>%
    mutate(weighted_prop = weighted_n / sum(weighted_n)) %>%
    ungroup() %>%
    filter(matchstat == 0)
}

# Calculate age-adjusted all-cause mortality rate.
calc_ageAdjusted_mortality <- function(.df, .age_dist, .age_dist_name, .cod_col, ...) {
  df <-
    .df %>%
    left_join(.age_dist, by = "age_bucket") %>%
    mutate(age_specific_mortality = weighted_prop * age_weight)
  
  if(.cod_col != "none") {
    df <- df %>% group_by(.data[[.cod_col]], pick(...))
  } else {
    df <- df %>% group_by(pick(...))
  }
  
  df <-
    df %>%
    summarise(age_adjusted_weighted_mortality = sum(age_specific_mortality)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = cj_pre2015_contact,
      values_from = age_adjusted_weighted_mortality,
      values_fill = 0
    ) %>%
    mutate(diff = `TRUE` - `FALSE`, ratio = `TRUE` / `FALSE`) %>%
    pivot_longer(
      !matches("race|sex|cause"), names_to = c("estimate"), values_to = "value"
    ) %>%
    unite(variable, where(is.character)) %>%
    mutate(variable = paste0(.age_dist_name, "_", variable))
}

# Generate estimates of age-adjusted all-cause mortality for each group type.
create_estimates <- function(.df, .indices, .target_age_dist) {
  .df <- .df %>% slice(.indices)
  
  # Find age distribution of the sample (will be different if using a random bootstrap sample).
  random_age_dist <- generate_age_distribution(.df)
  
  # Calculate age-specific mortality rates for each grouping variable.
  age_specific_cj <- calc_ageSpecific_mortality(.df, "none", "cj_pre2015_contact")
  age_specific_cj_race <- calc_ageSpecific_mortality(.df, "none", "cj_pre2015_contact", "race")
  age_specific_cj_sex <- calc_ageSpecific_mortality(.df, "none", "cj_pre2015_contact", "sex")
  age_specific_cj_race_sex <- calc_ageSpecific_mortality(.df, "none", "cj_pre2015_contact", "race", "sex")
  age_specific_cod_cj <- calc_ageSpecific_mortality(.df, "cause_of_death", "cj_pre2015_contact")
  age_specific_cod_cj_race <- calc_ageSpecific_mortality(.df, "cause_of_death", "cj_pre2015_contact", "race")
  age_specific_cod_cj_sex <- calc_ageSpecific_mortality(.df, "cause_of_death", "cj_pre2015_contact", "sex")
  
  # Calculate age-adjusted mortality using age distribution from our sample.
  age_adjusted_cj <- calc_ageAdjusted_mortality(age_specific_cj, .target_age_dist, "original", "none", "cj_pre2015_contact")
  age_adjusted_cj_race <- calc_ageAdjusted_mortality(age_specific_cj_race, .target_age_dist, "original", "none", "cj_pre2015_contact", "race")
  age_adjusted_cj_sex <- calc_ageAdjusted_mortality(age_specific_cj_sex, .target_age_dist, "original", "none", "cj_pre2015_contact", "sex")
  age_adjusted_cj_race_sex <- calc_ageAdjusted_mortality(age_specific_cj_race_sex, .target_age_dist, "original", "none", "cj_pre2015_contact", "race", "sex")
  age_adjusted_cod_cj <- calc_ageAdjusted_mortality(age_specific_cod_cj, .target_age_dist, "original", "cause_of_death", "cj_pre2015_contact")
  age_adjusted_cod_cj_race <- calc_ageAdjusted_mortality(age_specific_cod_cj_race, .target_age_dist, "original", "cause_of_death", "cj_pre2015_contact", "race")
  age_adjusted_cod_cj_sex <- calc_ageAdjusted_mortality(age_specific_cod_cj_sex, .target_age_dist, "original", "cause_of_death", "cj_pre2015_contact", "sex")
  
  # Calculate age-adjusted mortality using boot-strapped age distribution.
  age_adjusted_boot_cj <- calc_ageAdjusted_mortality(age_specific_cj, random_age_dist, "bootstrap", "none", "cj_pre2015_contact")
  age_adjusted_boot_cj_race <- calc_ageAdjusted_mortality(age_specific_cj_race, random_age_dist, "bootstrap", "none", "cj_pre2015_contact", "race")
  age_adjusted_boot_cj_sex <- calc_ageAdjusted_mortality(age_specific_cj_sex, random_age_dist, "bootstrap", "none", "cj_pre2015_contact", "sex")
  age_adjusted_boot_cj_race_sex <- calc_ageAdjusted_mortality(age_specific_cj_race_sex, random_age_dist, "bootstrap", "none", "cj_pre2015_contact", "race", "sex")
  age_adjusted_boot_cod_cj <- calc_ageAdjusted_mortality(age_specific_cod_cj, random_age_dist, "bootstrap", "cause_of_death", "cj_pre2015_contact")
  age_adjusted_boot_cod_cj_race <- calc_ageAdjusted_mortality(age_specific_cod_cj_race, random_age_dist, "bootstrap", "cause_of_death", "cj_pre2015_contact", "race")
  age_adjusted_boot_cod_cj_sex <- calc_ageAdjusted_mortality(age_specific_cod_cj_sex, random_age_dist, "bootstrap", "cause_of_death", "cj_pre2015_contact", "sex")
  
  all_estimates <-
    bind_rows(
      age_adjusted_cj, age_adjusted_cj_race, age_adjusted_cj_sex, age_adjusted_cj_race_sex,
      age_adjusted_cod_cj, age_adjusted_cod_cj_race, age_adjusted_cod_cj_sex,
      age_adjusted_boot_cj, age_adjusted_boot_cj_race, age_adjusted_boot_cj_sex, age_adjusted_boot_cj_race_sex,
      age_adjusted_boot_cod_cj, age_adjusted_boot_cod_cj_race, age_adjusted_boot_cod_cj_sex,
    )
  
  return_vals <- all_estimates$value
  names(return_vals) <- all_estimates$variable
  remove(.df)
  gc()
  return(return_vals)
}

################################################################################
# Calculate risk differences and risk ratios for mortality using bootstrap samples.
################################################################################
bootstrap_samples <-
  boot(
    data = sample,
    statistic = create_estimates,
    R = as.numeric(args[2]),
    parallel = "multicore",
    ncpus = as.numeric(args[3]),
    .target_age_dist = target_age_distribution
  )
colnames(bootstrap_samples$t) <- names(bootstrap_samples$t0)

################################################################################
# Create variable names for all the differences/ratios being calculated.
################################################################################
args_allcause_cj <-
  expand_grid(
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio")
  ) %>%
  transmute(variable = paste0(age_distribution, "_", estimate))

args_allcause_cj_race <-
  expand_grid(
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio"),
    race = c("white", "black", "hispanic")
  ) %>%
  transmute(variable = paste0(age_distribution, "_", race, "_", estimate))

args_allcause_cj_sex <-
  expand_grid(
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio"),
    sex = c("male", "female")
  ) %>%
  transmute(variable = paste0(age_distribution, "_", sex, "_", estimate))

args_allcause_cj_race_sex <-
  expand_grid(
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio"),
    sex = c("male", "female"),
    race = c("white", "black", "hispanic")
  ) %>%
  transmute(
    variable = paste0(age_distribution, "_", race, "_", sex, "_", estimate)
  )

args_cod_cj <-
  expand_grid(
    cod = c("homicide", "suicide", "naturalcauses", "accident", "overdose"),
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio")
  ) %>%
  transmute(variable = paste0(age_distribution, "_", cod,  "_", estimate))

args_cod_cj_race <-
  expand_grid(
    cod = c("homicide", "suicide", "naturalcauses", "accident", "overdose"),
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio"),
    race = c("white", "black", "hispanic")
  ) %>%
  transmute(
    variable = paste0(age_distribution, "_", cod, "_", race, "_", estimate)
  )

args_cod_cj_sex <-
  expand_grid(
    cod = c("homicide", "suicide", "naturalcauses", "accident", "overdose"),
    age_distribution = c("original", "bootstrap"),
    estimate = c("diff", "ratio"),
    sex = c("male", "female")
  ) %>%
  transmute(
    variable = paste0(age_distribution, "_", cod, "_",  sex, "_", estimate)
  )

all_args <-
  bind_rows(
    args_allcause_cj, args_allcause_cj_race, args_allcause_cj_sex, args_allcause_cj_race_sex,
    args_cod_cj, args_cod_cj_race, args_cod_cj_sex
  )

################################################################################
# Calculate bootstrap confidence intervals.
################################################################################
bootstrap_cis <-
  map(
    all_args$variable,
    function(boot_samples, column) {
      CIs <-
        boot.ci(
          boot_samples,
          conf = c(0.9, 0.95, 0.99, 0.999),
          index = column,
          type = c("norm","basic", "perc")
        )
      
      CIs_df <-
        map(
          list("normal", "basic", "percent"),
          function(ci_type, boot_ci, statistic) {
            CIs_type <- boot_ci[[ci_type]]
            
            if(ci_type == "basic" | ci_type == "percent") {
              CIs_type <- CIs_type[, c(1, 4, 5)]
            }
            
            colnames(CIs_type) <- c("conf_level", "lower_ci", "upper_ci")
            
            CIs_type <-
              CIs_type %>%
              as_tibble() %>%
              mutate(ci_type = ci_type, statistic = statistic, n = boot_ci$R)
            
            return(CIs_type)
          },
          boot_ci = CIs,
          statistic = column
        ) %>%
        bind_rows()
      
      return(CIs_df)
    },
    boot_samples = bootstrap_samples
  ) %>%
  bind_rows()

################################################################################
# Pretty up the confidence intervals.
################################################################################
confidence_intervals <-
  bootstrap_cis %>%
  mutate(
    age_dist = str_extract(statistic, "original|bootstrap"),
    race = str_extract(statistic, "white|black|hispanic"),
    sex = str_extract(statistic, "male|female"),
    measure = str_extract(statistic, "ratio|diff"),
    cod = str_extract(statistic, "homicide|suicide|accident|overdose|naturalcauses")
  ) %>%
  mutate(
    race = if_else(is.na(race), "all", race),
    sex = if_else(is.na(sex), "all", sex),
    cod = if_else(is.na(cod), "all", cod)
  ) %>%
  select(-statistic)

significance <-
  confidence_intervals %>%
  mutate(
    significant =
      case_when(
        measure == "diff" & ((lower_ci < 0 & upper_ci < 0) | (lower_ci > 0 & upper_ci > 0)) ~ T,
        measure == "ratio" & ((lower_ci < 1 & upper_ci < 1) | (lower_ci > 1 & upper_ci > 1)) ~ T,
        T ~ F
      )
  ) %>%
  filter(significant) %>%
  select(-significant) %>%
  group_by(ci_type, age_dist, race, sex, measure, cod) %>%
  summarise(significance = max(conf_level)) %>%
  ungroup() %>%
  full_join(confidence_intervals) %>%
  mutate(
    significance = if_else(is.na(significance), "null", as.character(significance))
  ) %>%
  mutate(
    significance =
      case_when(
        significance == "0.999" ~ "***",
        significance == "0.99" ~ "**",
        significance == "0.95" ~ "*",
        significance == "0.9" ~ "+",
        significance == "null" ~ "Not significant",
      )
  ) %>%
  select(-conf_level, -upper_ci, -lower_ci, -n) %>%
  distinct() %>%
  pivot_wider(
    id_cols = c("race", "sex", "cod", "measure"),
    names_from = c("ci_type", "age_dist"),
    values_from = "significance"
  )

write_csv(
  confidence_intervals,
  file.path(
    table_dir, paste0("bootstrap_cis_age_adjusted_mortality_", args[1], ".csv")
  )
)

write_csv(
  significance,
  file.path(
    table_dir, paste0("bootstrap_sig_age_adjusted_mortality_", args[1], ".csv")
  )
)

################################################################################
# Compare bootstrapped statistical significance to non-bootstrapped.
################################################################################
sig_dir <-
  file.path("/projects", "joe_workspace", "results", "7b_age_adjustment", "tables")

all_cause_cj_sig <- read_csv(file.path(sig_dir, "all_cause_cj_sig.csv"))
all_cause_cj_race_sig <- read_csv(file.path(sig_dir, "all_cause_race_sig.csv"))
all_cause_cj_sex_sig <- read_csv(file.path(sig_dir, "all_cause_sex_sig.csv"))
all_cause_cj_race_sex_sig <- read_csv(file.path(sig_dir, "all_cause_race_sex_sig.csv"))
cod_cj_sig <- read_csv(file.path(sig_dir, "cod_condensed_cj_sig.csv"))
cod_cj_race_sig <- read_csv(file.path(sig_dir, "cod_condensed_race_sig.csv"))
cod_cj_sex_sig <- read_csv(file.path(sig_dir, "cod_condensed_sex_sig.csv"))

boot_sig_vs_nonboot_sig <-
  bind_rows(
    all_cause_cj_sig, all_cause_cj_race_sig, all_cause_cj_sex_sig, all_cause_cj_race_sex_sig,
    cod_cj_sig, cod_cj_race_sig, cod_cj_sex_sig
  ) %>%
  mutate(
    across(matches("race|sex|cause_of_death"), function(col) {tolower(col)})
  ) %>%
  mutate(
    race = if_else(is.na(race), "all", race),
    sex = if_else(is.na(sex), "all", sex),
    cause_of_death = if_else(is.na(cause_of_death), "all", cause_of_death),
    cause_of_death = if_else(cause_of_death == "natural causes", "naturalcauses", cause_of_death),
    cause_of_death = if_else(cause_of_death == "drug overdose", "overdose", cause_of_death),
    across(matches("pvalue"), function(col) {if_else(is.na(col), "", col)})
  ) %>%
  relocate(
    cause_of_death, race, sex, n_died_TRUE, n_died_FALSE, n_total_TRUE, n_total_FALSE,
    age_adjusted_mortality_TRUE, age_adjusted_mortality_FALSE, estimate_type,
    estimate
  ) %>%
  full_join(
    significance,
    by = c("race", "sex", "cause_of_death" = "cod", "estimate_type" = "measure")
  )

write_csv(
  boot_sig_vs_nonboot_sig,
  file.path(
    table_dir, paste0("boot_vs_noboot_", args[1], ".csv")
  )
)
