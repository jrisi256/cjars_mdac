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
  rename(sex = SEX, cause_of_death = cause113_new_condensed) %>%
  select(cj_pre2015_contact, age_bucket, sex, race, cause_of_death, matchstat, mdac_wgt)

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
      group_by(matchstat, pick(...)) %>%
      summarise(n_event = n(), survey_weight_event = sum(mdac_wgt))
  } else {
    .df <-
      .df %>%
      group_by(.data[[.cod_col]], matchstat, pick(...)) %>%
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
    filter(matchstat == 0) %>%
    select(-matchstat)
}

all_cause_all <- calc_crude_mortality(sample, "none")
all_cause_cj <- calc_crude_mortality(sample, "none", "cj_pre2015_contact")
all_cause_race <- calc_crude_mortality(sample, "none", "cj_pre2015_contact", "race")
all_cause_sex <- calc_crude_mortality(sample, "none", "cj_pre2015_contact", "sex")
all_cause_age <- calc_crude_mortality(sample, "none", "cj_pre2015_contact", "age_bucket")
all_cause_age_race <- calc_crude_mortality(sample, "none", "cj_pre2015_contact", "age_bucket", "race")
all_cause_age_sex <- calc_crude_mortality(sample, "none", "cj_pre2015_contact", "age_bucket", "sex")
all_cause_age_race_sex <- calc_crude_mortality(sample, "none", "cj_pre2015_contact", "age_bucket", "race", "sex")
cod_age <- calc_crude_mortality(sample, "cause_of_death", "cj_pre2015_contact", "age_bucket")
cod_condensed_age <-
  sample %>%
  mutate(
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
  calc_crude_mortality("cause_of_death", "cj_pre2015_contact", "age_bucket")

################################################################################
# Create cause of death ranking tables.
################################################################################
create_cod_rank_table <- function(.df, ...) {
  .df <-
    .df %>%
    filter(matchstat == 0) %>%
    group_by(cause_of_death, pick(...)) %>%
    summarise(n_event = n(), survey_weight_event = sum(mdac_wgt)) %>%
    group_by(pick(...)) %>%
    mutate(
      prop_event = survey_weight_event / sum(survey_weight_event),
      n_total = sum(n_event),
      survey_weight_total = sum(survey_weight_event),
      rank = min_rank(-prop_event)
    ) %>%
    ungroup()
}

cod_rank_all <- create_cod_rank_table(sample)
cod_rank_cj <- create_cod_rank_table(sample, "cj_pre2015_contact")
cod_rank_race <- create_cod_rank_table(sample, "cj_pre2015_contact", "race")
cod_rank_sex <- create_cod_rank_table(sample, "cj_pre2015_contact", "sex")
cod_rank_age <- create_cod_rank_table(sample, "cj_pre2015_contact", "age_bucket")

# Save correlation tables.
rank_cj_wide <-
  cod_rank_cj %>%
  select(matches("cj_pre|rank|cause")) %>%
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
write.csv(corr_cj, file.path(table_dir, "corr_rank_cj.csv"))

rank_sex_wide <-
  cod_rank_sex %>%
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
  )

corr_sex <- cor(select(rank_sex_wide, -cause_of_death), method = "spearman")
write.csv(corr_sex, file.path(table_dir, "corr_rank_sex.csv"))

rank_race_wide <-
  cod_rank_race %>%
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
  )

corr_race <- cor(select(rank_race_wide, -cause_of_death), method = "spearman")
write.csv(corr_race, file.path(table_dir, "corr_rank_race.csv"))

################################################################################
# Create cause of death ranking graphs
################################################################################
############################################################## The whole sample.
sort_cod_all <- cod_rank_all %>% arrange(-rank) %>% pull(cause_of_death)

graph_cod_rank_all <-
  cod_rank_all %>%
  filter(rank <= 15) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_all)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death"
  )

ggsave(
  file.path(graph_dir, "cod_rank_all.png"),
  graph_cod_rank_all,
  height = 10,
  width = 14
)

####################################################### Comparing CJ vs. non-CJ.
sort_cod_cj <- cod_rank_cj %>% filter(rank <= 15) %>% pull(cause_of_death) %>% unique()
sort_cod_cj_f <-
  cod_rank_cj %>%
  filter(cause_of_death %in% sort_cod_cj, !cj_pre2015_contact) %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_cj <-
  cod_rank_cj %>%
  filter(cause_of_death %in% sort_cod_cj) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_cj_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for JII vs. non-JII (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "cod_rank_cj.png"),
  graph_cod_rank_cj,
  height = 10,
  width = 14
)

################################################## Comparing men CJ vs. non-CJ.
sort_cod_male <- cod_rank_sex %>% filter(rank <= 15, sex == "Male") %>% pull(cause_of_death) %>% unique()
sort_cod_male_f <-
  cod_rank_sex %>%
  filter(cause_of_death %in% sort_cod_male, !cj_pre2015_contact, sex == "Male") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_male <-
  cod_rank_sex %>%
  filter(cause_of_death %in% sort_cod_male, sex == "Male") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_male_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for JII males vs. non-JII males (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "cod_rank_male.png"),
  graph_cod_rank_male,
  height = 10,
  width = 14
)

################################################## Comparing women CJ vs. non-CJ.
sort_cod_female <- cod_rank_sex %>% filter(rank <= 15, sex == "Female") %>% pull(cause_of_death) %>% unique()
sort_cod_female_f <-
  cod_rank_sex %>%
  filter(cause_of_death %in% sort_cod_female, !cj_pre2015_contact, sex == "Female") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_female <-
  cod_rank_sex %>%
  filter(cause_of_death %in% sort_cod_female, sex == "Female") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_female_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for JII females vs. non-JII females (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "cod_rank_female.png"),
  graph_cod_rank_female,
  height = 10,
  width = 14
)

############################################# Comparing men and women within CJ.
sort_cod_sex_cj <- cod_rank_sex %>% filter(rank <= 15, cj_pre2015_contact) %>% pull(cause_of_death) %>% unique()
sort_cod_male_t <-
  cod_rank_sex %>%
  filter(cause_of_death %in% sort_cod_sex_cj, cj_pre2015_contact, sex == "Male") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_sex_cj <-
  cod_rank_sex %>%
  filter(cause_of_death %in% sort_cod_sex_cj, cj_pre2015_contact) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_male_t)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = sex)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for JII males vs. JII females (ranked by males)",
    fill = "Sex"
  )

ggsave(
  file.path(graph_dir, "cod_rank_sex_cj.png"),
  graph_cod_sex_cj,
  height = 10,
  width = 14
)

################################################## Comparing white CJ vs. non-CJ.
sort_cod_white <- cod_rank_race %>% filter(rank <= 15, race == "White") %>% pull(cause_of_death) %>% unique()
sort_cod_white_f <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_white, !cj_pre2015_contact, race == "White") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_white <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_white, race == "White") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_white_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for White JIIs vs. White non-JIIs (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "cod_rank_white.png"),
  graph_cod_rank_white,
  height = 10,
  width = 14
)

################################################## Comparing Black CJ vs. non-CJ.
sort_cod_black <- cod_rank_race %>% filter(rank <= 15, race == "Black") %>% pull(cause_of_death) %>% unique()
sort_cod_black_f <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_black, !cj_pre2015_contact, race == "Black") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_black <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_black, race == "Black") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_black_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for Black JIIs vs. Black non-JIIs (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "cod_rank_black.png"),
  graph_cod_rank_black,
  height = 10,
  width = 14
)

################################################## Comparing Hispanic CJ vs. non-CJ.
sort_cod_hisp <- cod_rank_race %>% filter(rank <= 15, race == "Hispanic") %>% pull(cause_of_death) %>% unique()
sort_cod_hisp_f <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_hisp, !cj_pre2015_contact, race == "Hispanic") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_rank_hisp <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_hisp, race == "Hispanic") %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_hisp_f)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = cj_pre2015_contact)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for Hispanic JIIs vs. Hispanic non-JIIs (ranked by non-JII)",
    fill = "CJ contact prior to or during 2015?"
  )

ggsave(
  file.path(graph_dir, "cod_rank_hispanic.png"),
  graph_cod_rank_hisp,
  height = 10,
  width = 14
)

############################################# Comparing races within CJ.
sort_cod_race_cj <- cod_rank_race %>% filter(rank <= 15, cj_pre2015_contact) %>% pull(cause_of_death) %>% unique()
sort_cod_white_t <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_race_cj, cj_pre2015_contact, race == "White") %>%
  arrange(-rank) %>%
  pull(cause_of_death)

graph_cod_race_cj <-
  cod_rank_race %>%
  filter(cause_of_death %in% sort_cod_race_cj, cj_pre2015_contact) %>%
  mutate(cause_of_death = factor(x = cause_of_death, levels = sort_cod_white_t)) %>%
  ggplot(aes(x = cause_of_death, y = prop_event)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = race)) +
  theme_bw() +
  coord_flip() +
  labs(
    x = "Cause of death",
    y = "Proportion of deaths due to specific cause",
    title = "Top 15 causes of death for White JIIs vs. Black JIIs vs. Hispanic JIIs (ranked by White JIIs)",
    fill = "Race Ethnicity"
  )

ggsave(
  file.path(graph_dir, "cod_rank_race_cj.png"),
  graph_cod_race_cj,
  height = 10,
  width = 14
)

################################################################################
# Save tables.
################################################################################
combine_cod_allcause <- function(.all_cause, .cod, ...) {
  df <-
    .all_cause %>%
    mutate(cause_of_death = "All cause", rank = 0) %>%
    select(-matches("variance|effective")) %>%
    bind_rows(.cod) %>%
    arrange(pick(...), rank)
}

rank_table_all <- combine_cod_allcause(all_cause_all, cod_rank_all)
rank_table_cj <- combine_cod_allcause(all_cause_cj, cod_rank_cj, "cj_pre2015_contact")
rank_table_race <- combine_cod_allcause(all_cause_race, cod_rank_race, "cj_pre2015_contact", "race")
rank_table_sex <- combine_cod_allcause(all_cause_sex, cod_rank_sex, "cj_pre2015_contact", "sex")
rank_table_age <- combine_cod_allcause(all_cause_age, cod_rank_age, "cj_pre2015_contact", "age_bucket")

args <-
  list(
    "rank_table_all.csv" = rank_table_all,
    "rank_table_cj.csv" = rank_table_cj,
    "rank_table_race.csv" = rank_table_race,
    "rank_table_sex.csv" = rank_table_sex,
    "rank_table_age.csv" = rank_table_age
  )

pwalk(
  list(args, names(args)),
  function(df, file_name, dir) {write_csv(df, file.path(table_dir, file_name))},
  dir = table_dir
)

################################################################################
# Create disclosure tables.
################################################################################
cj_cod_vector <-
  rank_table_cj %>%
  filter(rank <= 20) %>%
  pull(cause_of_death) %>%
  unique()

support_rank_cj <-
  rank_table_cj %>%
  select(cj_pre2015_contact, n_event, n_total, cause_of_death, rank) %>%
  mutate(
    cause_of_death =
      if_else(
        !(cause_of_death %in% cj_cod_vector),
        "All other causes of death",
        cause_of_death
      )
  ) %>%
  group_by(cj_pre2015_contact, cause_of_death) %>%
  summarise(
    n_event = sum(n_event),
    n_total = unique(n_total)
  ) %>%
  group_by(cj_pre2015_contact) %>%
  arrange(cj_pre2015_contact, -n_event) %>%
  rename("JII status" = "cj_pre2015_contact")
write_csv(support_rank_cj, file.path(support_dir, "4_SU_S1_rank_table_cj.csv"))

disclosure_rank_cj <-
  rank_table_cj %>%
  select(cj_pre2015_contact, n_event, n_total, prop_event, cause_of_death, rank) %>%
  filter(cause_of_death %in% cj_cod_vector) %>%
  mutate(
    prop_event = signif(prop_event, 4),
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
  ) %>%
  rename("JII status" = "cj_pre2015_contact", "Proportion died" = "prop_event")
write_csv(disclosure_rank_cj, file.path(disclosure_dir, "4_S1_rank_table_cj.csv"))

###############################################################################
# Causes of death graph - age X-axis
###############################################################################
cod_rank_age_graph <-
  cod_rank_age %>%
  mutate(cause_of_death = str_sub(cause_of_death, 1, 30)) %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = cj_pre2015_contact)) +
  geom_line(aes(color = cj_pre2015_contact, group = cj_pre2015_contact)) +
  facet_wrap(~cause_of_death, scale = "free_y") +
  theme_bw() +
  labs(
    x = "Age bucket",
    y = "Percent who died of specific cause (among those who died)",
    color = "CJ contact prior to/during 2015"
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, vjust = 1, angle = 35)
  )
ggsave(
  file.path(graph_dir, "cod_age.png"),
  cod_rank_age_graph,
  height = 12,
  width = 20
)

###############################################################################
# All-cause mortality graphs (age, sex, race)
###############################################################################
# Age
all_cause_age_graph <-
  all_cause_age %>%
  ggplot(aes(x = age_bucket, y = prop_event)) +
  geom_point(aes(color = cj_pre2015_contact)) +
  geom_line(aes(color = cj_pre2015_contact, group = cj_pre2015_contact)) +
  theme_bw() +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
    color = "CJ contact prior to/during 2015"
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
  geom_point(aes(color = cj_pre2015_contact)) +
  geom_line(aes(color = cj_pre2015_contact, group = cj_pre2015_contact)) +
  theme_bw() +
  facet_wrap(~race) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
    color = "CJ contact prior to/during 2015"
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
  facet_wrap(~cj_pre2015_contact) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
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
  geom_point(aes(color = cj_pre2015_contact)) +
  geom_line(aes(color = cj_pre2015_contact, group = cj_pre2015_contact)) +
  theme_bw() +
  facet_wrap(~sex) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
    color = "CJ contact prior to/during 2015"
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
  facet_wrap(~cj_pre2015_contact) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
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
  geom_point(aes(color = cj_pre2015_contact)) +
  geom_line(aes(color = cj_pre2015_contact, group = cj_pre2015_contact)) +
  theme_bw() +
  facet_wrap(~race+sex, ncol = 2) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
    color = "CJ contact prior to/during 2015"
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
  facet_wrap(~cj_pre2015_contact+sex, ncol = 2) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
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
  facet_wrap(~cj_pre2015_contact+race, ncol = 3) +
  labs(
    x = "Age bucket",
    y = "Crude sample-weighted mortality",
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
      names_from = "cj_pre2015_contact",
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
wide_cod_condensed_age <- create_df_stat_test(cod_condensed_age, ci = 0.95, "cause_of_death")

# Save tables
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
      "ci_all_cause_age_race_sex" = wide_all_cause_age_race_sex,
      "ci_cod_condensed_age" = wide_cod_condensed_age
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
      "1_S1_ci_all_cause_age" = wide_all_cause_age,
      "2_S1_ci_all_cause_age_race" = wide_all_cause_age_race,
      "3_S1_ci_all_cause_age_sex" = wide_all_cause_age_sex
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
      "1_SU_S1_ci_all_cause_age" = wide_all_cause_age,
      "2_SU_S1_ci_all_cause_age_race" = wide_all_cause_age_race,
      "3_SU_S1_ci_all_cause_age_sex" = wide_all_cause_age_sex
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
    x = "Age bucket", y = "Risk difference", color = "95% CI type"
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
    x = "Age bucket", y = "Risk ratio", color = "95% CI type"
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
  facet_wrap(~race) +
  labs(
    x = "Age bucket", y = "Risk difference", color = "95% CI type"
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
  facet_wrap(~race) +
  labs(
    x = "Age bucket", y = "Risk ratio", color = "95% CI type"
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
  facet_wrap(~sex) +
  labs(
    x = "Age bucket", y = "Risk difference", color = "95% CI type"
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
  facet_wrap(~sex) +
  labs(
    x = "Age bucket", y = "Risk ratio", color = "95% CI type"
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
  facet_wrap(~race+sex, ncol = 2) +
  labs(
    x = "Age bucket", y = "Risk difference", color = "95% CI type"
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
    x = "Age bucket", y = "Risk ratio", color = "95% CI type"
  ) +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_all_cause_age_race_sex.png"),
  g_all_cause_age_race_sex_ratio,
  height = 10,
  width = 12
)

# Causes of death by Age
g_cod_cond_age_diff <-
  wide_cod_condensed_age %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "diff") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~cause_of_death, scale = "free_y") +
  labs(
    x = "Age bucket", y = "Risk difference", color = "95% CI type"
  ) +
  geom_hline(yintercept = 0)
ggsave(
  file.path(graph_dir, "risk_diff_cod_condensed_age.png"),
  g_cod_cond_age_diff,
  height = 9,
  width = 17
)

g_cod_cond_age_ratio <-
  wide_cod_condensed_age %>%
  turn_cis_into_long() %>%
  filter(estimate_type == "ratio") %>%
  ggplot(aes(x = age_bucket, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = paste0(ci_type, "_", distr)), width = 0.5) +
  theme_bw() +
  facet_wrap(~cause_of_death) +
  labs(
    x = "Age bucket", y = "Risk ratio", color = "95% CI type"
  ) +
  geom_hline(yintercept = 1)
ggsave(
  file.path(graph_dir, "risk_ratio_cod_condensed_age.png"),
  g_cod_cond_age_ratio,
  height = 9,
  width = 17
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
      "sig_all_cause_age_race_sex" = wide_all_cause_age_race_sex,
      "sig_cod_condensed_age" = wide_cod_condensed_age
    ),
    create_sig_table
  )

pwalk(
  list(sig_tables, names(sig_tables)),
  function(df, file_name, path) {write_csv(df, file.path(path, paste0(file_name, ".csv")))},
  path = table_dir
)
