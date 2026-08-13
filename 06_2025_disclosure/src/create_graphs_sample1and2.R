library(here)
library(purrr)
library(dplyr)
library(readxl)
library(ggplot2)

read_path <- here("06_2025_disclosure", "disclosure")
write_path <- here("06_2025_disclosure", "graphs", "sample1and2")

################################################################################
# Read in samples 1 and 2.
################################################################################
sheets_sample1 <-
  excel_sheets(file.path(read_path, "sample_1_disclosure_tables.xlsx"))

sheets_sample2 <-
  excel_sheets(file.path(read_path, "sample_2_disclosure_tables.xlsx"))

list_sample1 <-
  sheets_sample1 |>
  set_names() |>
  map(read_xlsx, path = file.path(read_path, "sample_1_disclosure_tables.xlsx"))

list_sample2 <-
  sheets_sample2 |>
  set_names() |>
  map(read_xlsx, path = file.path(read_path, "sample_2_disclosure_tables.xlsx"))

################################################################################
# Fig. 1 - All-cause mortality by age
################################################################################
s1_age <-
  list_sample1$`1_S1_ci_all_cause_age` |>
  mutate(Sample = "Sample 1 (8 year follow-up)")

s2_age <-
  list_sample2$`8_S2_ci_all_cause_age`|>
  mutate(Sample = "Sample 2 (16 year follow-up)")

################ Use facets.
allcause_age_facet <-
  bind_rows(s1_age, s2_age) |>
  ggplot(aes(x = age_bucket, y = Ratio)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.5
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~Sample) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up"
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15)
  )

ggsave(
  file.path(write_path, "1a_allcause_age_s1s2_facet.png"),
  allcause_age_facet,
  width = 16,
  height = 9
)

################# Use colors.
allcause_age_group <-
  bind_rows(s1_age, s2_age) |>
  ggplot(aes(x = age_bucket, y = Ratio, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.3,
    position = position_dodge(0.9)
  ) +
  geom_hline(yintercept = 1) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up"
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values =
      c(
        "Sample 1 (8 year follow-up)" = "#BFBFBF",
        "Sample 2 (16 year follow-up)" = "#696969"
      )
  )

ggsave(
  file.path(write_path, "1b_allcause_age_s1s2_group.png"),
  allcause_age_group,
  width = 16,
  height = 9
)

################################################################################
# Fig. 2 - All-cause mortality by age and race.
################################################################################
s1_age_race <-
  list_sample1$`2_S1_ci_all_cause_age_race` |>
  mutate(Sample = "Sample 1 (8 year follow-up)")

s2_age_race <-
  list_sample2$`9_S2_ci_all_cause_age_race`|>
  mutate(Sample = "Sample 2 (16 year follow-up)") |>
  mutate(
    race =
      case_when(
        race == "black" ~ "Black",
        race == "white" ~ "White",
        race == "hisp" ~ "Hispanic"
      )
  )

################ Facet everything.
allcause_age_race_facet <-
  bind_rows(s1_age_race, s2_age_race) |>
  ggplot(aes(x = age_bucket, y = Ratio)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.5
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~race+Sample, ncol = 2) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up"
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15)
  )

ggsave(
  file.path(write_path, "2a_allcause_age_race_s1s2_facet.png"),
  allcause_age_race_facet,
  width = 14,
  height = 13
)

############ Facet race, use colors for sample.
allcause_age_race_GroupSample <-
  bind_rows(s1_age_race, s2_age_race) |>
  ggplot(aes(x = age_bucket, y = Ratio, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.3,
    position = position_dodge(0.9)
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~race, ncol = 1) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up"
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values =
      c(
        "Sample 1 (8 year follow-up)" = "#BFBFBF",
        "Sample 2 (16 year follow-up)" = "#696969"
      )
  )

ggsave(
  file.path(write_path, "2b_allcause_age_race_s1s2_GroupSample.png"),
  allcause_age_race_GroupSample,
  width = 14,
  height = 16
)

############ Facet sample, use colors for race.
allcause_age_race_GroupRace <-
  bind_rows(s1_age_race, s2_age_race) |>
  ggplot(aes(x = age_bucket, y = Ratio, fill = race)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.3,
    position = position_dodge(0.9)
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~Sample) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up",
    fill = "Race/Ethnicity"
  ) +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values =
      c(
        "White" = "#BFBFBF", "Hispanic" = "#8F8F8F", "Black" = "#5C5C5C"
      )
  )

ggsave(
  file.path(write_path, "2c_allcause_age_race_s1s2_GroupRace.png"),
  allcause_age_race_GroupRace,
  width = 16,
  height = 9
)

################################################################################
# Fig. 3 - All-cause mortality by age and sex.
################################################################################
s1_age_sex <-
  list_sample1$`3_S1_ci_all_cause_age_sex` |>
  mutate(Sample = "Sample 1 (8 year follow-up)")

s2_age_sex <-
  list_sample2$`10_S2_ci_all_cause_age_sex` |>
  mutate(Sample = "Sample 2 (16 year follow-up)") |>
  mutate(sex = case_when(sex == "male" ~ "Male", sex == "female" ~ "Female"))

################ Facet everything.
allcause_age_sex_facet <-
  bind_rows(s1_age_sex, s2_age_sex) |>
  ggplot(aes(x = age_bucket, y = Ratio)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.5
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~sex+Sample, ncol = 2) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up"
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15)
  )

ggsave(
  file.path(write_path, "3a_allcause_age_sex_s1s2_facet.png"),
  allcause_age_sex_facet,
  width = 14,
  height = 13
)

############ Facet sex, use colors for sample.
allcause_age_sex_GroupSample <-
  bind_rows(s1_age_sex, s2_age_sex) |>
  ggplot(aes(x = age_bucket, y = Ratio, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.3,
    position = position_dodge(0.9)
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~sex) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up"
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values =
      c(
        "Sample 1 (8 year follow-up)" = "#BFBFBF",
        "Sample 2 (16 year follow-up)" = "#696969"
      )
  )

ggsave(
  file.path(write_path, "3b_allcause_age_sex_s1s2_GroupSample.png"),
  allcause_age_sex_GroupSample,
  width = 18,
  height = 10
)

############ Facet sample, use colors for sex.
allcause_age_sex_GroupSex <-
  bind_rows(s1_age_sex, s2_age_sex) |>
  ggplot(aes(x = age_bucket, y = Ratio, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = ci95_lower_weighted_poisson, ymax = ci95_upper_weighted_poisson),
    width = 0.3,
    position = position_dodge(0.9)
  ) +
  geom_hline(yintercept = 1) +
  facet_wrap(~Sample) +
  theme_bw() +
  labs(
    x = "Age group",
    y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
    title = "8 year follow-up vs. 16 year follow-up",
    fill = "Sex"
  ) +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(values = c("Female" = "#BFBFBF", "Male" = "#5C5C5C"))

ggsave(
  file.path(write_path, "3c_allcause_age_sex_s1s2_GroupSex.png"),
  allcause_age_sex_GroupSex,
  width = 16,
  height = 9
)
