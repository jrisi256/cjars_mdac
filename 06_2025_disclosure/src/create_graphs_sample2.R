library(here)
library(purrr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(forcats)

read_path <- here("06_2025_disclosure", "disclosure")
s2_write_path <- here("06_2025_disclosure", "graphs", "sample2")

################################################################################
# Read in sample 2 (Numident + MDAC)
################################################################################
sheets_sample2 <-
  excel_sheets(file.path(read_path, "sample_2_disclosure_tables.xlsx"))

list_sample2 <-
    sheets_sample2 |>
    set_names() |>
    map(
      read_xlsx, path = file.path(read_path, "sample_2_disclosure_tables.xlsx")
    )

################################################################################
# Fig. 8 - All-cause mortality by age
################################################################################
allcause_age <-
    ggplot(list_sample2$`8_S2_ci_all_cause_age`, aes(x = age_bucket, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_weighted_poisson,
            ymax = ci95_upper_weighted_poisson
        ),
        width = 0.5
    ) +
    geom_hline(yintercept = 1) +
    theme_bw() +
    labs(
        x = "Age bucket",
        y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
        title = "Comparison of all-cause mortality rates for JII vs. non-JII individuals by age",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Poisson CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "8_allcause_age_s2.png"),
    allcause_age,
    width = 8,
    height = 6
)

################################################################################
# Fig. 9 - All-cause mortality by age and race.
################################################################################
allcause_age_race <-
    list_sample2$`9_S2_ci_all_cause_age_race` |>
    mutate(
        race =
            case_when(
                race == "black" ~ "Black",
                race == "white" ~ "White",
                race == "hisp" ~ "Hispanic"
            )
    ) |>
    ggplot(aes(x = age_bucket, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_weighted_poisson,
            ymax = ci95_upper_weighted_poisson
        ),
        width = 0.5
    ) +
    geom_hline(yintercept = 1) +
    facet_wrap(~race) +
    theme_bw() +
    labs(
        x = "Age bucket",
        y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
        title = "Comparison of all-cause mortality rates for JII vs. non-JII individuals by age and race",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Poisson CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "9_allcause_age_race_s2.png"),
    allcause_age_race,
    width = 13,
    height = 9
)

################################################################################
# Fig. 10 - All-cause mortality by age and sex.
################################################################################
allcause_age_sex <-
    list_sample2$`10_S2_ci_all_cause_age_sex` |>
    mutate(sex = if_else(sex == "female", "Female", "Male")) |>
    ggplot(aes(x = age_bucket, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_weighted_poisson,
            ymax = ci95_upper_weighted_poisson
        ),
        width = 0.5
    ) +
    geom_hline(yintercept = 1) +
    facet_wrap(~sex) +
    theme_bw() +
    labs(
        x = "Age bucket",
        y = "Ratio of all-cause mortality rates for JII to non-JII individuals",
        title = "Comparison of all-cause mortality rates for JII vs. non-JII individuals by age and sex",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Poisson CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "10_allcause_age_sex_s2.png"),
    allcause_age_sex,
    width = 13,
    height = 9
)

################################################################################
# Fig. 11 - Survival curves for JII vs. non-JII
################################################################################
survival <-
    list_sample2$`11_S2_survival_cj` |>
    pivot_longer(
        cols = matches("Age-adjusted"),
        names_to = "jii_status",
        values_to = "survival_rate"
    ) |>
    ggplot(aes(x = year, y = survival_rate)) +
    geom_point(aes(color = jii_status)) +
    geom_line(aes(group = jii_status, color = jii_status)) +
    theme_bw() +
    labs(
        x = "Year",
        y = "Survival rate",
        title = "Comparing survival rates for JIIs vs. non-JIIs",
        color = "JII status",
        caption = 
            paste0(
                "Survival rates are calculated using age-adjustment and MDAC sampling weights.\n",
                "The difference between JIIs and non-JIIs at each time point is significantly different at the 99.9% level.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "11_survival_s2.png"),
    survival,
    width = 10,
    height = 8
)

################################################################################
# Fig. 12 - Survival curves for JII vs. non-JII by race.
################################################################################
survival_race <-
    list_sample2$`12_S2_survival_race` |>
    mutate(
        race =
            case_when(
                race == "black" ~ "Black",
                race == "white" ~ "White",
                race == "hisp" ~ "Hispanic"
            )
    ) |>
    pivot_longer(
        cols = matches("Age-adjusted"),
        names_to = "jii_status",
        values_to = "survival_rate"
    ) |>
    ggplot(aes(x = year, y = survival_rate)) +
    geom_point(aes(color = jii_status)) +
    geom_line(aes(group = jii_status, color = jii_status)) +
    facet_wrap(~race) +
    theme_bw() +
    labs(
        x = "Year",
        y = "Survival rate",
        title = "Comparing survival rates for JIIs vs. non-JIIs by race",
        color = "JII status",
        caption = 
            paste0(
                "Survival rates are calculated using age-adjustment and MDAC sampling weights.\n",
                "The difference between JIIs and non-JIIs at each time point is significantly different at the 99.9% level.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "12_survival_race_s2.png"),
    survival_race,
    width = 13,
    height = 10
)

################################################################################
# Fig. 13 - Survival curves for JII vs. non-JII by sex.
################################################################################
survival_sex <-
    list_sample2$`13_S2_survival_sex` |>
    mutate(sex = if_else(sex == "female", "Female", "Male")) |>
    pivot_longer(
        cols = matches("Age-adjusted"),
        names_to = "jii_status",
        values_to = "survival_rate"
    ) |>
    ggplot(aes(x = year, y = survival_rate)) +
    geom_point(aes(color = jii_status)) +
    geom_line(aes(group = jii_status, color = jii_status)) +
    facet_wrap(~sex) +
    theme_bw() +
    labs(
        x = "Year",
        y = "Survival rate",
        title = "Comparing survival rates for JIIs vs. non-JIIs by sex",
        color = "JII status",
        caption = 
            paste0(
                "Survival rates are calculated using age-adjustment and MDAC sampling weights.\n",
                "The difference between JIIs and non-JIIs at each time point is significantly different at the 99.9% level.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "13_survival_sex_s2.png"),
    survival_sex,
    width = 10,
    height = 8
)

################################################################################
# Fig. 21 - All-cause age-adjusted: JIIs vs. non-JIIs.
################################################################################
allcause_ageadjusted <-
    list_sample2$`21_S2_ci_all_cause_cj` |>
    ggplot(aes(x = 1, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_fayfeuer,
            ymax = ci95_upper_ageAdjusted_fayfeuer
        ),
        width = 0.2
    ) +
    geom_hline(yintercept = 1) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(
        x = "",
        y = "Ratio of all-cause age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing age-adjusted all-cause mortality rates for JIIs vs. non-JIIs",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "21_allcause_ageadjusted_s2.png"),
    allcause_ageadjusted,
    width = 8,
    height = 6
)

################################################################################
# Fig. 22 - All-cause age-adjusted: JIIs vs. non-JIIs by race.
################################################################################
allcause_ageadjusted_race <-
    list_sample2$`22_S2_ci_all_cause_race` |>
    mutate(
        race =
            case_when(
                race == "black" ~ "Black",
                race == "white" ~ "White",
                race == "hisp" ~ "Hispanic"
            )
    ) |>
    ggplot(aes(x = race, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_fayfeuer,
            ymax = ci95_upper_ageAdjusted_fayfeuer
        ),
        width = 0.2
    ) +
    geom_hline(yintercept = 1) +
    theme_bw() +
    labs(
        x = "Race",
        y = "Ratio of all-cause age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing age-adjusted all-cause mortality rates for JIIs vs. non-JIIs by race",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "22_allcause_ageadjusted_race_s2.png"),
    allcause_ageadjusted_race,
    width = 10,
    height = 8
)

################################################################################
# Fig. 23 - All-cause age-adjusted: JIIs vs. non-JIIs by sex.
################################################################################
allcause_ageadjusted_sex <-
    list_sample2$`23_S2_ci_all_cause_sex` |>
    mutate(sex = if_else(sex == "female", "Female", "Male")) |>
    ggplot(aes(x = sex, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_fayfeuer,
            ymax = ci95_upper_ageAdjusted_fayfeuer
        ),
        width = 0.2
    ) +
    geom_hline(yintercept = 1) +
    theme_bw() +
    labs(
        x = "Race",
        y = "Ratio of all-cause age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing age-adjusted all-cause mortality rates for JIIs vs. non-JIIs by sex",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "23_allcause_ageadjusted_sex_s2.png"),
    allcause_ageadjusted_sex,
    width = 10,
    height = 8
)

################################################################################
# Fig. 24 - All-cause age-adjusted: JIIs vs. non-JIIs by race and sex.
################################################################################
allcause_ageadjusted_race_sex <-
    list_sample2$`24_S2_ci_all_cause_race_sex` |>
    mutate(
        race =
            case_when(
                race == "black" ~ "Black",
                race == "white" ~ "White",
                race == "hisp" ~ "Hispanic"
            )
    ) |>
    mutate(sex = if_else(sex == "female", "Female", "Male")) |>
    ggplot(aes(x = race, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_poisson,
            ymax = ci95_upper_ageAdjusted_poisson
        ),
        width = 0.2
    ) +
    facet_wrap(~sex) +
    geom_hline(yintercept = 1) +
    theme_bw() +
    labs(
        x = "Race",
        y = "Ratio of all-cause age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing age-adjusted all-cause mortality rates for JIIs vs. non-JIIs by race and sex",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Poisson CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 2 (Numident + MDAC, Numident death date)."
            )
    )

ggsave(
    file.path(s2_write_path, "24_allcause_ageadjusted_race_sex_s2.png"),
    allcause_ageadjusted_race_sex,
    width = 10,
    height = 8
)
