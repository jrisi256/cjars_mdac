library(here)
library(purrr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(forcats)

read_path <- here("june_2025_disclosure", "disclosure")
s1_write_path <- here("june_2025_disclosure", "graphs", "sample1")

################################################################################
# Read in sample 1 (MDAC only)
################################################################################
sheets_sample1 <- excel_sheets(file.path(read_path, "sample_1_disclosure_tables.xlsx"))

list_sample1 <-
    sheets_sample1 |>
    set_names() |>
    map(read_xlsx, path = file.path(read_path, "sample_1_disclosure_tables.xlsx"))

################################################################################
# Fig. 1 - All-cause mortality by age
################################################################################
allcause_age <-
    ggplot(list_sample1$`1_S1_ci_all_cause_age`, aes(x = age_bucket, y = Ratio)) +
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "1_allcause_age_s1.png"),
    allcause_age,
    width = 8,
    height = 6
)

################################################################################
# Fig. 2 - All-cause mortality by age and race.
################################################################################
allcause_age_race <-
    ggplot(list_sample1$`2_S1_ci_all_cause_age_race`, aes(x = age_bucket, y = Ratio)) +
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "2_allcause_age_race_s1.png"),
    allcause_age_race,
    width = 13,
    height = 9
)

################################################################################
# Fig. 3 - All-cause mortality by age and sex.
################################################################################
allcause_age_sex <-
    ggplot(list_sample1$`3_S1_ci_all_cause_age_sex`, aes(x = age_bucket, y = Ratio)) +
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "3_allcause_age_sex_s1.png"),
    allcause_age_sex,
    width = 13,
    height = 9
)

################################################################################
# Fig. 4 - Ranking relative causes of death.
################################################################################
cod_ranked <-
    list_sample1$`4_S1_rank_table_cj` |>
    filter(cause_of_death != "All cause") |>
    ggplot(aes(x = fct_reorder(cause_of_death, -rank), y = `Proportion died`)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = `JII status`)) +
    coord_flip() +
    theme_bw() +
    labs(
        x = "Cause of death",
        y = "Proportion who died of a specific cause (among those are who dead)",
        title = "Comparing causes of death for JIIs vs non-JIIs",
        caption = "Results are based on sample 1 (MDAC only, MDAC death date)."
    )

ggsave(
    file.path(s1_write_path, "4_cod_ranked_s1.png"),
    cod_ranked,
    width = 13,
    height = 9
)

################################################################################
# Fig. 5 - Survival curves for JII vs. non-JII
################################################################################
survival <-
    list_sample1$`5_S1_survival_cj` |>
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "5_survival_s1.png"),
    survival,
    width = 10,
    height = 8
)

################################################################################
# Fig. 6 - Survival curves for JII vs. non-JII by race.
################################################################################
survival_race <-
    list_sample1$`6_S1_survival_race` |>
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "6_survival_race_s1.png"),
    survival_race,
    width = 10,
    height = 8
)

################################################################################
# Fig. 7 - Survival curves for JII vs. non-JII by sex.
################################################################################
survival_sex <-
    list_sample1$`7_S1_survival_sex` |>
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "7_survival_sex_s1.png"),
    survival_sex,
    width = 10,
    height = 8
)

################################################################################
# Fig. 14 - Comparing cause-specific mortality rates.
################################################################################
cod <-
    list_sample1$`14_S1_ci_cod_condensed_cj` |>
    ggplot(aes(x = cause_of_death, y = Ratio)) +
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
        x = "Cause of death",
        y = "Ratio of cause-specific age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing mortality rates by cause of death for JIIs vs. non-JIIs",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "14_cod_s1.png"),
    cod,
    width = 10,
    height = 8
)

################################################################################
# Fig. 15 - Comparing cause-specific mortality rates by race.
################################################################################
cod_race <-
    list_sample1$`15_S1_ci_cod_condensed_race` |>
    ggplot(aes(x = cause_of_death, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_fayfeuer,
            ymax = ci95_upper_ageAdjusted_fayfeuer
        ),
        width = 0.2
    ) +
    geom_hline(yintercept = 1) +
    facet_wrap(~race) +
    theme_bw() +
    labs(
        x = "Cause of death",
        y = "Ratio of cause-specific age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing mortality rates by cause of death for JIIs vs. non-JIIs by race",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "15_cod_race_s1.png"),
    cod_race,
    width = 12,
    height = 10
)

################################################################################
# Fig. 16 - Comparing cause-specific mortality rates by sex.
################################################################################
cod_sex <-
    list_sample1$`16_S1_ci_cod_condensed_sex` |>
    ggplot(aes(x = cause_of_death, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_fayfeuer,
            ymax = ci95_upper_ageAdjusted_fayfeuer
        ),
        width = 0.2
    ) +
    geom_hline(yintercept = 1) +
    facet_wrap(~sex) +
    theme_bw() +
    labs(
        x = "Cause of death",
        y = "Ratio of cause-specific age-adjusted mortality rates for JII to non-JII individuals",
        title = "Comparing mortality rates by cause of death for JIIs vs. non-JIIs by sex",
        caption = 
            paste0(
                "Values aboue 1 indicate JIIs had higher mortality.\n",
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "16_cod_sex_s1.png"),
    cod_sex,
    width = 12,
    height = 10
)

################################################################################
# Fig. 17 - All-cause age-adjusted: JIIs vs. non-JIIs.
################################################################################
allcause_ageadjusted <-
    list_sample1$`17_S1_ci_all_cause_cj` |>
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "17_allcause_ageadjusted_s1.png"),
    allcause_ageadjusted,
    width = 8,
    height = 6
)

################################################################################
# Fig. 18 - All-cause age-adjusted: JIIs vs. non-JIIs by race.
################################################################################
allcause_ageadjusted_race <-
    list_sample1$`18_S1_ci_all_cause_race` |>
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "18_allcause_ageadjusted_race_s1.png"),
    allcause_ageadjusted_race,
    width = 10,
    height = 8
)

################################################################################
# Fig. 19 - All-cause age-adjusted: JIIs vs. non-JIIs by sex.
################################################################################
allcause_ageadjusted_sex <-
    list_sample1$`19_S1_ci_all_cause_sex` |>
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
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "19_allcause_ageadjusted_sex_s1.png"),
    allcause_ageadjusted_sex,
    width = 10,
    height = 8
)

################################################################################
# Fig. 20 - All-cause age-adjusted: JIIs vs. non-JIIs by race and sex.
################################################################################
allcause_ageadjusted_race_sex <-
    list_sample1$`20_S1_ci_all_cause_race_sex` |>
    ggplot(aes(x = race, y = Ratio)) +
    geom_bar(stat = "identity") +
    geom_errorbar(
        aes(
            ymin = ci95_lower_ageAdjusted_fayfeuer,
            ymax = ci95_upper_ageAdjusted_fayfeuer
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
                "Error bars are 95% Fay-Feuer CIs weighted by MDAC sampling weights.\n",
                "We treat the overall JII population as the reference population for age-adjustment purposes.\n",
                "Results are based on sample 1 (MDAC only, MDAC death date)."
            )
    )

ggsave(
    file.path(s1_write_path, "20_allcause_ageadjusted_race_sex_s1.png"),
    allcause_ageadjusted_race_sex,
    width = 10,
    height = 8
)
