library(here)
library(purrr)
library(dplyr)
library(readxl)
library(ggplot2)

read_path <- here("december_2025_disclosure", "disclosure")
write_path <- here("december_2025_disclosure", "graphs")

################################################################################
# Read in sample.
################################################################################
sheets <- excel_sheets(file.path(read_path, "S8-9_disclosure_tables.xlsx"))

list <-
  sheets |>
  set_names() |>
  map(read_xlsx, path = file.path(read_path, "S8-9_disclosure_tables.xlsx"))

################################################################################
# Years since first adjudication.
################################################################################
years_since_1st_adj_df <-
  list$`11_S8_years_since_first_adj` |>
  mutate(
    `age of first adjudication` = as.character(`age of first adjudication`)
  )

years_since_1st_adj_graph <-
  ggplot(
    years_since_1st_adj_df,
    aes(x = `years since first adjudication`, y = `survival rate`)
  ) +
  geom_point(aes(color = `age of first adjudication`)) +
  geom_line(
    aes(
      color = `age of first adjudication`,
      group = `age of first adjudication`,
    )
  ) +
  facet_wrap(~`type of adjudication event`) +
  theme_bw() +
  labs(
    x = "Years since first adjudication",
    y = "Survival rate",
    color = "Age of first adjudication",
    caption = 
      paste0(
        "Results are based on CJARS + Numident.\n",
        "'Guilty felony' individuals may have had an earlier guilty non-felony or non-guilty adjudication.\n",
        "'Guilty non-felony only' individuals were never adjudicated for a guilty felony. They may have had an earlier non-guilty adjudication.\n",
        "'Non-guilty only' individuals were never adjudicated for a guilty adjudication."
      )
  )

ggsave(
  file.path(write_path, "years_since_first_adj.png"),
  years_since_1st_adj_graph,
  height = 10,
  width = 16
)

################################################################################
# Years since first adjudication by birth cohort.
################################################################################
years_since_1st_adj_cohort_df <-
  list$`12_S8_years_since_first_adj_by_` |>
  mutate(
    `age of first adjudication` = as.character(`age of first adjudication`)
  )

years_since_1st_adj_cohort_graph <-
  ggplot(
    years_since_1st_adj_cohort_df,
    aes(x = `years since first adjudication`, y = `survival rate`)
  ) +
  geom_point(aes(color = `year range when entering age 17`), size = 2) +
  geom_line(
    aes(
      color = `year range when entering age 17`,
      group = `year range when entering age 17`,
    )
  ) +
  facet_wrap(
    ~`type of adjudication event` + `age of first adjudication`,
    nrow = 3
  ) +
  theme_bw() +
  labs(
    x = "Years since first adjudication",
    y = "Survival rate",
    color = "Cohort (age turned 17)",
    caption = 
      paste0(
        "Results are based on CJARS + Numident.\n",
        "'Guilty felony' individuals may have had an earlier guilty non-felony or non-guilty adjudication.\n",
        "'Guilty non-felony only' individuals were never adjudicated for a guilty felony. They may have had an earlier non-guilty adjudication.\n",
        "'Non-guilty only' individuals were never adjudicated for a guilty adjudication."
      )
  )

ggsave(
  file.path(write_path, "years_since_first_adj_cohort.png"),
  years_since_1st_adj_cohort_graph,
  height = 10,
  width = 16
)

years_since_1st_adj_age_graph <-
  ggplot(
    years_since_1st_adj_cohort_df,
    aes(x = `years since first adjudication`, y = `survival rate`)
  ) +
  geom_point(aes(color = `age of first adjudication`)) +
  geom_line(
    aes(
      color = `age of first adjudication`,
      group = `age of first adjudication`,
    )
  ) +
  facet_wrap(
    ~`type of adjudication event` + `year range when entering age 17`,
    nrow = 3
  ) +
  theme_bw() +
  labs(
    x = "Years since first adjudication",
    y = "Survival rate",
    color = "Age of first adjudication",
    caption = 
      paste0(
        "Results are based on CJARS + Numident.\n",
        "'Guilty felony' individuals may have had an earlier guilty non-felony or non-guilty adjudication.\n",
        "'Guilty non-felony only' individuals were never adjudicated for a guilty felony. They may have had an earlier non-guilty adjudication.\n",
        "'Non-guilty only' individuals were never adjudicated for a guilty adjudication."
      )
  )

ggsave(
  file.path(write_path, "years_since_first_adj_age.png"),
  years_since_1st_adj_age_graph,
  height = 10,
  width = 16
)

################################################################################
# Years since first incarceration.
################################################################################
years_since_1st_inc_df <-
  list$`13_S9_years_since_first_inc` |>
  mutate(
    `age of first incarceration` = as.character(`age of first incarceration`)
  )

years_since_1st_inc_graph <-
  ggplot(
    years_since_1st_inc_df,
    aes(x = `years since first incarceration`, y = `survival rate`)
  ) +
  geom_point(aes(color = `age of first incarceration`)) +
  geom_line(
    aes(
      color = `age of first incarceration`,
      group = `age of first incarceration`,
    )
  ) +
  theme_bw() +
  labs(
    x = "Years since first incarceration",
    y = "Survival rate",
    color = "Age of first incarceration",
    caption = "Results are based on CJARS + Numident."
  )

ggsave(
  file.path(write_path, "years_since_first_inc.png"),
  years_since_1st_inc_graph,
  height = 10,
  width = 16
)
