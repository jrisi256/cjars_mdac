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
# Fig. 4 - Ranking relative causes of death.
################################################################################
cod_ranked_BlackGray <-
  list_sample1$`4_S1_rank_table_cj` |>
  filter(cause_of_death != "All cause") |>
  mutate(`JII status` = if_else(`JII status` == "TRUE", "JII", "Non-JII")) |>
  ggplot(aes(x = fct_reorder(cause_of_death, -rank), y = `Proportion died`)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = `JII status`)) +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Cause of death",
    y = "Proportion who died of a specific cause (among those are who dead)",
    title = "Comparing causes of death for JIIs vs non-JIIs"
  ) +
  scale_fill_manual(values = c("Non-JII" = "#7F7F7F", "JII" = "#171717")) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15),
    title = element_text(size = 15)
  )

ggsave(
  file.path(s1_write_path, "4a_cod_ranked_s1_BlackGray.png"),
  cod_ranked_BlackGray,
  width = 13,
  height = 9
)

cod_ranked_clevelandDotPlot <-
  list_sample1$`4_S1_rank_table_cj` |>
  filter(cause_of_death != "All cause") |>
  pivot_wider(
    id_cols = "cause_of_death",
    names_from = "JII status",
    values_from = "Proportion died"
  ) |>
  rowwise() |>
  mutate(
    mean = mean(c(`FALSE`, `TRUE`)),
    diff = `FALSE` - `TRUE`
  ) |>
  ungroup() |>
  mutate(
    diff_label = case_when(
      diff > 0 ~ "Non-JII higher",
      diff < 0 ~ "JII higher",
      diff == 0 ~ "Same"
    )
  ) |>
  arrange(mean) |>
  mutate(cause_of_death = factor(cause_of_death, cause_of_death)) |>
  ggplot(aes(x = cause_of_death, y = `Proportion died`)) +
  geom_segment(
    aes(
      x = cause_of_death, xend = cause_of_death, y = `FALSE`, yend = `TRUE`,
      linetype = diff_label
    )
  ) +
  geom_point(aes(x = cause_of_death, y = `FALSE`, color = "Non-JII")) +
  geom_point(aes(x = cause_of_death, y = `TRUE`, color = "JII")) +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Cause of death",
    y = "Proportion who died of a specific cause (among those are who dead)",
    title = "Comparing leading causes of death for JIIs vs non-JIIs",
    linetype = "Difference"
  ) +
  scale_color_manual(
    name = "JII status",
    breaks = c("JII", "Non-JII"),
    values = c("Non-JII" = "#BFBFBF", "JII" = "#030303"),
    guide = "legend"
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15),
    title = element_text(size = 15)
  )

ggsave(
  file.path(s1_write_path, "4b_cod_ranked_s1_clevelandDotPlot.png"),
  cod_ranked_clevelandDotPlot,
  width = 13,
  height = 9
)
