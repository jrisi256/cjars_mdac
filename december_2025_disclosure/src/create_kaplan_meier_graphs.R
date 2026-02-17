library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(stringr)
library(ggplot2)

read_path <- here("december_2025_disclosure", "disclosure")
write_path <- here("december_2025_disclosure", "graphs")

################################################################################
# Read in sample.
################################################################################
sheets <- excel_sheets(file.path(read_path, "S3-7_disclosure_tables.xlsx"))[11:15]

list <-
  sheets |>
  set_names() |>
  map(read_xlsx, path = file.path(read_path, "S3-7_disclosure_tables.xlsx"))

################################################################################
# Clean up the data and get it rady to be graphed.
################################################################################
cleaned_df <-
  pmap(
    list(list, names(list)),
    function(df, state) {
      state <-
        case_when(
          str_detect(state, "_FL_") ~ "Florida",
          str_detect(state, "_MI_") ~ "Michigan",
          str_detect(state, "_NC_") ~ "North Carolina",
          str_detect(state, "_TX_") ~ "Texas",
          str_detect(state, "_WI_") ~ "Wisconsin"
        )

      df <-
        df |>
        pivot_longer(
          -matches("Time"),
          names_to = "column",
          values_to = "value",
          values_transform  = as.character
        ) |>
        select(-`JII Time`) |>
        rename(time = "Non JII Time") |>
        mutate(
          group = if_else(str_detect(column, "Non JII"), "Non-JII", "JII"),
          column = trimws(str_remove(column, "Non JII|JII")),
          value = if_else(value == "D", NA, value),
          value = as.numeric(value),
          time = as.numeric(time)
        ) |>
        pivot_wider(
          id_cols = c("time", "group"),
          names_from = "column",
          values_from = "value"
        ) |>
        mutate(state = state)
    }
  ) |>
  bind_rows()

################################################################################
# All states in one graph.
################################################################################
all_states_graph <-
  ggplot(cleaned_df, aes(x = time, y = `survival rate`)) +
  geom_line(aes(group = group), color = "black") + 
  geom_ribbon(
    aes(
      ymin = `confidence interval L`,
      ymax = `confidence interval H`,
      fill = group
    ),
    alpha = 0.5
  ) +
  facet_wrap(~state) +
  theme_bw() +
  labs(x = "Time", y = "Survival Rate", fill = "JII Group") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(values = c("#7F7F7F", "#171717"))

ggsave(
  file.path(write_path, "kaplan_meier_curves_by_state.png"),
  all_states_graph,
  width = 16,
  height = 9
)

jii_graph <-
  ggplot(cleaned_df, aes(x = time, y = `survival rate`)) +
  geom_line(aes(group = state), color = "black") + 
  geom_ribbon(
    aes(
      ymin = `confidence interval L`,
      ymax = `confidence interval H`,
      fill = state
    ),
    alpha = 0.5
  ) +
  facet_wrap(~group) +
  theme_bw() +
  labs(x = "Time", y = "Survival Rate", fill = "State") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 15)
  )

ggsave(
  file.path(write_path, "kaplan_meier_curves_by_jii.png"),
  jii_graph,
  width = 16,
  height = 9
)

################################################################################
# Using dots instead of lines.
################################################################################
all_states_graph_points <-
  ggplot(cleaned_df, aes(x = time, y = `survival rate`)) +
  geom_point(aes(shape = group), color = "black", size = 2) + 
  geom_ribbon(
    aes(
      ymin = `confidence interval L`,
      ymax = `confidence interval H`,
      fill = group
    ),
    alpha = 0.5
  ) +
  facet_wrap(~state) +
  theme_bw() +
  labs(x = "Time", y = "Survival Rate", fill = "JII Group", shape = "JII Group") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(values = c("#7F7F7F", "#171717"))

ggsave(
  file.path(write_path, "kaplan_meier_curves_by_state_points.png"),
  all_states_graph_points,
  width = 16,
  height = 9
)

################################################################################
# Using line types.
################################################################################
all_states_graph_lty <-
  ggplot(cleaned_df, aes(x = time, y = `survival rate`)) +
  geom_line(aes(lty = group), color = "black", linewidth = 1) + 
  geom_ribbon(
    aes(
      ymin = `confidence interval L`,
      ymax = `confidence interval H`,
      fill = group
    ),
    alpha = 0.5
  ) +
  facet_wrap(~state) +
  theme_bw() +
  labs(x = "Time", y = "Survival Rate", fill = "JII Group", lty = "JII Group") +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 15)
  ) +
  scale_fill_manual(values = c("#7F7F7F", "#171717"))

ggsave(
  file.path(write_path, "kaplan_meier_curves_by_state_lty.png"),
  all_states_graph_lty,
  width = 16,
  height = 9
)
