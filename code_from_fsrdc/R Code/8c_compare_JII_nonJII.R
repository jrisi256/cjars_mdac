library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)
library(lubridate)

################################################################################
# Read in data.
################################################################################
read_dir <- ""
graph_dir <- ""

mdac_sample <- read_csv(file.path(read_dir, "8_mdac_wide_focal_states.csv"))
graph_df <- mdac_sample %>% mutate(jii = if_else(nr_cjars_ids > 0, T, F))

################################################################################
# Create summary tables and graph results (for categorical variables).
################################################################################
create_summary_table <- function(df, col) {
  if(col %in% c("jwd")) {
    df_new <-
      df %>%
      mutate(
        jwd_string =
          case_when(
            jwd <= 59 ~ paste0("2008-01-01 00:", jwd),
            jwd >= 100 & jwd <= 959 ~ paste0("2008-01-01 ", str_sub(jwd, 1, 1), ":", str_sub(jwd, 2)),
            jwd >= 1000 ~ paste0("2008-01-01 ", str_sub(jwd, 1, 2), ":", str_sub(jwd, 3))
          ),
        jwd = round_date(ymd_hm(jwd_string), "hour")
      )
  } else if(col %in% c("jwmn")) {
    df_new <- df %>% mutate(jwmn = plyr::round_any(jwmn, 15))
  } else if(col %in% "bds") {
    df_new <- df %>% filter(bds < 50)
  } else if(col %in% "workers_in_family") {
    df_new <-
      df %>%
      mutate(
        workers_in_family =
          case_when(
            workers_in_family == "no_workers" ~ "0",
            workers_in_family == "one_worker" ~ "1",
            workers_in_family == "two_workers" ~ "2",
            workers_in_family == "three_or_more_workers" ~ "3+"
          )
      )
  } else {
    df_new <- df
  }
  
  df_new <-
    df_new %>%
    count(pick(all_of(c("jii", col))), wt = mdac_wgt) %>%
    filter(!is.na(.data[[col]])) %>%
    group_by(jii) %>%
    mutate(prcnt = n / sum(n) * 100) %>%
    arrange(jii, prcnt) %>%
    mutate(column = col) %>%
    rename(value = col)
  
  if(!(col %in% c("year_home_built", "wkw", "np", "bds", "jwd", "jwmn", "workers_in_family"))) {
    levels <- df_new %>% filter(!jii) %>% pull(value)
    df_new <- df_new %>% mutate(value := factor(value, levels = levels))
  }
  
  return(df_new)
}

create_graphs_categorical <- function(table) {
  ggplot(table, aes(x = value, y = prcnt)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = jii)) +
    theme_bw() +
    coord_flip() +
    labs(
      y = "Percent",
      x = "Category value",
      fill = "Justice involvement?",
      title = paste0("Comparison of JII and non-JIIs: ", unique(table$column))
    )
}

graphs_categorical_tables <- 
  map(
    list(
      "indg" = "indg", "ur" = "ur", "memi" = "memi", "sex" = "sex",
      "race_short" = "race_short", "hht" = "hht", "paoc" = "paoc", "psf" = "psf",
      "nr" = "nr", "r18" = "r18", "r65" = "r65", "marriage" = "marriage",
      "education" = "education", "citizen" = "citizen", "hhl" = "hhl",
      "english_ability" = "english_ability", "employment" = "employment",
      "cow" = "cow", "occg" = "occg", "wkl" = "wkl", "wkw" = "wkw",
      "health_insurance" = "health_insurance", "medicaid" = "medicaid",
      "disability" = "disability", "ever_in_military" = "ever_in_military",
      "moved_past_year" = "moved_past_year", "workers_in_family" = "workers_in_family",
      "food_stamps" = "food_stamps", "home_ownership" = "home_ownership",
      "type_of_home" = "type_of_home", "year_home_built" = "year_home_built",
      "plumbing" = "plumbing", "kitchen" = "kitchen", "np" = "np", "bds" = "bds",
      "telephone_services" = "telephone_services", "jwd" = "jwd", "jwmn" = "jwmn"
    ),
    create_summary_table,
    df = graph_df
  )

graphs_categorical <- map(graphs_categorical_tables, create_graphs_categorical)
pdf(file.path(graph_dir, "categorical_variables.pdf"))
graphs_categorical
dev.off()

################################################################################
# Create summary tables and graph results (for numerical variables).
################################################################################
create_graphs_numerical <- function(df, col) {
  quantiles <- quantile(df[[col]], seq(0, 1, 0.005), na.rm = T)
  lower_bound <- quantiles["0.5%"]
  upper_bound <- quantiles["99.0%"]
  
  df <- df %>% filter(.data[[col]] > lower_bound & .data[[col]] < upper_bound)
  
  ggplot(df, aes(x = jii, y = .data[[col]])) +
    geom_violin(aes(weight = mdac_wgt), draw_quantiles = c(0.25, 0.5, 0.75), trim = T) +
    theme_bw() + 
    labs(x = "JII Involvement?", y = col) +
    coord_flip()
}

graphs_numerical <-
  map(
    list(
      "mvy" = "mvy", "val" = "val", "finc" = "finc", "grpi" = "grpi",
      "hinc" = "hinc", "pinc" = "pinc", "poverty" = "poverty"
    ),
    create_graphs_numerical,
    df = graph_df
  )

pdf(file.path(graph_dir, "numerical_variables.pdf"))
graphs_numerical
dev.off()

################################################################################
# Special graph
################################################################################
age_at_death <-
  graph_df %>%
  filter(cause113_label != "Alive") %>%
  mutate(
    age =
      if_else(
        is.na(dod),
        time_length(interval(ymd("2024-03-01"), dob), "year"),
        time_length(interval(dob, dod), "year")
      )
  )

graph_age_at_death <- create_graphs_numerical(age_at_death, "age")

pdf(file.path(graph_dir, "age_at_death.pdf"))
graph_age_at_death
dev.off()
