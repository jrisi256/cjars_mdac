library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(dtplyr)
library(ggplot2)

################################################################################
# Read in data.
################################################################################
focal_states <- c("12", "26", "37", "48", "55")
cjars_dir <- ""
coverage_dir <- ""
save_dir <- ""

adj <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_adjud_rsch.sas7bdat"),
    col_select =
      c(
        "ADJ_FILE_DT_YYYY", "ADJ_DISP_DT_YYYY", "ADJ_SENT_DT_YYYY", 
        "ADJ_GRD_CD", "ADJ_DISP_CD", "ADJ_ST_ORI_FIPS",
        "ADJ_SENT_SERV", "ADJ_SENT_DTH", "ADJ_SENT_INC", "ADJ_SENT_PRO",
        "ADJ_SENT_REST", "ADJ_SENT_SUS", "ADJ_SENT_TRT", "ADJ_SENT_FINE",
        "ADJ_SENT_INC_MIN", "ADJ_SENT_INC_MAX", 
      )
  )

pro <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_probat_rsch.sas7bdat"),
    col_select = c("PRO_BGN_DT_YYYY", "PRO_END_DT_YYYY", "PRO_ST_ORI_FIPS")
  )

inc <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_incar_rsch.sas7bdat"),
    col_select = c("INC_ENTRY_DT_YYYY", "INC_EXIT_DT_YYYY", "INC_ST_ORI_FIPS")
  )

cjars_yearly_coverage <- read_csv(file.path(coverage_dir, "cjars_yearly_coverage.csv"))

################################################################################
# Find years of complete coverage in the CJARS tables for each focal state.
################################################################################
complete_coverage_years <-
  cjars_yearly_coverage %>%
  filter(
    (state %in% c("FL", "MI", "NC", "TX", "WI") & cjars %in% c("inc", "pro") & prcnt_coverage == 100) |
    (state == "FL" & cjars == "adj" & prcnt_coverage_adj_fe >= 96 & prcnt_coverage_adj_mi >= 96) |
    (state == "MI" & cjars == "adj" & prcnt_coverage_adj_fe >= 90 & prcnt_coverage_adj_mi >= 90) |
    (state == "NC" & cjars == "adj" & prcnt_coverage_adj_fe == 100 & prcnt_coverage_adj_mi == 100) |
    (state == "TX" & cjars == "adj" & prcnt_coverage_adj_fe >= 75 & prcnt_coverage_adj_mi >= 75) |
    (state == "WI" & cjars == "adj" & prcnt_coverage_adj_fe >= 98 & prcnt_coverage_adj_mi >= 98)
  ) %>%
  rowwise() %>%
  mutate(
    prcnt_coverage =
      if_else(
        is.na(prcnt_coverage),
        min(prcnt_coverage_adj_fe, prcnt_coverage_adj_mi, na.rm = T),
        prcnt_coverage
      )
  ) %>%
  group_by(state, cjars) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    min_coverage = min(prcnt_coverage, na.rm = T)
  ) %>%
  group_by(state) %>%
  summarise(
    start_year = max(min_year),
    end_year = min(max_year),
    min_coverage = min(min_coverage)
  ) %>%
  ungroup()

################################################################################
# Filter observations to our focal states and time frames of complete coverage.
################################################################################
adj_fltr <-
  adj %>%
  lazy_dt() %>%
  filter(ADJ_ST_ORI_FIPS %in% focal_states) %>%
  mutate(
    state = 
      case_when(
        ADJ_ST_ORI_FIPS == "12" ~ "FL",
        ADJ_ST_ORI_FIPS == "26" ~ "MI",
        ADJ_ST_ORI_FIPS == "37" ~ "NC",
        ADJ_ST_ORI_FIPS == "48" ~ "TX",
        ADJ_ST_ORI_FIPS == "55" ~ "WI"
      )
  ) %>%
  full_join(complete_coverage_years, by = "state") %>%
  mutate(
    year = 
      case_when(
        is.na(ADJ_FILE_DT_YYYY) & is.na(ADJ_DISP_DT_YYYY) & is.na(ADJ_SENT_DT_YYYY) ~ NA_real_,
        is.na(ADJ_FILE_DT_YYYY) & is.na(ADJ_DISP_DT_YYYY) ~ ADJ_SENT_DT_YYYY,
        is.na(ADJ_FILE_DT_YYYY) ~ ADJ_DISP_DT_YYYY,
        !is.na(ADJ_FILE_DT_YYYY) ~ ADJ_FILE_DT_YYYY
      )
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  mutate(
    disposition =
      case_when(
        ADJ_DISP_CD == "DU" ~ "diversion",
        ADJ_DISP_CD %in% c("GC", "GJ", "GP", "GI", "GU") ~ "guilty",
        ADJ_DISP_CD %in% c("NA", "ND", "NI", "NM", "NU", "NP") ~ "not_guilty",
        ADJ_DISP_CD %in% c("PT", "PU") ~ "procedural",
        ADJ_DISP_CD == "UU" ~ "unknown"
      )
  ) %>%
  select(-matches("YYYY|ST_ORI|coverage|_DISP_CD"), -start_year, -end_year) %>%
  as_tibble()

pro_fltr <-
  pro %>%
  lazy_dt() %>%
  filter(PRO_ST_ORI_FIPS %in% focal_states) %>%
  mutate(
    state = 
      case_when(
        PRO_ST_ORI_FIPS == "12" ~ "FL",
        PRO_ST_ORI_FIPS == "26" ~ "MI",
        PRO_ST_ORI_FIPS == "37" ~ "NC",
        PRO_ST_ORI_FIPS == "48" ~ "TX",
        PRO_ST_ORI_FIPS == "55" ~ "WI"
      )
  ) %>%
  full_join(complete_coverage_years, by = "state") %>%
  mutate(
    year = 
      case_when(
        is.na(PRO_BGN_DT_YYYY) & is.na(PRO_END_DT_YYYY) ~ NA_real_,
        is.na(PRO_BGN_DT_YYYY) ~ PRO_END_DT_YYYY,
        !is.na(PRO_BGN_DT_YYYY) ~ PRO_BGN_DT_YYYY
      )
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  select(-matches("YYYY|ST_ORI|coverage"), -start_year, -end_year) %>%
  as_tibble()

inc_fltr <-
  inc %>%
  lazy_dt() %>%
  filter(INC_ST_ORI_FIPS %in% focal_states) %>%
  mutate(
    state = 
      case_when(
        INC_ST_ORI_FIPS == "12" ~ "FL",
        INC_ST_ORI_FIPS == "26" ~ "MI",
        INC_ST_ORI_FIPS == "37" ~ "NC",
        INC_ST_ORI_FIPS == "48" ~ "TX",
        INC_ST_ORI_FIPS == "55" ~ "WI"
      )
  ) %>%
  full_join(complete_coverage_years, by = "state") %>%
  mutate(
    year = 
      case_when(
        is.na(INC_ENTRY_DT_YYYY) & is.na(INC_EXIT_DT_YYYY) ~ NA_real_,
        is.na(INC_ENTRY_DT_YYYY) ~ INC_EXIT_DT_YYYY,
        !is.na(INC_ENTRY_DT_YYYY) ~ INC_ENTRY_DT_YYYY
      )
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  select(-matches("YYYY|ST_ORI|coverage"), -start_year, -end_year) %>%
  as_tibble()

################################################################################
# Investigate guilty adjs. to see if we can gather more info on the punishment.
################################################################################
guilty_adj <-
  adj_fltr %>%
  lazy_dt() %>%
  filter(disposition == "guilty") %>%
  select(-ADJ_GRD_CD) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = matches("ADJ_SENT"),
    names_to = "punishment",
    values_to = "value"
  ) %>%
  group_by(state, year, punishment) %>%
  summarise(
    n = n(),
    nr_missing = sum(is.na(value))
  ) %>%
  ungroup() %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(prcnt_missing = nr_missing / n * 100)

punishment_graphs <-
  map(
    list("FL", "MI", "NC", "TX", "WI"),
    function(focal_state, df) {
      df %>%
        filter(state == focal_state) %>%
        ggplot(aes(x = year, y = prcnt_missing)) +
        geom_point() +
        geom_line() +
        facet_wrap(~punishment) +
        theme_bw() +
        labs(x = "Year", y = "Percent missing", title = focal_state) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
    },
    df = guilty_adj
  )
names(punishment_graphs) <- c("FL", "MI", "NC", "TX", "WI")
pwalk(
  list(punishment_graphs, names(punishment_graphs)),
  function(graph, file_name, dir) {
    ggsave(
      file.path(dir, paste0(file_name, "_punishment_columns.png")),
      graph,
      height = 8,
      width = 12
    )
  },
  dir = save_dir
)

################################################################################
# Get summary counts of the number of events per year per state.
################################################################################
adj_state_summ <- adj_fltr %>% filter(disposition == "guilty") %>% count(state)

adj_guilty_summ <-
  adj_fltr %>%
  filter(disposition == "guilty") %>%
  count(year, state, name = "nr_guilty_adj")

pro_summ <- pro_fltr %>% count(year, state, name = "nr_pro")
inc_summ <- inc_fltr %>% count(year, state, name = "nr_inc")

summ <- 
  reduce(
    list(adj_guilty_summ, pro_summ, inc_summ),
    function(df1, df2) {full_join(df1, df2, by = c("state", "year"))}
  ) %>%
  rowwise() %>%
  mutate(nr_pro_and_inc = sum(nr_pro, nr_inc)) %>%
  pivot_longer(cols = matches("nr"), names_to = "table", values_to = "count")

compare_adj_inc_pro_graph <-
  summ %>%
  filter(table %in% c("nr_guilty_adj", "nr_pro_and_inc")) %>%
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = table)) +
  facet_wrap(~state, scale = "free_x") +
  theme_bw()
ggsave(
  file.path(save_dir, "compare_adj_inc_pro.png"),
  compare_adj_inc_pro_graph,
  height = 8,
  width = 12
)

################################################################################
# Zoom in on Wisconsin because they have pretty complete data.
################################################################################
wi_adj_summ <-
  adj_fltr %>%
  filter(
    disposition == "guilty",
    state == "WI",
    ADJ_SENT_INC != 0 | ADJ_SENT_PRO != 0
  ) %>%
  count(year, state, name = "nr_guilty_punish_adj")

wi_summ <- 
  reduce(
    list(wi_adj_summ, pro_summ, inc_summ),
    function(df1, df2) {inner_join(df1, df2, by = c("state", "year"))}
  ) %>%
  rowwise() %>%
  mutate(nr_pro_and_inc = sum(nr_pro, nr_inc)) %>%
  pivot_longer(cols = matches("nr"), names_to = "table", values_to = "count") %>%
  bind_rows(filter(summ, state == "WI", table == "nr_guilty_adj"))

compare_adj_inc_pro_wi_graph <-
  wi_summ %>%
  filter(table %in% c("nr_guilty_punish_adj", "nr_guilty_adj", "nr_pro_and_inc")) %>%
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = table)) +
  theme_bw() +
  labs(title = "Wisconsin")
ggsave(
  file.path(save_dir, "compare_adj_inc_pro_WI.png"),
  compare_adj_inc_pro_wi_graph,
  height = 8,
  width = 12
)

################################################################################
# Zoom in on North Carolina and explore the data they have.
################################################################################
# The intuition is that perhaps if an individual is missing the incarceration
# and probation data, it is because they did not receive any incarceration or
# probation punishment. Instead, they ONLY received a a fine or restitution.
nc_true_missing <-
  adj_fltr %>%
  filter(
    disposition == "guilty",
    state == "NC",
    is.na(ADJ_SENT_INC_MAX), is.na(ADJ_SENT_INC_MIN), is.na(ADJ_SENT_PRO),
    is.na(ADJ_SENT_FINE), is.na(ADJ_SENT_REST)
  )

# Assume all missing values had no punishment.
prcnt_nc_punish_missing <-
  nrow(nc_true_missing) / adj_state_summ %>% filter(state == "NC") %>% pull(n) * 100

nc_adj_summ <-
  adj_fltr %>%
  filter(
    disposition == "guilty",
    state == "NC",
    ADJ_SENT_INC_MAX != 0 | ADJ_SENT_INC_MIN != 0 | ADJ_SENT_PRO != 0
  ) %>%
  count(year, state, name = "nr_guilty_punish_adj")

nc_summ <- 
  reduce(
    list(nc_adj_summ, pro_summ, inc_summ),
    function(df1, df2) {inner_join(df1, df2, by = c("state", "year"))}
  ) %>%
  rowwise() %>%
  mutate(nr_pro_and_inc = sum(nr_pro, nr_inc)) %>%
  pivot_longer(cols = matches("nr"), names_to = "table", values_to = "count") %>%
  bind_rows(filter(summ, state == "NC", table == "nr_guilty_adj"))

compare_adj_inc_pro_nc_graph <-
  nc_summ %>%
  filter(table %in% c("nr_guilty_punish_adj", "nr_guilty_adj", "nr_pro_and_inc")) %>%
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = table)) +
  theme_bw() +
  labs(title = "North Carolina")
ggsave(
  file.path(save_dir, "compare_adj_inc_pro_NC.png"),
  compare_adj_inc_pro_nc_graph,
  height = 8,
  width = 12
)

################################################################################
# Zoom in on Texas amd explore the data they have.
################################################################################
tx_true_missing <-
  adj_fltr %>%
  filter(
    disposition == "guilty",
    state == "TX",
    is.na(ADJ_SENT_INC), is.na(ADJ_SENT_PRO), is.na(ADJ_SENT_FINE)
  )

# Assume all missing values had no punishment.
prcnt_tx_punish_missing <-
  nrow(tx_true_missing) / adj_state_summ %>% filter(state == "TX") %>% pull(n) * 100

tx_adj_summ <-
  adj_fltr %>%
  filter(
    disposition == "guilty",
    state == "TX",
    ADJ_SENT_INC != 0 | ADJ_SENT_PRO != 0
  ) %>%
  count(year, state, name = "nr_guilty_punish_adj")

tx_summ <- 
  reduce(
    list(tx_adj_summ, pro_summ, inc_summ),
    function(df1, df2) {inner_join(df1, df2, by = c("state", "year"))}
  ) %>%
  rowwise() %>%
  mutate(nr_pro_and_inc = sum(nr_pro, nr_inc)) %>%
  pivot_longer(cols = matches("nr"), names_to = "table", values_to = "count") %>%
  bind_rows(filter(summ, state == "TX", table == "nr_guilty_adj"))

compare_adj_inc_pro_tx_graph <-
  tx_summ %>%
  filter(table %in% c("nr_guilty_punish_adj", "nr_guilty_adj", "nr_pro_and_inc")) %>%
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = table)) +
  theme_bw() +
  labs(title = "Texas")
ggsave(
  file.path(save_dir, "compare_adj_inc_pro_TX.png"),
  compare_adj_inc_pro_tx_graph,
  height = 8,
  width = 12
)
