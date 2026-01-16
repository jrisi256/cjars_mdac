library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(dtplyr)
library(ggplot2)
library(stringr)

################################################################################
# Read in data.
################################################################################
focal_states <- c("12", "26", "37", "48", "55")
cjars_dir <- ""
graph_dir <- ""
coverage_dir <- ""

adj <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_adjud_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID",
        "ADJ_OFF_DT_YYYY", "ADJ_OFF_DT_MM", "ADJ_OFF_DT_DD",
        "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD",
        "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD",
        "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD",
        "ADJ_GRD_CD", "ADJ_DISP_CD", "ADJ_ST_ORI_FIPS",
        "ADJ_SENT_SERV", "ADJ_SENT_DTH", "ADJ_SENT_INC", "ADJ_SENT_PRO",
        "ADJ_SENT_REST", "ADJ_SENT_SUS", "ADJ_SENT_TRT", "ADJ_SENT_FINE",
        "ADJ_SENT_INC_MIN", "ADJ_SENT_INC_MAX", 
      )
  )

pro <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_probat_rsch.sas7bdat"),
    col_select =
      c("CJARS_ID", "PRO_BGN_DT_YYYY", "PRO_END_DT_YYYY", "PRO_ST_ORI_FIPS")
  )

inc <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_incar_rsch.sas7bdat"),
    col_select =
      c("CJARS_ID", "INC_ENTRY_DT_YYYY", "INC_EXIT_DT_YYYY", "INC_ST_ORI_FIPS")
  )

cjars_pik_crosswalk <-
  read_csv(
    file.path("/projects", "joe_workspace", "output", "2_joined_cjars_pik.csv"),
    col_select = c("pik", "CJARS_ID")
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
# Keep observations from our focal states and with at least one valid full date.
# They must also match to a PIK.
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
      ),
    disposition =
      case_when(
        ADJ_DISP_CD == "DU" ~ "diversion",
        ADJ_DISP_CD %in% c("GC", "GJ", "GP", "GI", "GU") ~ "guilty",
        ADJ_DISP_CD %in% c("NA", "ND", "NI", "NM", "NU", "NP") ~ "not_guilty",
        ADJ_DISP_CD %in% c("PT", "PU") ~ "procedural",
        ADJ_DISP_CD == "UU" ~ "unknown"
      )
  ) %>%
  select(-ADJ_ST_ORI_FIPS, -ADJ_DISP_CD) %>%
  filter(
    (!is.na(ADJ_OFF_DT_YYYY) & !is.na(ADJ_OFF_DT_MM) & !is.na(ADJ_OFF_DT_DD)) |
    (!is.na(ADJ_FILE_DT_YYYY) & !is.na(ADJ_FILE_DT_MM) & !is.na(ADJ_FILE_DT_DD)) |
    (!is.na(ADJ_DISP_DT_YYYY) & !is.na(ADJ_DISP_DT_MM) & !is.na(ADJ_DISP_DT_DD)) |
    (!is.na(ADJ_SENT_DT_YYYY) & !is.na(ADJ_SENT_DT_MM) & !is.na(ADJ_SENT_DT_DD))
  ) %>%
  inner_join(cjars_pik_crosswalk, by = "CJARS_ID") %>%
  as_tibble()

################################################################################
# Calculate number of guilty events per state per year.
################################################################################
summ_adj <-
  adj_fltr %>%
  full_join(complete_coverage_years, by = "state") %>%
  mutate(
    year = 
      case_when(
        is.na(ADJ_FILE_DT_YYYY) & is.na(ADJ_DISP_DT_YYYY) & is.na(ADJ_SENT_DT_YYYY) & is.na(ADJ_OFF_DT_YYYY) ~ NA_real_,
        is.na(ADJ_FILE_DT_YYYY) & is.na(ADJ_DISP_DT_YYYY) & is.na(ADJ_SENT_DT_YYYY) ~ ADJ_OFF_DT_YYYY,
        is.na(ADJ_FILE_DT_YYYY) & is.na(ADJ_DISP_DT_YYYY) ~ ADJ_SENT_DT_YYYY,
        is.na(ADJ_FILE_DT_YYYY) ~ ADJ_DISP_DT_YYYY,
        !is.na(ADJ_FILE_DT_YYYY) ~ ADJ_FILE_DT_YYYY
      )
  ) %>%
  filter(year >= start_year, year <= end_year, disposition == "guilty") %>%
  count(year, state, name = "nr_guilty") %>%
  mutate(group = "adj_all") %>%
  as_tibble()

summ_pro <-
  pro %>%
  lazy_dt() %>%
  filter(PRO_ST_ORI_FIPS %in% focal_states) %>%
  inner_join(cjars_pik_crosswalk, by = "CJARS_ID") %>%
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
  count(year, state, name = "nr_guilty") %>%
  mutate(group = "pro") %>%
  as_tibble()

summ_inc <-
  inc %>%
  lazy_dt() %>%
  filter(INC_ST_ORI_FIPS %in% focal_states) %>%
  inner_join(cjars_pik_crosswalk, by = "CJARS_ID") %>%
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
  count(year, state, name = "nr_guilty") %>%
  mutate(group = "inc") %>%
  as_tibble()

summ_pro_inc <-
  bind_rows(summ_pro, summ_inc) %>%
  pivot_wider(names_from = "group", values_from = "nr_guilty") %>%
  rowwise() %>%
  mutate(nr_guilty = sum(pro, inc), group = "pro_and_inc") %>%
  select(-pro, -inc)

################################################################################
# 1st, examine how many cases we lose when we only consider one date at a time.
################################################################################
nr_cases_per_date <- function(df, y, m, d, coverage_df) {
  df %>%
    lazy_dt() %>%
    filter(!is.na(.data[[y]]), !is.na(.data[[m]]), !is.na(.data[[d]])) %>%
    full_join(coverage_df, by = "state") %>%
    filter(.data[[y]] >= start_year, .data[[y]] <= end_year, disposition == "guilty") %>%
    count(state, .data[[y]], name = "nr_guilty") %>%
    mutate(group = paste0("total_", y)) %>%
    rename(year = y) %>%
    as_tibble()
}

nr_disp <- nr_cases_per_date(adj_fltr, "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD", complete_coverage_years)
nr_file <- nr_cases_per_date(adj_fltr, "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD", complete_coverage_years)
nr_off <- nr_cases_per_date(adj_fltr, "ADJ_OFF_DT_YYYY", "ADJ_OFF_DT_MM", "ADJ_OFF_DT_DD", complete_coverage_years)
nr_sent <- nr_cases_per_date(adj_fltr, "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD", complete_coverage_years)

# Sometimes, one of the subsets will have a larger number of cases than all adjudications.
# This is because the number of cases each year per state is slightly different for
# all adjudications. We use all dates to find an appropriate year. I.e., the same
# case may be classified in different years.
nr_adj <- bind_rows(summ_adj, nr_disp, nr_file, nr_off, nr_sent)

graph_nr_adj <-
  nr_adj %>%
  ggplot(aes(x = year, y = nr_guilty)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = group)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Number of adjudications (guilty)"
  ) +
  facet_wrap(~state, scales = "free")
ggsave(
  file.path(graph_dir, "compare_adj_totals.png"),
  graph_nr_adj,
  width = 16,
  height = 9
)

################################################################################
# Merge adjudications which happened on the same day. I only pick one date to match on
# at a time because it becomes more ambiguous if an adjudication actually refers
# to the same case if they match on one date but not another.
################################################################################
nr_guilty_adj_per_state_per_year <- function(df, y, m, d, coverage_df) {
  df %>%
    lazy_dt() %>%
    filter(!is.na(.data[[y]]) & !is.na(.data[[m]]) & !is.na(.data[[d]])) %>%
    full_join(complete_coverage_years, by = "state") %>%
    filter(.data[[y]] >= start_year, .data[[y]] <= end_year) %>%
    group_by(CJARS_ID, state, .data[[y]], .data[[m]], .data[[d]]) %>%
    summarise(
      n = n(),
      disposition = sum(disposition == "guilty")
    ) %>%
    ungroup() %>%
    mutate(disposition = if_else(disposition > 0, "guilty", "not_guilty")) %>%
    filter(disposition == "guilty") %>%
    count(state, .data[[y]], name = "nr_guilty") %>%
    rename(year = y) %>%
    mutate(group = y) %>%
    as_tibble()
}

summ_adj_disp <-
  nr_guilty_adj_per_state_per_year(
    adj_fltr, "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD", complete_coverage_years
  )

summ_adj_file <-
  nr_guilty_adj_per_state_per_year(
    adj_fltr, "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD", complete_coverage_years
  )

summ_adj_off <-
  nr_guilty_adj_per_state_per_year(
    adj_fltr, "ADJ_OFF_DT_YYYY", "ADJ_OFF_DT_MM", "ADJ_OFF_DT_DD", complete_coverage_years
  )

summ_adj_sent <-
  nr_guilty_adj_per_state_per_year(
    adj_fltr, "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD", complete_coverage_years
  )

summ <- 
  bind_rows(
    nr_disp, nr_file, nr_off, nr_sent, summ_pro_inc,
    summ_adj_disp, summ_adj_file, summ_adj_off, summ_adj_sent
  ) %>%
  mutate(
    group_big =
      case_when(
        str_detect(group, "DISP") ~ "disp",
        str_detect(group, "OFF") ~ "off",
        str_detect(group, "SENT") ~ "sent",
        str_detect(group, "FILE") ~ "file",
        str_detect(group, "pro_and_inc") ~ group
      ),
    type =
      case_when(
        str_detect(group, "total") ~ "total",
        str_detect(group, "pro_and_inc") ~ group,
        !str_detect(group, "total") ~ "individual"
      )
  )

state_graphs_ind_vs_total <-
  map(
    list("FL" = "FL", "TX" = "TX","NC" = "NC", "WI" = "WI", "MI" = "MI"),
    function(df, st) {
      df %>%
        filter(state == st) %>%
        ggplot(aes(x = year, y = nr_guilty)) +
        geom_point(
          position = position_dodge2(width = 0.6),
          size = 3,
          aes(color = group_big, shape = type)
        ) +
        geom_line(
          position = position_dodge2(width = 0.6),
          aes(color = group_big, group = paste0(year, group_big))
        ) +
        theme_bw() +
        labs(
          x = "Year",
          y = "Number of events",
          title = paste0(st, ": # of guilty adjudications (ind. vs. group) vs. # of inc + pro")
        )
    },
    df = summ
  )
pwalk(
  list(state_graphs_ind_vs_total, names(state_graphs_ind_vs_total)),
  function(plot, name, dir) {
    ggsave(
      file.path(dir, paste0("compare_adj_inc_pro_pik_", name, ".png")),
      plot,
      width = 16,
      height = 9
    )
  },
  dir = graph_dir
)

################################################################################
# Merge adjudications which happened on the same day. And only keep adjudication
# which we know for sure(ish) resulted in a punishment for Wisconsin.
################################################################################
nr_punish_adj_wi_per_year <- function(df, y, m, d, coverage_df) {
  df %>%
    lazy_dt() %>%
    filter(!is.na(.data[[y]]) & !is.na(.data[[m]]) & !is.na(.data[[d]]) & state == "WI") %>%
    full_join(complete_coverage_years, by = "state") %>%
    filter(.data[[y]] >= start_year, .data[[y]] <= end_year) %>%
    group_by(CJARS_ID, state, .data[[y]], .data[[m]], .data[[d]]) %>%
    summarise(
      n = n(),
      disposition = sum(disposition == "guilty"),
      punishment_inc = sum(ADJ_SENT_INC != 0, na.rm = T),
      punishment_pro = sum(ADJ_SENT_PRO, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      disposition =
        if_else(
          disposition > 0 & (punishment_inc > 0 | punishment_pro > 0),
          "guilty",
          "not_guilty"
        )
    ) %>%
    filter(disposition == "guilty") %>%
    count(state, .data[[y]], name = "nr_guilty") %>%
    rename(year = y) %>%
    mutate(group = y) %>%
    as_tibble()
}

summ_adj_disp_wi <-
  nr_punish_adj_wi_per_year(
    adj_fltr, "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD", complete_coverage_years
  )

summ_adj_file_wi <-
  nr_punish_adj_wi_per_year(
    adj_fltr, "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD", complete_coverage_years
  )

summ_adj_off_wi <-
  nr_punish_adj_wi_per_year(
    adj_fltr, "ADJ_OFF_DT_YYYY", "ADJ_OFF_DT_MM", "ADJ_OFF_DT_DD", complete_coverage_years
  )

summ_adj_sent_wi <-
  nr_punish_adj_wi_per_year(
    adj_fltr, "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD", complete_coverage_years
  )

summ_wi <-
  bind_rows(
    summ_adj_disp_wi, summ_adj_file_wi, summ_adj_off_wi, summ_adj_sent_wi
  ) %>%
  mutate(
    type = "punishment",
    group_big =
      case_when(
        str_detect(group, "DISP") ~ "disp",
        str_detect(group, "OFF") ~ "off",
        str_detect(group, "SENT") ~ "sent",
        str_detect(group, "FILE") ~ "file",
        str_detect(group, "pro_and_inc") ~ group
      )
  ) %>%
  bind_rows(summ) %>%
  filter(state == "WI")

graph_wi <-
  ggplot(summ_wi, aes(x = year, y = nr_guilty)) +
  geom_point(size = 3, aes(color = group_big, shape = type)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Number of events",
    title = "WI: # of punishment adjudications vs. # of inc + pro"
  )
ggsave(
  file.path(graph_dir, "compare_punish_inc_pro_WI.png"),
  graph_wi,
  height = 10,
  width = 16
)

################################################################################
# Merge adjudications which happened on the same day. And only keep adjudication
# which we know for sure(ish) resulted in a punishment for Texas
################################################################################
nr_punish_adj_tx_per_year <- function(df, y, m, d, coverage_df) {
  df %>%
    lazy_dt() %>%
    filter(!is.na(.data[[y]]) & !is.na(.data[[m]]) & !is.na(.data[[d]]) & state == "TX") %>%
    full_join(complete_coverage_years, by = "state") %>%
    filter(.data[[y]] >= start_year, .data[[y]] <= end_year) %>%
    group_by(CJARS_ID, state, .data[[y]], .data[[m]], .data[[d]]) %>%
    summarise(
      n = n(),
      disposition = sum(disposition == "guilty"),
      punishment_inc = sum(ADJ_SENT_INC != 0, na.rm = T),
      punishment_pro = sum(ADJ_SENT_PRO, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      disposition =
        if_else(
          disposition > 0 & (punishment_inc > 0 | punishment_pro > 0),
          "guilty",
          "not_guilty"
        )
    ) %>%
    filter(disposition == "guilty") %>%
    count(state, .data[[y]], name = "nr_guilty") %>%
    rename(year = y) %>%
    mutate(group = y) %>%
    as_tibble()
}

summ_adj_disp_tx <-
  nr_punish_adj_tx_per_year(
    adj_fltr, "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD", complete_coverage_years
  )

summ_adj_file_tx <-
  nr_punish_adj_tx_per_year(
    adj_fltr, "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD", complete_coverage_years
  )

summ_adj_off_tx <-
  nr_punish_adj_tx_per_year(
    adj_fltr, "ADJ_OFF_DT_YYYY", "ADJ_OFF_DT_MM", "ADJ_OFF_DT_DD", complete_coverage_years
  )

summ_adj_sent_tx <-
  nr_punish_adj_tx_per_year(
    adj_fltr, "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD", complete_coverage_years
  )

summ_tx <-
  bind_rows(
    summ_adj_disp_tx, summ_adj_file_tx, summ_adj_off_tx, summ_adj_sent_tx
  ) %>%
  mutate(
    type = "punishment",
    group_big =
      case_when(
        str_detect(group, "DISP") ~ "disp",
        str_detect(group, "OFF") ~ "off",
        str_detect(group, "SENT") ~ "sent",
        str_detect(group, "FILE") ~ "file",
        str_detect(group, "pro_and_inc") ~ group
      )
  ) %>%
  bind_rows(summ) %>%
  filter(state == "TX")

graph_tx <-
  ggplot(summ_tx, aes(x = year, y = nr_guilty)) +
  geom_point(position = position_dodge2(width = 0.3), size = 3, aes(color = group_big, shape = type)) +
  geom_line(
    position = position_dodge2(width = 0.3),
    aes(color = group_big, group = paste0(year, group_big))
  ) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Number of events",
    title = "TX: # of punishment adjudications vs. # of inc + pro"
  )
ggsave(
  file.path(graph_dir, "compare_punish_inc_pro_TX.png"),
  graph_tx,
  height = 10,
  width = 16
)

################################################################################
# Merge adjudications which happened on the same day. And only keep adjudication
# which we know for sure(ish) resulted in a punishment for North Carolina
################################################################################
nr_punish_adj_nc_per_year <- function(df, y, m, d, coverage_df) {
  df %>%
    lazy_dt() %>%
    filter(!is.na(.data[[y]]) & !is.na(.data[[m]]) & !is.na(.data[[d]]) & state == "NC") %>%
    full_join(complete_coverage_years, by = "state") %>%
    filter(.data[[y]] >= start_year, .data[[y]] <= end_year) %>%
    group_by(CJARS_ID, state, .data[[y]], .data[[m]], .data[[d]]) %>%
    summarise(
      n = n(),
      disposition = sum(disposition == "guilty"),
      punishment_inc_max = sum(ADJ_SENT_INC_MAX != 0, na.rm = T),
      punishment_inc_min = sum(ADJ_SENT_INC_MIN != 0, na.rm = T),
      punishment_pro = sum(ADJ_SENT_PRO != 0, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      disposition =
        if_else(
          disposition > 0 & (punishment_inc_max > 0 | punishment_inc_min | punishment_pro > 0),
          "guilty",
          "not_guilty"
        )
    ) %>%
    filter(disposition == "guilty") %>%
    count(state, .data[[y]], name = "nr_guilty") %>%
    rename(year = y) %>%
    mutate(group = y) %>%
    as_tibble()
}

summ_adj_disp_nc <-
  nr_punish_adj_nc_per_year(
    adj_fltr, "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD", complete_coverage_years
  )

summ_adj_file_nc <-
  nr_punish_adj_nc_per_year(
    adj_fltr, "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD", complete_coverage_years
  )

summ_adj_off_nc <-
  nr_punish_adj_nc_per_year(
    adj_fltr, "ADJ_OFF_DT_YYYY", "ADJ_OFF_DT_MM", "ADJ_OFF_DT_DD", complete_coverage_years
  )

summ_adj_sent_nc <-
  nr_punish_adj_nc_per_year(
    adj_fltr, "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD", complete_coverage_years
  )

summ_nc <-
  bind_rows(
    summ_adj_disp_nc, summ_adj_file_nc, summ_adj_off_nc, summ_adj_sent_nc
  ) %>%
  mutate(
    type = "punishment",
    group_big =
      case_when(
        str_detect(group, "DISP") ~ "disp",
        str_detect(group, "OFF") ~ "off",
        str_detect(group, "SENT") ~ "sent",
        str_detect(group, "FILE") ~ "file",
        str_detect(group, "pro_and_inc") ~ group
      )
  ) %>%
  bind_rows(summ) %>%
  filter(state == "NC")

graph_nc <-
  ggplot(summ_nc, aes(x = year, y = nr_guilty)) +
  geom_point(position = position_dodge2(width = 0.3), size = 3, aes(color = group_big, shape = type)) +
  geom_line(
    position = position_dodge2(width = 0.3),
    aes(color = group_big, group = paste0(year, group_big))
  ) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Number of events",
    title = "NC: # of punishment adjudications vs. # of inc + pro"
  )
ggsave(
  file.path(graph_dir, "compare_punish_inc_pro_NC.png"),
  graph_nc,
  height = 10,
  width = 16
)
