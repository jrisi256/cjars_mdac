joined_samples_cjsubject <-
  readRDS(file.path("joined_samples_cjsubject.rds"))

################################################################################
# Change this depending on the subject table.
################################################################################
table <- "parole"
d_bcol <- "PAR_BGN_DT_DD"
m_bcol <- "PAR_BGN_DT_MM"
y_bcol <- "PAR_BGN_DT_YYYY"
d_ecol <- "PAR_END_DT_DD"
m_ecol <- "PAR_END_DT_MM"
y_ecol <- "PAR_END_DT_YYYY"
nomdac_table <- "nomdac_parole"
mdac_table <- "mdac_parole"
mdac_alt_table <- "mdac_alt_parole"
end_status_col <- "PAR_END_CD"
end_status_plot_title <- "Parole end status"
state_col <- "PAR_ST_ORI_FIPS"
county_col <- "PAR_CNTY_ORI_FIPS"
county_year_yaxis <- "Percent of paroles"
year_xaxis <- "Year of parole entry"

################################################################################
# Compare parole between non-MDAC-CJARS and MDAC-CJARS.
################################################################################
incident_level <-
  pmap(
    list(
      joined_samples_cjsubject[str_detect(names(joined_samples_cjsubject), table)],
      names(joined_samples_cjsubject[str_detect(names(joined_samples_cjsubject), table)])
    ),
    function(list, name, d_bcol, m_bcol, y_bcol, d_ecol, m_ecol, y_ecol) {
      find_length(list$df, name, d_bcol, m_bcol, y_bcol, d_ecol, m_ecol, y_ecol)
    },
    d_bcol = d_bcol, m_bcol = m_bcol, y_bcol = y_bcol,
    d_ecol = d_ecol, m_ecol = m_ecol, y_ecol = y_ecol
  )

individual_level <- pmap(list(incident_level, names(incident_level)), find_nr)

################################################################################
# Compare proportions of individuals who had a parole entry.
################################################################################
prop_test_has_record <-
  prop.test(
    x =
      c(
        joined_samples_cjsubject[[nomdac_table]]$nr_pik_cjsubject,
        joined_samples_cjsubject[[mdac_table]]$nr_pik_cjsubject
      ),
    n =
      c(
        joined_samples_cjsubject[[nomdac_table]]$nr_pik_sample,
        joined_samples_cjsubject[[mdac_table]]$nr_pik_sample
      ),
    correct = F
  )

prop_test_alt_has_record <-
  prop.test(
    x =
      c(
        joined_samples_cjsubject[[nomdac_table]]$nr_pik_cjsubject,
        joined_samples_cjsubject[[mdac_alt_table]]$nr_pik_cjsubject
      ),
    n =
      c(
        joined_samples_cjsubject[[nomdac_table]]$nr_pik_sample,
        joined_samples_cjsubject[[mdac_alt_table]]$nr_pik_sample
      ),
    correct = F
  )

################################################################################
# Compare average parole length and parole length distribution.
################################################################################
incident_level_clean <- map(incident_level, function(df) {df %>% filter(length >= 0)})
incident_level_clean_summary <- map(incident_level_clean, function(df) {summary(df$length)})

ttest_length <-
  t.test(
    incident_level_clean[[nomdac_table]]$length,
    incident_level_clean[[mdac_table]]$length
  )

ttest_alt_length <-
  t.test(
    incident_level_clean[[nomdac_table]]$length,
    incident_level_clean[[mdac_alt_table]]$length
  )

kstest_length <-
  ks.test(
    incident_level_clean[[nomdac_table]]$length,
    incident_level_clean[[mdac_table]]$length
  )

kstest_alt_length <-
  ks.test(
    incident_level_clean[[nomdac_table]]$length,
    incident_level_clean[[mdac_alt_table]]$length
  )

ggplot(bind_rows(incident_level_clean), aes(x = length)) +
  geom_histogram(color = "black", aes(fill = sample), bins = 50) +
  facet_wrap(~sample, scales = "free_y") +
  theme_bw()

################################################################################
# Compare number of paroles between MDAC and non-MDAC.
################################################################################
individual_level_summary <- map(individual_level, function(df) {summary(df$total)})

ttest_nr <-
  t.test(
    individual_level[[nomdac_table]]$total, individual_level[[mdac_table]]$total
  )

ttest_alt_nr <-
  t.test(
    individual_level[[nomdac_table]]$total, individual_level[[mdac_alt_table]]$total
  )

kstest_nr <-
  ks.test(
    individual_level[[nomdac_table]]$total, individual_level[[mdac_table]]$total
  )

kstest_alt_nr <-
  ks.test(
    individual_level[[nomdac_table]]$total, individual_level[[mdac_alt_table]]$total
  )

ggplot(bind_rows(individual_level), aes(x = total)) +
  geom_histogram(color = "black", aes(fill = sample), bins = 25) +
  facet_wrap(~sample, scales = "free_y") +
  theme_bw()

################################################################################
# Compare parole end statuses.
################################################################################
end_status <-
  bind_rows(incident_level) %>%
  lazy_dt() %>%
  count(sample, .data[[end_status_col]]) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(end_status, aes(x = fct_reorder(.data[[end_status_col]], prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = end_status_plot_title, y = "Percent") +
  facet_wrap(~sample)

# This will be different depending on the specific subject table being examined.
end_status_revise <-
  end_status %>%
  select(-prcnt) %>%
  mutate(
    end_status =
      case_when(
        .data[[end_status_col]] == "UU" ~ "unknown",
        .data[[end_status_col]] == "CO" ~ "completion",
        .data[[end_status_col]] %in% c("RV", "RO", "RN") ~ "incarceration",
        .data[[end_status_col]] %in% c("AB", "DE", "TR", "OT", "OU") ~ "other"
      )
  ) %>%
  select(-all_of(end_status_col)) %>%
  group_by(sample, end_status) %>%
  summarise(n = sum(n)) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  group_by(sample) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = "end_status",
    values_from = c("n", "prcnt")
  )

chisq_test_end_status <-
  chisq.test(
    bind_rows(
      end_status_revise %>% filter(sample == nomdac_table) %>% select(matches("n_")),
      end_status_revise %>% filter(sample == mdac_table) %>% select(matches("n_"))
    ),
    correct = F
  )

chisq_test_alt_end_status <-
  chisq.test(
    bind_rows(
      end_status_revise %>% filter(sample == nomdac_table) %>% select(matches("n_")),
      end_status_revise %>% filter(sample == mdac_table) %>% select(matches("n_"))
    ),
    correct = F
  )

end_status_cols <- c("n_completion", "n_incarceration", "n_unknown", "n_other")

prop_test_endst <-
  pmap(
    list(
      sample1 = as.list(rep(nomdac_table, 4)),
      sample2 = as.list(rep(mdac_table, 4)),
      column = as.list(end_status_cols)
    ),
    conduct_prop_test,
    df = end_status_revise,
    n_column = "total"
  )
names(prop_test_endst) <- end_status_cols

prop_test_alt_endst <-
  pmap(
    list(
      sample1 = as.list(rep(nomdac_table, 4)),
      sample2 = as.list(rep(mdac_alt_table, 4)),
      column = as.list(end_status_cols)
    ),
    conduct_prop_test,
    df = end_status_revise,
    n_column = "total"
  )
names(prop_test_alt_endst) <- end_status_cols

################################################################################
# Compare parole counties.
################################################################################
county <-
  incident_level %>%
  bind_rows() %>%
  lazy_dt() %>%
  count(sample, .data[[state_col]], .data[[county_col]]) %>%
  group_by(sample, .data[[state_col]]) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(
    sample != mdac_alt_table,
    .data[[county_col]] != "",
    .data[[state_col]] %in% c("12", "26", "37", "48", "55")
  ) %>%
  as_tibble()

map(
  unique(county[[state_col]]),
  function(state, df, state_column, county_column, yaxis) {
    df %>%
      filter(.data[[state_column]] == state) %>%
      ggplot(aes(x = fct_reorder(.data[[county_column]], prcnt), y = prcnt)) +
      geom_bar(stat = "identity", aes(fill = sample), position = "dodge") +
      theme_bw() +
      facet_wrap(~.data[[state_column]], scales = "free") +
      labs(x = "County", y = yaxis)
  },
  df = county,
  state_column = state_col,
  county_column = county_col,
  yaxis = county_year_yaxis
)

################################################################################
# Compare parole years
################################################################################
year <-
  incident_level %>%
  bind_rows() %>%
  lazy_dt() %>%
  mutate(year = year(begin_date)) %>%
  count(sample, year) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

year %>%
  filter(sample != mdac_alt_table, year >= 1980) %>%
  ggplot(aes(x = year, y = prcnt)) +
  geom_bar(stat = "identity", aes(fill = sample), position = "dodge") +
  theme_bw() +
  labs(x = year_xaxis, y = county_year_yaxis)
