joined_samples_cjsubject <-
  readRDS(file.path("joined_samples_cjsubject.rds"))

################################################################################
# Change this depending on the subject table.
################################################################################
table <- "incar"
d_bcol <- "INC_ENTRY_DT_DD"
m_bcol <- "INC_ENTRY_DT_MM"
y_bcol <- "INC_ENTRY_DT_YYYY"
d_ecol <- "INC_EXIT_DT_DD"
m_ecol <- "INC_EXIT_DT_MM"
y_ecol <- "INC_EXIT_DT_YYYY"
nomdac_table <- "nomdac_incar"
mdac_table <- "mdac_incar"
mdac_alt_table <- "mdac_alt_incar"
state_col <- "INC_ST_ORI_FIPS"
county_col <- "INC_CNTY_ORI_FIPS"
county_year_yaxis <- "Percent of incarcerations"
year_xaxis <- "Year of incarceration entry"

################################################################################
# Compare incarceration between non-MDAC-CJARS and MDAC-CJARS.
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
# Compare proportions of individuals who had an incarceration entry.
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
# Compare average incarceration length and incarceration length distribution.
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
# Compare number of incarceration spells between MDAC and non-MDAC.
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
# Compare incarceration facilities.
################################################################################
incar_fclt <-
  bind_rows(incident_level) %>%
  lazy_dt() %>%
  count(sample, INC_FCL_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(incar_fclt, aes(x = fct_reorder(INC_FCL_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Incarceration Facility Type", y = "Percent") +
  facet_wrap(~sample)

incar_fclt_revise <-
  incar_fclt %>%
  group_by(sample) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = INC_FCL_CD,
    values_from = c("n", "prcnt")
  )

incar_chisq_test_fclt <-
  chisq.test(
    bind_rows(
      incar_fclt_revise %>% filter(sample == "nomdac_incar") %>% select(matches("n_")),
      incar_fclt_revise %>% filter(sample == "mdac_incar") %>% select(matches("n_"))
    ),
    correct = F
  )

incar_chisq_test_alt_fclt <-
  chisq.test(
    bind_rows(
      incar_fclt_revise %>% filter(sample == "nomdac_incar") %>% select(matches("n_")),
      incar_fclt_revise %>% filter(sample == "mdac_alt_incar") %>% select(matches("n_"))
    ),
    correct = F
  )

incar_prop_test_fclt <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_incar", 10)),
      sample2 = as.list(rep("mdac_incar", 10)),
      column = list("n_UU", "n_MD", "n_MN", "n_CM", "n_MX", "n_OT", "n_SP", "n_LJ", "n_FD", "n_AD")
    ),
    conduct_prop_test,
    df = incar_fclt_revise,
    n_column = "total"
  )
names(incar_prop_test_fclt) <- c("n_UU", "n_MD", "n_MN", "n_CM", "n_MX", "n_OT", "n_SP", "n_LJ", "n_FD", "n_AD")

incar_prop_test_alt_fclt <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_incar", 10)),
      sample2 = as.list(rep("mdac_alt_incar", 10)),
      column = list("n_UU", "n_MD", "n_MN", "n_CM", "n_MX", "n_OT", "n_SP", "n_LJ", "n_FD", "n_AD")
    ),
    conduct_prop_test,
    df = incar_fclt_revise,
    n_column = "total"
  )
names(incar_prop_test_alt_fclt) <- c("n_UU", "n_MD", "n_MN", "n_CM", "n_MX", "n_OT", "n_SP", "n_LJ", "n_FD", "n_AD")

################################################################################
# Compare incarceration entry conditions.
################################################################################
incar_entry <-
  bind_rows(incident_level) %>%
  lazy_dt() %>%
  count(sample, INC_ENTRY_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(incar_entry, aes(x = fct_reorder(INC_ENTRY_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Incarceration Entry Conditions", y = "Percent") +
  facet_wrap(~sample)

incar_entry_revise <-
  incar_entry %>%
  select(-prcnt) %>%
  mutate(
    entry_status =
      case_when(
        INC_ENTRY_CD == "UU" ~ "unknown",
        INC_ENTRY_CD == "CC" ~ "court_commitment",
        INC_ENTRY_CD %in% c("RI", "RW", "RN", "PP") ~ "parole_revocation",
        INC_ENTRY_CD %in% c("PN", "PW") ~ "probation_revocation",
        INC_ENTRY_CD %in% c("OT", "TR", "UC", "EI", "EW", "RA", "IE", "SS", "PR") ~ "other"
      )
  ) %>%
  group_by(sample, entry_status) %>%
  summarise(n = sum(n)) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = "entry_status",
    values_from = c("n", "prcnt")
  )

incar_chisq_test_entry <-
  chisq.test(
    bind_rows(
      incar_entry_revise %>% filter(sample == "nomdac_incar") %>% select(matches("n_")),
      incar_entry_revise %>% filter(sample == "mdac_incar") %>% select(matches("n_"))
    ),
    correct = F
  )

incar_chisq_test_alt_entry <-
  chisq.test(
    bind_rows(
      incar_entry_revise %>% filter(sample == "nomdac_incar") %>% select(matches("n_")),
      incar_entry_revise %>% filter(sample == "mdac_alt_incar") %>% select(matches("n_"))
    ),
    correct = F
  )

entry_cols <-
  c("n_court_commitment", "n_other", "n_parole_revocation", "n_probation_revocation", "n_unknown")

incar_prop_test_entry <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_incar", length(entry_cols))),
      sample2 = as.list(rep("mdac_incar", length(entry_cols))),
      column = as.list(entry_cols)
    ),
    conduct_prop_test,
    df = incar_entry_revise,
    n_column = "total"
  )
names(incar_prop_test_entry) <- entry_cols

incar_prop_test_alt_entry <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_incar", length(entry_cols))),
      sample2 = as.list(rep("mdac_alt_incar", length(entry_cols))),
      column = as.list(entry_cols)
    ),
    conduct_prop_test,
    df = incar_entry_revise,
    n_column = "total"
  )
names(incar_prop_test_alt_entry) <- entry_cols

################################################################################
# Compare incarceration exit conditions.
################################################################################
incar_exit <-
  bind_rows(incident_level) %>%
  lazy_dt() %>%
  count(sample, INC_EXIT_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(incar_exit, aes(x = fct_reorder(INC_EXIT_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Incarceration Exit Conditions", y = "Percent") +
  facet_wrap(~sample)

incar_exit_revise <-
  incar_exit %>%
  select(-prcnt) %>%
  mutate(
    exit_status =
      case_when(
        INC_EXIT_CD == "UU" ~ "unknown",
        INC_EXIT_CD == "ES" ~ "release_finish_sentence",
        INC_EXIT_CD == "MR" ~ "release_mandatory_parole",
        INC_EXIT_CD == "PD" ~ "release_parole_board",
        INC_EXIT_CD == "RA" ~ "release_appeal_bond",
        INC_EXIT_CD %in% c("OR", "PR", "UR", "CP") ~ "release_other",
        INC_EXIT_CD %in% c("TR", "OT", "RC", "EA", "DN", "OD", "EX", "IE", "SU", "HI") ~ "other"
      )
  ) %>%
  group_by(sample, exit_status) %>%
  summarise(n = sum(n)) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = "exit_status",
    values_from = c("n", "prcnt")
  )

incar_chisq_test_exit <-
  chisq.test(
    bind_rows(
      incar_exit_revise %>% filter(sample == "nomdac_incar") %>% select(matches("n_")),
      incar_exit_revise %>% filter(sample == "mdac_incar") %>% select(matches("n_"))
    ),
    correct = F
  )

incar_chisq_test_alt_exit <-
  chisq.test(
    bind_rows(
      incar_exit_revise %>% filter(sample == "nomdac_incar") %>% select(matches("n_")),
      incar_exit_revise %>% filter(sample == "mdac_alt_incar") %>% select(matches("n_"))
    ),
    correct = F
  )

exit_cols <-
  c(
    "n_unknown", "n_release_finish_sentence", "n_release_mandatory_parole",
    "n_release_parole_board", "n_release_appeal_bond", "n_release_other", "n_other"
  )

incar_prop_test_exit <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_incar", length(exit_cols))),
      sample2 = as.list(rep("mdac_incar", length(exit_cols))),
      column = as.list(exit_cols)
    ),
    conduct_prop_test,
    df = incar_exit_revise,
    n_column = "total"
  )
names(incar_prop_test_exit) <- exit_cols

################################################################################
# Compare incarceration counties.
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
# Compare incarceration years.
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
