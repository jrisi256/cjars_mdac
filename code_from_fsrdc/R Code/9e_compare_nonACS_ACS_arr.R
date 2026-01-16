joined_samples_cjsubject <-
  readRDS(file.path("joined_samples_cjsubject.rds"))

################################################################################
# Change this depending on the subject table.
################################################################################
table <- "arr"
begin_date <- "ARR_ARR_DT_YYYY"
nomdac_table <- "nomdac_arr"
mdac_table <- "mdac_arr"
mdac_alt_table <- "mdac_alt_arr"
state_col <- "ARR_ST_ORI_FIPS"
county_col <- "ARR_CNTY_ORI_FIPS"
county_year_yaxis <- "Percent of arrests"
year_xaxis <- "Year of arrest"
y_bcol <- "ARR_ARR_DT_YYYY"

################################################################################
# Compare arrests between non-MDAC-CJARS and MDAC-CJARS.
################################################################################
individual_level <-
  pmap(
    list(
      joined_samples_cjsubject[str_detect(names(joined_samples_cjsubject), table)],
      names(joined_samples_cjsubject[str_detect(names(joined_samples_cjsubject), table)])
    ),
    function(list, name) {
      list$df %>%
        lazy_dt() %>%
        count(pik) %>%
        mutate(sample = name) %>%
        as_tibble()
    }
  )

################################################################################
# Compare proportions of individuals who had an arrest.
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
# Compare number of arrests between MDAC and non-MDAC.
################################################################################
individual_level_summary <- map(individual_level, function(df) {summary(df$n)})

ttest_nr <-
  t.test(
    individual_level[[nomdac_table]]$n, individual_level[[mdac_table]]$n
  )

ttest_alt_nr <-
  t.test(
    individual_level[[nomdac_table]]$n, individual_level[[mdac_alt_table]]$n
  )

kstest_nr <-
  ks.test(
    individual_level[[nomdac_table]]$n, individual_level[[mdac_table]]$n
  )

kstest_alt_nr <-
  ks.test(
    individual_level[[nomdac_table]]$n, individual_level[[mdac_alt_table]]$n
  )

ggplot(bind_rows(individual_level), aes(x = n)) +
  geom_histogram(color = "black", aes(fill = sample), bins = 25) +
  facet_wrap(~sample, scales = "free_y") +
  theme_bw()

################################################################################
# Compare arrest offenses.
################################################################################
arr_off <-
  bind_rows(
    joined_samples_cjsubject$nomdac_arr$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_arr$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_arr$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ARR_OFF_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(arr_off, aes(x = fct_reorder(ARR_OFF_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Arrest Type", y = "Percent") +
  facet_wrap(~sample)

arr_off_broad <-
  bind_rows(
    joined_samples_cjsubject$nomdac_arr$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_arr$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_arr$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  mutate(
    offense_category = str_sub(ARR_OFF_CD, 1, 1),
    offense_category_detailed =
      case_when(
        offense_category == 1 ~ "violent",
        offense_category == 2 ~ "property",
        offense_category == 3 ~ "drug",
        offense_category == 4 ~ "dui",
        offense_category == 5 ~ "public_order",
        offense_category == 6 ~ "traffic",
        offense_category == 8 | offense_category == 9 ~ "other"
      )
  ) %>%
  count(sample, offense_category_detailed) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(arr_off_broad, aes(x = fct_reorder(offense_category_detailed, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Arrest Type", y = "Percent") +
  facet_wrap(~sample)

arr_off_broad_revise <-
  arr_off_broad %>%
  group_by(sample) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = offense_category_detailed,
    values_from = c("n", "prcnt")
  )

arr_chisq_test_off <-
  chisq.test(
    bind_rows(
      arr_off_broad_revise %>% filter(sample == "nomdac_arr") %>% select(matches("n_")),
      arr_off_broad_revise %>% filter(sample == "mdac_arr") %>% select(matches("n_"))
    ),
    correct = F
  )

arr_chisq_test_alt_off <-
  chisq.test(
    bind_rows(
      arr_off_broad_revise %>% filter(sample == "nomdac_arr") %>% select(matches("n_")),
      arr_off_broad_revise %>% filter(sample == "mdac_alt_arr") %>% select(matches("n_"))
    ),
    correct = F
  )

off_cols <-
  c("n_violent", "n_property", "n_drug", "n_dui", "n_public_order", "n_traffic", "n_other")

arr_prop_test_off <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_arr", length(off_cols))),
      sample2 = as.list(rep("mdac_arr", length(off_cols))),
      column = off_cols
    ),
    conduct_prop_test,
    df = arr_off_broad_revise,
    n_column = "total"
  )
names(arr_prop_test_off) <- off_cols

################################################################################
# Compare arrest counties.
################################################################################
county <-
  bind_rows(
    joined_samples_cjsubject$nomdac_arr$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_arr$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_arr$df %>% mutate(sample = mdac_alt_table)
  ) %>%
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
# Compare arrest years.
################################################################################
year <-
  bind_rows(
    joined_samples_cjsubject$nomdac_arr$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_arr$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_arr$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, .data[[y_bcol]]) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

year %>%
  filter(sample != mdac_alt_table, .data[[y_bcol]] >= 1980) %>%
  ggplot(aes(x = .data[[y_bcol]], y = prcnt)) +
  geom_bar(stat = "identity", aes(fill = sample), position = "dodge") +
  theme_bw() +
  labs(x = year_xaxis, y = county_year_yaxis)
