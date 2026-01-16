joined_samples_cjsubject <- readRDS(file.path("joined_samples_cjsubject.rds"))

################################################################################
# Change this depending on the subject table.
################################################################################
table <- "adj"
begin_date <- "ADJ_DISP_DT_YYYY"
nomdac_table <- "nomdac_adj"
mdac_table <- "mdac_adj"
mdac_alt_table <- "mdac_alt_adj"
state_col <- "ADJ_ST_ORI_FIPS"
county_col <- "ADJ_CNTY_ORI_FIPS"
county_year_yaxis <- "Percent of adjudications"
year_xaxis <- "Year of adjudication (using disposition date)"
y_bcol <- "ADJ_DISP_DT_YYYY"

################################################################################
# Compare adjudication between non-MDAC-CJARS and MDAC-CJARS.
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
# Compare number of adjudications between MDAC and non-MDAC.
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
# Compare adjudication offense grades.
################################################################################
adj_off_grd <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_GRD_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(adj_off_grd, aes(x = fct_reorder(ADJ_GRD_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Adjudication offense grades", y = "Percent") +
  facet_wrap(~sample)

adj_off_grd_revise <-
  adj_off_grd %>%
  select(-prcnt) %>%
  group_by(sample, ADJ_GRD_CD) %>%
  summarise(n = sum(n)) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = "ADJ_GRD_CD",
    values_from = c("n", "prcnt")
  )

adj_chisq_test_grd <-
  chisq.test(
    bind_rows(
      adj_off_grd_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_off_grd_revise %>% filter(sample == "mdac_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

adj_chisq_test_alt_grd <-
  chisq.test(
    bind_rows(
      adj_off_grd_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_off_grd_revise %>% filter(sample == "mdac_alt_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

grd_cols <- c("n_FE", "n_MI", "n_UU")

adj_prop_test_grd <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(grd_cols))),
      sample2 = as.list(rep("mdac_adj", length(grd_cols))),
      column = as.list(grd_cols)
    ),
    conduct_prop_test,
    df = adj_off_grd_revise,
    n_column = "total"
  )
names(adj_prop_test_grd) <- grd_cols

adj_prop_test_alt_grd <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(grd_cols))),
      sample2 = as.list(rep("mdac_alt_adj", length(grd_cols))),
      column = as.list(grd_cols)
    ),
    conduct_prop_test,
    df = adj_off_grd_revise,
    n_column = "total"
  )
names(adj_prop_test_alt_grd) <- grd_cols

################################################################################
# Compare adjudication legal codes.
################################################################################
adj_lgl_cd <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_OFF_LGL_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(adj_lgl_cd, aes(x = fct_reorder(ADJ_OFF_LGL_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Adjudication legal codes", y = "Percent") +
  facet_wrap(~sample)

adj_lgl_cd_revise <-
  adj_lgl_cd %>%
  select(-prcnt) %>%
  group_by(sample, ADJ_OFF_LGL_CD) %>%
  summarise(n = sum(n)) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = "ADJ_OFF_LGL_CD",
    values_from = c("n", "prcnt")
  )

adj_chisq_test_lgl <-
  chisq.test(
    bind_rows(
      adj_lgl_cd_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_lgl_cd_revise %>% filter(sample == "mdac_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

adj_chisq_test_alt_lgl <-
  chisq.test(
    bind_rows(
      adj_lgl_cd_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_lgl_cd_revise %>% filter(sample == "mdac_alt_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

lgl_cols <- c("n_ST", "n_OR", "n_UU")

adj_prop_test_lgl <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(lgl_cols))),
      sample2 = as.list(rep("mdac_adj", length(lgl_cols))),
      column = as.list(lgl_cols)
    ),
    conduct_prop_test,
    df = adj_lgl_cd_revise,
    n_column = "total"
  )
names(adj_prop_test_lgl) <- lgl_cols

adj_prop_test_alt_lgl <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(lgl_cols))),
      sample2 = as.list(rep("mdac_alt_adj", length(lgl_cols))),
      column = as.list(lgl_cols)
    ),
    conduct_prop_test,
    df = adj_lgl_cd_revise,
    n_column = "total"
  )
names(adj_prop_test_alt_lgl) <- lgl_cols

################################################################################
# Compare adjudication offenses at time of filing.
################################################################################
adj_chrg_off <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_CHRG_OFF_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

adj_chrg_off_wide <-
  adj_chrg_off %>%
  select(-n) %>%
  pivot_wider(
    id_cols = "ADJ_CHRG_OFF_CD", names_from = sample, values_from = prcnt
  ) %>%
  mutate(diff = nomdac_adj - mdac_adj)

ggplot(adj_chrg_off, aes(x = fct_reorder(ADJ_CHRG_OFF_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Charge offense at time of filing", y = "Percent") +
  facet_wrap(~sample)

adj_chrg_off_broad <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  mutate(
    offense_category = str_sub(ADJ_CHRG_OFF_CD, 1, 1),
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

adj_chrg_off_broad %>%
  ggplot(aes(x = fct_reorder(offense_category_detailed, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Charge offense at time of filing (broad)", y = "Percent") +
  facet_wrap(~sample)

adj_chrg_off_broad_revise <-
  adj_chrg_off_broad %>%
  group_by(sample) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = offense_category_detailed,
    values_from = c("n", "prcnt")
  )

adj_chisq_test_chrg <-
  chisq.test(
    bind_rows(
      adj_chrg_off_broad_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_chrg_off_broad_revise %>% filter(sample == "mdac_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

adj_chisq_test_alt_chrg <-
  chisq.test(
    bind_rows(
      adj_chrg_off_broad_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_chrg_off_broad_revise %>% filter(sample == "mdac_alt_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

chrg_cols <-
  c("n_violent", "n_property", "n_drug", "n_dui", "n_public_order", "n_traffic", "n_other")

adj_prop_test_chrg <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(chrg_cols))),
      sample2 = as.list(rep("mdac_adj", length(chrg_cols))),
      column = chrg_cols
    ),
    conduct_prop_test,
    df = adj_chrg_off_broad_revise,
    n_column = "total"
  )
names(adj_prop_test_chrg) <- chrg_cols

################################################################################
# Compare dispositions.
################################################################################
adj_disp_cd <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_DISP_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(adj_disp_cd, aes(x = fct_reorder(ADJ_DISP_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Disposition Status", y = "Percent") +
  facet_wrap(~sample)

adj_disp_cd_revise <-
  adj_disp_cd %>%
  select(-prcnt) %>%
  mutate(
    disposition =
      case_when(
        ADJ_DISP_CD == "GU" ~ "guilty_unk",
        ADJ_DISP_CD == "GP" ~ "guilty_plea",
        ADJ_DISP_CD == "ND" ~ "dismissal",
        ADJ_DISP_CD == "UU" ~ "unknown",
        ADJ_DISP_CD == "DU" ~ "diversion",
        ADJ_DISP_CD %in% c("NU", "NP", "NA") ~ "not_guilty_acquittal",
        ADJ_DISP_CD %in% c("GC", "GJ", "GI") ~ "guilty_other",
        ADJ_DISP_CD %in% c("PT", "PU") ~ "procedural_or_transfer",
        ADJ_DISP_CD %in% c("NM", "NI") ~ "mistrial_dismissal_insane"
      )
  ) %>%
  group_by(sample, disposition) %>%
  summarise(n = sum(n)) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = "disposition",
    values_from = c("n", "prcnt")
  )

adj_chisq_test_disp_cd <-
  chisq.test(
    bind_rows(
      adj_disp_cd_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_disp_cd_revise %>% filter(sample == "mdac_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

disp_cd_cols <-
  c(
    "n_guilty_unk", "n_guilty_plea", "n_dismissal", "n_unknown", "n_diversion",
    "n_not_guilty_acquittal", "n_guilty_other", "n_procedural_or_transfer",
    "n_mistrial_dismissal_insane"
  )

adj_prop_test_disp_cd <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(disp_cd_cols))),
      sample2 = as.list(rep("mdac_adj", length(disp_cd_cols))),
      column = as.list(disp_cd_cols)
    ),
    conduct_prop_test,
    df = adj_disp_cd_revise,
    n_column = "total"
  )
names(adj_prop_test_disp_cd) <- disp_cd_cols

################################################################################
# Compare adjudication offenses at time of disposition.
################################################################################
adj_disp_off <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_DISP_OFF_CD) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100) %>%
  ungroup() %>%
  as_tibble()

ggplot(adj_disp_off, aes(x = fct_reorder(ADJ_DISP_OFF_CD, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Charge offense at time of disposition", y = "Percent") +
  facet_wrap(~sample)

adj_disp_off_broad <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  mutate(
    offense_category = str_sub(ADJ_DISP_OFF_CD, 1, 1),
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

adj_disp_off_broad %>%
  ggplot(aes(x = fct_reorder(offense_category_detailed, prcnt), y = prcnt)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Charge offense at time of disposition (broad)", y = "Percent") +
  facet_wrap(~sample)

adj_disp_off_broad_revise <-
  adj_disp_off_broad %>%
  group_by(sample) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = offense_category_detailed,
    values_from = c("n", "prcnt")
  )

adj_chisq_test_disp_off <-
  chisq.test(
    bind_rows(
      adj_disp_off_broad_revise %>% filter(sample == "nomdac_adj") %>% select(matches("n_")),
      adj_disp_off_broad_revise %>% filter(sample == "mdac_adj") %>% select(matches("n_"))
    ),
    correct = F
  )

disp_off_cols <-
  c("n_violent", "n_property", "n_drug", "n_dui", "n_public_order", "n_traffic", "n_other")

adj_prop_test_disp_off <-
  pmap(
    list(
      sample1 = as.list(rep("nomdac_adj", length(disp_off_cols))),
      sample2 = as.list(rep("mdac_adj", length(disp_off_cols))),
      column = disp_off_cols
    ),
    conduct_prop_test,
    df = adj_disp_off_broad_revise,
    n_column = "total"
  )
names(adj_prop_test_disp_off) <- disp_off_cols

################################################################################
# Compare proportions of individuals w/ a sentence involving community service.
################################################################################
adj_sent_serv <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_SENT_SERV) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = ADJ_SENT_SERV,
    values_from = c("n", "prcnt")
  ) %>%
  as_tibble()

################################################################################
# Compare proportions of individuals w/ a death sentence.
################################################################################
adj_sent_dth <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_SENT_DTH) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = ADJ_SENT_DTH,
    values_from = c("n", "prcnt")
  ) %>%
  as_tibble()

################################################################################
# Compare proportions of individuals w/ a suspended sentence.
################################################################################
adj_sent_sus <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_SENT_SUS) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = ADJ_SENT_SUS,
    values_from = c("n", "prcnt")
  ) %>%
  as_tibble()

################################################################################
# Compare proportions of individuals w/ a treatment_based sentence.
################################################################################
adj_sent_trt <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
  ) %>%
  lazy_dt() %>%
  count(sample, ADJ_SENT_TRT) %>%
  group_by(sample) %>%
  mutate(prcnt = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c("sample", "total"),
    names_from = ADJ_SENT_TRT,
    values_from = c("n", "prcnt")
  ) %>%
  as_tibble()

################################################################################
# Compare prison sentence length between MDAC and no-MDAC.
################################################################################
sent_inc_summary <-
  map(
    list(
      "mdac" = joined_samples_cjsubject$mdac_adj$df,
      "nomdac" = joined_samples_cjsubject$nomdac_adj$df
    ),
    function(df) {
      df_clean <- df %>% filter(ADJ_SENT_INC != -88888 & ADJ_SENT_INC != -99999)
      
      summary <- summary(df_clean$ADJ_SENT_INC)
      
      df_count <-
        df %>%
        lazy_dt() %>%
        mutate(
          type = 
            case_when(
              is.na(ADJ_SENT_INC) ~ "missing",
              ADJ_SENT_INC == -88888 ~ "life_sentence",
              ADJ_SENT_INC == -99999 ~ "death_sentence",
              ADJ_SENT_INC != -88888 & ADJ_SENT_INC != -99999 ~ "normal"
            )
        ) %>%
        count(type) %>%
        mutate(prcnt = n / sum(n) * 100) %>%
        as_tibble()
      
      return(list(summary = summary, df_count = df_count))
    }
  )

################################################################################
# Compare probation sentence length between MDAC and no-MDAC.
################################################################################
sent_pro_summary <-
  map(
    list(
      "mdac" = joined_samples_cjsubject$mdac_adj$df,
      "nomdac" = joined_samples_cjsubject$nomdac_adj$df
    ),
    function(df) {summary(df$ADJ_SENT_PRO)}
  )

################################################################################
# Compare restitution amounts between MDAC and no-MDAC.
################################################################################
sent_rest_summary <-
  map(
    list(
      "mdac" = joined_samples_cjsubject$mdac_adj$df,
      "nomdac" = joined_samples_cjsubject$nomdac_adj$df
    ),
    function(df) {summary(df$ADJ_SENT_REST)}
  )

################################################################################
# Compare fine amounts between MDAC and no-MDAC. Negative values?
################################################################################
sent_fine_summary <-
  map(
    list(
      "mdac" = joined_samples_cjsubject$mdac_adj$df,
      "nomdac" = joined_samples_cjsubject$nomdac_adj$df
    ),
    function(df) {summary(df$ADJ_SENT_FINE)}
  )

################################################################################
# Compare adjudication counties.
################################################################################
county <-
  bind_rows(
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
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
    joined_samples_cjsubject$nomdac_adj$df %>% mutate(sample = nomdac_table),
    joined_samples_cjsubject$mdac_adj$df %>% mutate(sample = mdac_table),
    joined_samples_cjsubject$mdac_alt_adj$df %>% mutate(sample = mdac_alt_table)
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
