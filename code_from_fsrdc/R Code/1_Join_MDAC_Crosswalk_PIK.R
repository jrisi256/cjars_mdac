library(haven)
library(dplyr)
library(readr)
library(dtplyr)

################################################################################
crosswalk_dir <- ""

crosswalk_acs_2008 <-
  read_sas(
    file.path(crosswalk_dir, "crosswalk_acs2008.sas7bdat")
  )

crosswalk_acs_2008_del <-
  read_sas(
    file.path(crosswalk_dir, "crosswalk_acs2008_del2.sas7bdat")
  ) %>%
  rename(VERFLG = verflg)

crosswalk_acs_2008_total <-
  bind_rows(crosswalk_acs_2008, crosswalk_acs_2008_del) %>%
  lazy_dt()

################################################################################
# Check if the crosswalk file is unique on (CMID, PNUM).
unique_crosswalk_df <-
  crosswalk_acs_2008_total %>%
  count(CMID, PNUM) %>%
  as_tibble()

check_unique_crosswalk <- all(unique_crosswalk_df$n == 1)

# Check if the crosswalk file is unique on PIK.
count_matches_crosswalk <-
  table(as_tibble(crosswalk_acs_2008_total)[["pik"]] == "", useNA = "ifany")
prop_matches_crosswalk <- prop.table(count_matches_crosswalk)

################################################################################
# Check the number of PIK matches.
count_pik_matches_df <-
  crosswalk_acs_2008_total %>%
  filter(pik != "" & !is.na(pik)) %>%
  group_by(pik) %>%
  mutate(n = n()) %>%
  as_tibble() %>%
  ungroup()

count_pik_matches <- table(count_pik_matches_df$n)
prop_pik_matches <- prop.table(count_pik_matches)

unique_piks <- length(unique(count_pik_matches_df$pik))

################################################################################
mdac <-
  read_sas(
    file.path("mdac2008_2015_v1.sas7bdat"),
    col_select = c("CMID", "PNUM")
  ) %>%
  lazy_dt()

unique_mdac_df <- mdac %>% count(CMID, PNUM) %>% as_tibble()
check_unique_mdac <- all(unique_mdac_df$n == 1)

################################################################################
mdac_pik <-
  full_join(mdac, crosswalk_acs_2008_total, by = c("CMID", "PNUM")) %>%
  select(-VERFLG) %>%
  group_by(pik) %>%
  filter(n() == 1) %>%
  as_tibble()

write_csv(mdac_pik, "1_joined_mdac_pik.csv")
