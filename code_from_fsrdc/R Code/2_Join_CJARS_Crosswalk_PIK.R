library(haven)
library(dplyr)
library(readr)
library(purrr)
library(dtplyr)
library(stringr)

cjars_dir <- ""

################################################################################
cjars_roster <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_roster_rsch.sas7bdat"),
    catalog_file = file.path(cjars_dir, "um_cjars_2023q3_roster.sas7bcat"),
    col_select = c("DQB_SOURCE_ID", "CJARS_ID", "ALIAS", "SRC_ST", "DOB_YYYY")
  ) %>%
  lazy_dt() #%>%
  # group_by(CJARS_ID) %>%
  # mutate(n = n()) %>%
  # ungroup() %>%
  # as_tibble() %>%

# Check number of n-to-1 matches of CJARS IDs.
n_cjars_matches <-
  cjars_roster %>%
  lazy_dt() %>%
  distinct(CJARS_ID, .keep_all = T) %>%
  as_tibble()

table_n_cjars_matches <- table(n_cjars_matches$n)

# Check number of unique CJARS IDs
length_unique_cjars_id <- length(unique(cjars_roster[["CJARS_ID"]]))

# Count number of states
nr_states_cjars_roster <- table(cjars_roster$SRC_ST)

################################################################################
cjars_roster_pik <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_roster_pvs.sas7bdat"),
    col_select = c("pik", "DQB_SOURCE_ID"),
  ) %>%
  lazy_dt()

# Check number of records with non-missing PIKs.
nm_pik <-
  cjars_roster_pik %>%
  filter(pik != "") %>%
  # group_by(pik) %>%
  # mutate(n = n()) %>%
  # ungroup() %>%
  as_tibble()

# Check number of n-to-1 matches of PIKs.
n_pik_matches <-
  nm_pik %>%
  lazy_dt() %>%
  distinct(pik, .keep_all = T) %>%
  as_tibble()

table_n_pik_matches <- table(n_pik_matches$n)

# Count the number of unique PIKS, and the number of 1-to-1 matches.
length_unique_pik <- length(unique(nm_pik$pik))

################################################################################
# Join CJARS roster to PIK Crosswalk file.
nm_pik_dt <- nm_pik %>% lazy_dt(key_by = "DQB_SOURCE_ID") #%>% select(-n)

join_cjars_pik <- 
  cjars_roster %>%
  lazy_dt(key_by = "DQB_SOURCE_ID") %>%
  # select(-n) %>%
  inner_join(nm_pik_dt, by = "DQB_SOURCE_ID") %>%
  as_tibble()

nr_records_state <- table(join_cjars_pik$SRC_ST)

################################################################################
# disambiguate between a CJARS ID which matches to multiple, different PIKs
# First, remove duplicate entries.
join_cjars_pik_unique <-
  join_cjars_pik %>%
  lazy_dt() %>%
  count(CJARS_ID, pik, ALIAS, SRC_ST, DOB_YYYY) %>%
  as_tibble()

# Second, remove all alias entries.
join_cjars_pik_no_alias <-
  join_cjars_pik_unique %>%
  lazy_dt() %>%
  group_by(CJARS_ID) %>%
  filter(all(ALIAS == 1) | ALIAS == 0) %>%
  select(-ALIAS) %>%
  ungroup() %>%
  as_tibble()

# Read in Numident file.
numident <-
  read_sas(
    file.path("cnum_2024q1.sas7bdat"),
    col_select = c("pik", "dobcc", "dobyy", "pobst")
  ) %>%
  lazy_dt(key_by = "pik") %>%
  mutate(yob = as.numeric(paste0(dobcc, dobyy))) %>%
  select(-dobcc, -dobyy)

# Third, keep entries which are closest to year of birth from numident.
join_cjars_pik_numident <-
  join_cjars_pik_no_alias %>%
  lazy_dt(key_by = "pik") %>%
  left_join(numident, by = "pik") %>%
  mutate(year_diff = abs(yob - DOB_YYYY)) %>%
  group_by(CJARS_ID) %>%
  filter(all(is.na(year_diff)) | (year_diff == min(year_diff, na.rm = T) & !is.na(year_diff))) %>%
  ungroup() %>%
  select(-yob, -DOB_YYYY, -year_diff) %>%
  as_tibble()

# 4th, compare 1st 2 characters of CJARS ID to birth state. Why not use SRC ST?
# 1st 2 characters of CJARS ID do not always match SRC ST (rare but happens).
# Also, 1-to-1 matches will not necessarily have matching states.
join_cjars_pik_samest <-
  join_cjars_pik_numident %>%
  lazy_dt(key_by = "CJARS_ID") %>%
  mutate(cjars_st = str_sub(CJARS_ID, 1, 2)) %>%
  group_by(CJARS_ID) %>%
  filter((all(cjars_st != pobst | is.na(pobst)) | (cjars_st == pobst & !is.na(pobst)) | n() == 1)) %>%
  ungroup() %>%
  select(-pobst, -cjars_st) %>%
  as_tibble()

# Finally, keep CJARS to PIK matches where the PIK is the modal PIK.
join_cjars_pik_modal <-
  join_cjars_pik_samest %>%
  lazy_dt() %>%
  # At this point, each CJARS_ID has only 1 SRC_ST associated with it.
  count(CJARS_ID, SRC_ST, pik, wt = n, name = "nr_rows_with_this_pik") %>%
  group_by(CJARS_ID) %>%
  filter(nr_rows_with_this_pik == max(nr_rows_with_this_pik)) %>%
  ungroup() %>%
  select(-nr_rows_with_this_pik) %>%
  as_tibble()

join_cjars_pik_final <-
  join_cjars_pik_modal %>%
  lazy_dt() %>%
  group_by(CJARS_ID) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  as_tibble()

write_csv(
  join_cjars_pik_final,
  file.path("2_joined_cjars_pik.csv")
)

################################################################################\
cjars_ids_unmatched <-
  join_cjars_pik_modal %>%
  lazy_dt() %>%
  group_by(CJARS_ID) %>%
  filter(n() != 1) %>%
  ungroup() %>%
  as_tibble() %>%
  pull(CJARS_ID) %>%
  unique() %>%
  length()

nr_records_state_final <- table(join_cjars_pik_final$SRC_ST)
nr_pik_final <- length(unique(join_cjars_pik_final$pik))
nr_cjarsids_per_pik <-
  join_cjars_pik_final %>% lazy_dt() %>% count(pik) %>% as_tibble()
