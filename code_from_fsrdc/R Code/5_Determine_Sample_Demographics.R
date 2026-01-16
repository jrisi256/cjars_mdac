library(readr)
library(dplyr)
library(dtplyr)
library(lubridate)

output_dir <- ""
input_dir <- ""

################################################################################
# Read in data.
################################################################################
mdac_cjars <- read_csv(file.path(output_dir, "4_joined_mdac_cjars-subject.csv"))
mdac_demog_recode <- read_csv(file.path(input_dir, "mdac_demog_recode.csv"))

################################################################################
# Filter out those with unknown mortality status.
################################################################################
mdac_join <-
  mdac_cjars %>%
  lazy_dt() %>%
  left_join(mdac_demog_recode, by = c("CMID", "PNUM")) %>%
  filter(matchstat %in% c("alive", "dead")) %>%
  as_tibble()

alive_or_dead <- mdac_join %>% distinct(pik, .keep_all = T)

match_st_pob <-
  alive_or_dead %>%
  filter(!(POB %in% c("USA territory", "Outside USA")))

match_st_sod <- alive_or_dead %>% filter(matchstat == "dead")

match_sod_pob <-
  alive_or_dead %>%
  filter(matchstat == "dead" & !(POB %in% c("USA territory", "Outside USA")))

mdac_join <- mdac_join %>% select(-POB, -sod)

################################################################################
# Investigate individuals who have multiple CJARS IDs.
################################################################################
# How many CJARS IDs does each individual have?
count_cjars_id <- mdac_join %>% filter(!is.na(CJARS_ID)) %>% count(pik)
more_than_one_cjars_id <- count_cjars_id %>% filter(n > 1) %>% pull(pik)

# Count the number of unique states for each PIK.
nr_states_with_cj_contact <-
  mdac_join %>%
  lazy_dt() %>%
  filter(pik %in% more_than_one_cjars_id) %>%
  count(pik, SRC_ST) %>%
  add_count(pik, name = "nr_unique_states") %>%
  group_by(pik) %>%
  mutate(
    category =
      case_when(
        nr_unique_states == 1 ~ "One state, multiple CJARS IDs",
        nr_unique_states > 1 & all(n == 1) ~ "Multiple states, One CJARS ID per state",
        nr_unique_states > 1 & !(all(n == 1)) ~ "Multiple states, Multiple CJARS ID per state"
      )
  ) %>%
  ungroup() %>%
  as_tibble()

############################# Individuals with all their CJARS IDs in one state.
one_state_mult_cjars_id <-
  nr_states_with_cj_contact %>%
  filter(category == "One state, multiple CJARS IDs")

############################## Individuals with one CJARS ID in multiple states.
mult_state_one_cjars_id <-
  nr_states_with_cj_contact %>%
  filter(category == "Multiple states, One CJARS ID per state")

# See how many of the data issues come from _S states.
no_s_mult_state_one_cjars <-
  mult_state_one_cjars_id %>%
  filter(SRC_ST != "_S") %>%
  add_count(pik, name = 'nr_unique_states') %>%
  filter(nr_unique_states == 1)

######################## Individuals with multiple CJARS IDs in multiple states.
mult_state_mult_cjars_id <-
  nr_states_with_cj_contact %>%
  filter(category == "Multiple states, Multiple CJARS ID per state")

# See how many of the data issues come from _S states.
no_s_mult_state_mult_cjars <-
  mult_state_mult_cjars_id %>%
  filter(SRC_ST != "_S") %>%
  count(pik, SRC_ST) %>%
  add_count(pik, name = "nr_unique_states") %>%
  filter(nr_unique_states == 1)

################################################################################
# Compare how variables from CJARS match variables from MDAC.
################################################################################
######## Check agreement between CJARS and MDAC for individuals with 1 CJARS ID.
one_cjars_id <-
  mdac_join %>%
  lazy_dt() %>%
  filter(!is.na(SRC_ST)) %>%
  group_by(pik) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    state_match =
      case_when(
        SRC_ST == "_S" ~ "_S",
        SRC_ST == ST ~ "States match.",
        SRC_ST != ST ~ "States do not match."
      )
  ) %>%
  as_tibble()

####### Check agreement between CJARS and MDAC for individuals with >1 CJARS ID.
mult_cjars_id <-
  mdac_join %>%
  lazy_dt() %>%
  filter(pik %in% more_than_one_cjars_id) %>%
  group_by(pik) %>%
  mutate(
    state_match =
      case_when(
        any(SRC_ST == ST) ~ "At least one state matches.",
        !any(SRC_ST == ST) ~ "No state matches and no SRC_ST are missing."
      )
  ) %>%
  ungroup() %>%
  as_tibble()

mult_cjars_id_matches <-
  mult_cjars_id %>%
  select(pik, state_match, race_match, sex_match) %>%
  distinct(pik, .keep_all = T)

################################################################################
# Collapse individuals with CJARS IDs from the state into one record.
################################################################################
#' If an individual does not have a record in a table, it cannot be used to help
#' us disambiguate the ambiguous cases. If all contacts are not post-2015 and
#' the individual does not have any contacts pre-2015, then there are some
#' contacts with an unknown date. This person's pre-2015 contact status is
#' uncertain (maybe they had contact pre-2015 but also maybe they did not). If
#' all cases are post-2015 or the individual has at least one contact pre-2015,
#' then this person definitively had CJ contact pre-2015.
mdac_collapse <-
  mdac_join %>%
  lazy_dt() %>%
  group_by(pik, SRC_ST) %>%
  summarise(
    sum_arr = sum(nr_arr),
    sum_adj = sum(nr_adj),
    sum_inc = sum(nr_inc),
    sum_pro = sum(nr_pro),
    sum_par = sum(nr_par),
    sum_pre2015_arr = sum(nr_pre2015_arr),
    sum_pre2015_adj = sum(nr_pre2015_adj),
    sum_pre2015_inc = sum(nr_pre2015_inc),
    sum_pre2015_pro = sum(nr_pre2015_pro),
    sum_pre2015_par = sum(nr_pre2015_par),
    sum_post2015_arr = sum(nr_post2015_arr),
    sum_post2015_adj = sum(nr_post2015_adj),
    sum_post2015_inc = sum(nr_post2015_inc),
    sum_post2015_pro = sum(nr_post2015_pro),
    sum_post2015_par = sum(nr_post2015_par),
    has_cjars_id = any(!is.na(CJARS_ID)),
  ) %>%
  ungroup() %>%
  mutate(
    pre2015_arr_uncertain =
      case_when(
        sum_arr == 0 ~ NA,
        sum_post2015_arr != sum_arr & sum_pre2015_arr == 0 ~ T,
        sum_post2015_arr == sum_arr | sum_pre2015_arr >=1 ~ F
      ),
    pre2015_adj_uncertain =
      case_when(
        sum_adj == 0 ~ NA,
        sum_post2015_adj != sum_adj & sum_pre2015_adj == 0 ~ T,
        sum_post2015_adj == sum_adj | sum_pre2015_adj >=1 ~ F
      ),
    pre2015_inc_uncertain =
      case_when(
        sum_inc == 0 ~ NA,
        sum_post2015_inc != sum_inc & sum_pre2015_inc == 0 ~ T,
        sum_post2015_inc == sum_inc | sum_pre2015_inc >=1 ~ F
      ),
    pre2015_pro_uncertain =
      case_when(
        sum_pro == 0 ~ NA,
        sum_post2015_pro != sum_pro & sum_pre2015_pro == 0 ~ T,
        sum_post2015_pro == sum_pro | sum_pre2015_pro >=1 ~ F
      ),
    pre2015_par_uncertain =
      case_when(
        sum_par == 0 ~ NA,
        sum_post2015_par != sum_par & sum_pre2015_par == 0 ~ T,
        sum_post2015_par == sum_par | sum_pre2015_par >=1 ~ F
      ),
    all_post2015_arr = 
      case_when(
        sum_arr == 0 ~ NA,
        sum_post2015_arr == sum_arr ~ T,
        sum_post2015_arr != sum_arr ~ F
      ),
    all_post2015_adj = 
      case_when(
        sum_adj == 0 ~ NA,
        sum_post2015_adj == sum_adj ~ T,
        sum_post2015_adj != sum_adj ~ F
      ),
    all_post2015_inc = 
      case_when(
        sum_inc == 0 ~ NA,
        sum_post2015_inc == sum_inc ~ T,
        sum_post2015_inc != sum_inc ~ F
      ),
    all_post2015_pro = 
      case_when(
        sum_pro == 0 ~ NA,
        sum_post2015_pro == sum_pro ~ T,
        sum_post2015_pro != sum_pro ~ F
      ),
    all_post2015_par = 
      case_when(
        sum_par == 0 ~ NA,
        sum_post2015_par == sum_par ~ T,
        sum_post2015_par != sum_par ~ F
      ),
  ) %>%
  as_tibble()

################################################################################
# Count the number of instances of each type of CJ contact.
################################################################################
no_cjars_id <- mdac_collapse %>% filter(!has_cjars_id) %>% nrow()

no_record_has_cjars_id <-
  mdac_collapse %>%
  filter(
    has_cjars_id, sum_arr == 0, sum_adj == 0 , sum_inc == 0, sum_pro == 0,
    sum_par == 0
  ) %>%
  nrow()

pre2015 <-
  mdac_collapse %>%
  filter(
    sum_pre2015_arr >= 1 | sum_pre2015_adj >= 1 | sum_pre2015_inc >= 1 |
      sum_pre2015_pro >= 1 | sum_pre2015_par >= 1
  ) %>%
  nrow()

all_post2015 <-
  mdac_collapse %>%
  # keep all rows where all contacts from at least one table are post 2015.
  filter(
    all_post2015_arr | all_post2015_adj | all_post2015_inc | all_post2015_pro |
      all_post2015_par
  ) %>%
  # keep all rows where: 1) all contacts from each table are post 2015, and/or
  # 2) there's no recorded contact of that type (although not for all tables).
  filter(
    (all_post2015_arr == T | is.na(all_post2015_arr)) &
      (all_post2015_adj == T | is.na(all_post2015_adj)) &
      (all_post2015_inc == T | is.na(all_post2015_inc)) &
      (all_post2015_pro == T | is.na(all_post2015_pro)) &
      (all_post2015_par == T | is.na(all_post2015_par))
  ) %>%
  nrow()

pre2015_uncertain <-
  mdac_collapse %>%
  # Keep all rows where their status is uncertain for at least one table.
  filter(
    pre2015_arr_uncertain | pre2015_adj_uncertain | pre2015_inc_uncertain |
      pre2015_pro_uncertain | pre2015_par_uncertain
  ) %>%
  # Keep all rows: 1) whose status is uncertain across all tables, and/or
  # 2) who had no record in that table (although not for all tables), and/or
  # 3) had all contacts in that table post-2015 (although not for all tables).
  filter(
    (pre2015_arr_uncertain == T | is.na(pre2015_arr_uncertain) | all_post2015_arr == T) &
      (pre2015_adj_uncertain == T | is.na(pre2015_adj_uncertain) | all_post2015_adj == T) &
      (pre2015_inc_uncertain == T | is.na(pre2015_inc_uncertain) | all_post2015_inc == T) &
      (pre2015_pro_uncertain == T | is.na(pre2015_pro_uncertain) | all_post2015_pro == T) &
      (pre2015_par_uncertain == T | is.na(pre2015_par_uncertain) | all_post2015_par == T)
  )

################################################################################
# Construct final sample.
################################################################################
mdac_final <-
  mdac_collapse %>%
  select(
    pik, SRC_ST, sum_post2015_arr, sum_post2015_adj, sum_post2015_inc,
    sum_post2015_pro, sum_post2015_par, sum_pre2015_arr, sum_pre2015_adj,
    sum_pre2015_inc, sum_pre2015_pro, sum_pre2015_par, sum_arr, sum_adj, sum_inc,
    sum_pro, sum_par
  ) %>%
  left_join(
    select(
      mdac_join, pik, SRC_ST, CMID, PNUM, ST, SEX, ucause, cause113, dod, dob,
      matchstat, race_ethnicity_long, race_ethnicity_short, mdac_wgt, GQMAJTYP
    ),
    by = c("pik", "SRC_ST")
  ) %>%
  distinct(pik, SRC_ST, .keep_all = T) %>%
  mutate(
    age =
      if_else(
        is.na(dod),
        as.numeric((ymd("2015-12-31") - dob) / 365.25),
        as.numeric((dod - dob) / 365.25)
      ),
    matchstat = if_else(matchstat == "alive", 1, 0),
    age_bucket =
      case_when(
        round(age) <= 16 ~ "16 and under",
        round(age) >= 17 & round(age) <= 25 ~ "17 - 25",
        round(age) >= 26 & round(age) <= 35 ~ "26 - 35",
        round(age) >= 36 & round(age) <= 45 ~ "36 - 45",
        round(age) >= 46 & round(age) <= 55 ~ "46 - 55",
        round(age) >= 56 & round(age) <= 65 ~ "56 - 65",
        round(age) >= 66 & round(age) <= 75 ~ "66 - 75",
        round(age) >= 76 ~ "76 and over"
      )
  ) %>%
  as_tibble()

# Write out results
write_csv(
  mdac_final, file = file.path(output_dir, "5_mdac_cjars_demographics.csv")
)
