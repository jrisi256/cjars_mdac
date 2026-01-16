library(readr)
library(dplyr)
library(lubridate)

################################################################################
# Read in MDAC and Numident samples.
################################################################################
focal_states <- c("FL", "MI", "NC", "TX", "WI")
mdac_dir <- ""
numident_dir <- ""

mdac_sample <-
  read_csv(
    file.path(mdac_dir, "6_main_mortality_sample.csv"),
    col_select = c("race_ethnicity_short", "age_bucket", "pik", "ST", "cj_pre2015_contact")
  ) %>%
  mutate(
    race = 
      case_when(
        race_ethnicity_short == "White alone, not Hispanic" ~ "White",
        race_ethnicity_short == "Hispanic" ~ "Hispanic",
        race_ethnicity_short == "Black alone, not Hispanic" ~ "Black",
        T ~ race_ethnicity_short
      )
  ) %>%
  filter(
    race %in% c("White", "Black", "Hispanic"),
    age_bucket != "16 and under",
    ST %in% focal_states
  ) %>%
  select(pik, cj_pre2015_contact) %>%
  rename(cj_contact_mdac = cj_pre2015_contact)

numident_sample <-
  read_csv(
    file.path(numident_dir, "8_mdac_wide_focal_states.csv"),
    col_select = c("pik", "dob", "dod", "race_short", "nr_cjars_ids")
  ) %>%
  mutate(
    age =
      if_else(
        is.na(dod),
        as.numeric((ymd("2024-03-01") - dob) / 365.25),
        as.numeric((dod - dob) / 365.25)
      ),
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
      ),
    race =
      case_when(
        race_short == "white_nh" ~ "white",
        race_short == "black_nh" ~ "black",
        race_short == "hispanic" ~ "hisp",
        T ~ "other"
      ),
    cj_contact_numident = if_else(nr_cjars_ids > 0, T, F)
  ) %>%
  filter(age_bucket != "16 and under", race != "other") %>%
  select(pik, cj_contact_numident)

################################################################################
# Compare overlap in samples.
################################################################################
total_nr_unique_individuals <-
  bind_rows(mdac_sample, numident_sample) %>%
  pull(pik) %>%
  unique() %>%
  length()

in_both <- inner_join(mdac_sample, numident_sample, by = "pik")
only_in_mdac <- anti_join(mdac_sample, numident_sample, by = "pik")
only_in_numident <- anti_join(numident_sample, mdac_sample, by = "pik")

################################################################################
# Compare overlap in JIIs and non-JIIs.
################################################################################
jii_non_jii_overlap <-
  full_join(mdac_sample, numident_sample, by = "pik") %>%
  mutate(cj_status = paste0(cj_contact_mdac, "_", cj_contact_numident))
  