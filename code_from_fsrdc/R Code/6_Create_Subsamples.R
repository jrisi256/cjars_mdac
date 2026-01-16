library(readr)
library(dplyr)
library(dtplyr)
library(stringr)

focal_states <- c("FL", "MI", "NC", "TX", "WI")

################################################################################
# Read in data.
################################################################################
output_dir <- ""
mortality_dir <- ""

mdac_cjars <- read_csv(file.path(output_dir, "5_mdac_cjars_demographics.csv"))
crosswalk <-
  read_csv(
    file.path("cause113_ucause_crosswalk.csv"),
    col_types = cols(cause113 = "c")
  )
mdac_cjars <- left_join(mdac_cjars, crosswalk, by = "cause113")

# Does CJARS capture those who were living in a jail/prison group quarter?
group_quarter_check <-
  mdac_cjars %>%
  filter(GQMAJTYP == "Adult correctional facility") %>%
  mutate(
    status =
      case_when(
        is.na(SRC_ST) ~ "CJARS did not capture",
        !is.na(SRC_ST) & sum_arr == 0 & sum_adj == 0 & sum_inc == 0 &
          sum_pro == 0 & sum_par == 0 ~ "CJARS did not capture",
        !is.na(SRC_ST) ~ "CJARS successfully captured"
      )
  )

# Re-code underlying causes of death.
mdac_cjars <-
  mdac_cjars %>% 
  mutate(
    icd_code_alpha = str_sub(ucause, 1, 1),
    icd_code_num =
      if_else(
        str_sub(ucause, 2, 2) == "0",
        str_sub(ucause, 3, 3),
        str_sub(ucause, 2, 3)
      ),
    cause113_new = 
      case_when(
        (icd_code_alpha == "X" & icd_code_num %in% c(40:44, 60:64)) |
          (icd_code_alpha == "Y" & icd_code_num %in% 10:14) ~
          "Drug overdose",
        T ~ cause113_label
      ),
    cause113_new_condensed = 
      case_when(
        (icd_code_alpha == "X" & icd_code_num %in% c(40:44, 60:64)) |
          (icd_code_alpha == "Y" & icd_code_num %in% 10:14) ~
          "Drug overdose",
        T ~ cause113_condensed
      )
  ) %>%
  select(-icd_code_alpha, -icd_code_num)

################################################################################
# Generate different samples.
################################################################################
################################################################################
# Count individuals w/ a CJARS ID but no records as not having had CJ contact.
################################################################################
cjars_id_no_cj_contact <-
  mdac_cjars %>%
  lazy_dt() %>%
  mutate(
    # Those w/ a CJARS ID but no records do not count as having had CJ contact.
    cj_pre2015_contact =
      (
        sum_pre2015_arr > 0 | sum_pre2015_adj > 0 | sum_pre2015_inc > 0 |
          sum_pre2015_pro > 0 | sum_pre2015_par > 0
      ) | 
      GQMAJTYP == "Adult correctional facility",
    cj_pre2015contact_in_focal_state = SRC_ST %in% focal_states & cj_pre2015_contact
  ) %>%
  select(-SRC_ST) %>%
  group_by(pik) %>%
  mutate(
    across(matches("sum"), function(col) {sum(col)}),
    cj_pre2015contact_in_focal_state = any(cj_pre2015contact_in_focal_state),
    cj_pre2015_contact = any(cj_pre2015_contact)
  ) %>%
  ungroup() %>%
  distinct(pik, .keep_all = T) %>%
  # Drop individuals whose pre/during 2015 CJ status is uncertain.
  # If an individual did not have a pre/during 2015 CJ contact and they have at
  # least one date-uncertain interaction, then their status is uncertain.
  filter(
    cj_pre2015_contact |
      (
        sum_post2015_arr == sum_arr & sum_post2015_adj == sum_adj &
          sum_post2015_inc == sum_inc & sum_post2015_pro == sum_pro &
          sum_post2015_par == sum_par
      )
  ) %>%
  as_tibble()

cjars_id_no_cj_live_focal_states <-
  cjars_id_no_cj_contact %>%
  filter(ST %in% focal_states)

cjars_id_no_cj_live_or_contact_focal_states <-
  cjars_id_no_cj_contact %>%
  filter(ST %in% focal_states | cj_pre2015contact_in_focal_state)

################################################################################
# Count those w/ CJARS ID but no records as having had pre/during-2015 contact.
################################################################################
cjars_id_cj_contact <-
  mdac_cjars %>%
  lazy_dt() %>%
  mutate(
    # Count individuals w/ a CJARS ID but no records as having had CJ contact.
    cj_pre2015_contact =
      (
        sum_pre2015_arr > 0 | sum_pre2015_adj > 0 | sum_pre2015_inc > 0 |
          sum_pre2015_pro > 0 | sum_pre2015_par > 0
      ) |
      (
        sum_arr == 0 & sum_adj == 0 & sum_inc == 0 & sum_pro == 0 &
          sum_par == 0 & !is.na(SRC_ST)
      ) |
      GQMAJTYP == "Adult correctional facility",
    cj_pre2015contact_in_focal_state = SRC_ST %in% focal_states & cj_pre2015_contact
  ) %>%
  group_by(pik) %>%
  mutate(
    across(matches("sum"), function(col) {sum(col)}),
    cj_pre2015contact_in_focal_state = any(cj_pre2015contact_in_focal_state),
    cj_pre2015_contact = any(cj_pre2015_contact)
  ) %>%
  ungroup() %>%
  distinct(pik, .keep_all =  T) %>%
  select(-SRC_ST) %>%
  # Drop individuals whose pre/during 2015 CJ status is uncertain.
  # If an individual did not have a pre/during 2015 CJ contact and they have at
  # least one date-uncertain interaction, then their status is uncertain.
  filter(
    cj_pre2015_contact |
      (
        sum_post2015_arr == sum_arr & sum_post2015_adj == sum_adj &
          sum_post2015_inc == sum_inc & sum_post2015_pro == sum_pro &
          sum_post2015_par == sum_par
      )
  ) %>%
  as_tibble()

cjars_id_cj_live_focal_states <-
  cjars_id_cj_contact %>%
  filter(ST %in% focal_states)

cjars_id_cj_live_or_contact_focal_states <-
  cjars_id_cj_contact %>%
  filter(ST %in% focal_states | cj_pre2015contact_in_focal_state)

################################################################################
# Drop those w/ CJARS ID but no records.
################################################################################
main_sample <-
  mdac_cjars %>%
  lazy_dt() %>%
  mutate(
    cj_pre2015_contact =
      (
        sum_pre2015_arr > 0 | sum_pre2015_adj > 0 | sum_pre2015_inc > 0 |
          sum_pre2015_pro > 0 | sum_pre2015_par > 0
      ) |
      GQMAJTYP == "Adult correctional facility",
    cj_pre2015contact_in_focal_state = SRC_ST %in% focal_states & cj_pre2015_contact
  ) %>%
  filter(
    (is.na(SRC_ST) | sum_arr > 0 | sum_adj > 0 | sum_inc > 0 | sum_pro > 0 |
      sum_par > 0 | GQMAJTYP == "Adult correctional facility")
  ) %>%
  group_by(pik) %>%
  mutate(
    across(matches("sum"), function(col) {sum(col)}),
    cj_pre2015contact_in_focal_state = any(cj_pre2015contact_in_focal_state),
    cj_pre2015_contact = any(cj_pre2015_contact)
  ) %>%
  ungroup() %>%
  distinct(pik, .keep_all =  T) %>%
  select(-SRC_ST) %>%
  # Drop individuals whose pre/during 2015 CJ status is uncertain.
  # If an individual did not have a pre/during 2015 CJ contact and they have at
  # least one date-uncertain interaction, then their status is uncertain.
  filter(
    cj_pre2015_contact |
      (
        sum_post2015_arr == sum_arr & sum_post2015_adj == sum_adj &
          sum_post2015_inc == sum_inc & sum_post2015_pro == sum_pro &
          sum_post2015_par == sum_par
      )
  ) %>%
  as_tibble()

main_sample_live_focal_states <-
  main_sample %>%
  filter(ST %in% focal_states)

main_sample_live_or_contact_focal_states <-
  main_sample %>%
  filter(ST %in% focal_states | cj_pre2015contact_in_focal_state)

################################################################################
# Save results.
################################################################################
write_csv(cjars_id_no_cj_contact, file.path(mortality_dir, "6_cjars_id_no_contact_sample.csv"))
write_csv(cjars_id_cj_contact, file.path(mortality_dir, "6_cjars_id_contact_sample.csv"))
write_csv(main_sample, file.path(mortality_dir, "6_main_mortality_sample.csv"))
