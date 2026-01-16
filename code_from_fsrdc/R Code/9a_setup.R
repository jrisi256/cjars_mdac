library(readr)
library(dplyr)
library(haven)
library(tidyr)
library(purrr)
library(dtplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(lubridate)
library(data.table)

################################################################################
# Read in data.
################################################################################
focal_states <- c("FL", "MI", "NC", "TX", "WI")
data_dir <- ""
cjars_dir <- ""

# PIK to CJARS ID crosswalk, cleaned.
cjars_roster_unique <-
  fread(
    file.path(data_dir, "2_joined_cjars_pik.csv"),
    colClasses = c(pik = "character")
  ) %>%
  as_tibble() %>%
  lazy_dt(key_by = "pik")

# Full MDAC roster, cleaned.
mdac_roster <- read_csv(file.path(data_dir, "1_joined_mdac_pik.csv"))

# Main analytic sample of individuals with CJ contact from MDAC.
mdac_cj_sample <-
  read_csv(file.path(dat_dir, "6_main_mortality_sample.csv")) %>%
  lazy_dt() %>%
  filter(ST %in% focal_states, cj_pre2015_contact) %>%
  select(pik) %>%
  inner_join(cjars_roster_unique, by = "pik") %>%
  select(pik, CJARS_ID, SRC_ST) %>%
  as_tibble() %>%
  lazy_dt(key_by = "CJARS_ID")

# Individuals w/ CJ contact in MDAC who live in focal state and/or had CJ contact in focal state.
mdac_cj_sample_alt <-
  read_csv(file.path(data_dir, "6_main_mortality_sample.csv")) %>%
  lazy_dt() %>%
  filter(
    (ST %in% focal_states & cj_pre2015_contact) | cj_pre2015contact_in_focal_state
  ) %>%
  select(pik) %>%
  inner_join(cjars_roster_unique, by = "pik") %>%
  select(pik, CJARS_ID, SRC_ST) %>%
  as_tibble() %>%
  lazy_dt(key_by = "CJARS_ID")

# Arrests
arrests <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_arrest_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "ARR_ID", "ARR_ARR_DT_YYYY", "ARR_OFF_CD", "ARR_ST_ORI_FIPS",
        "ARR_CNTY_ORI_FIPS"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Adjudications
adj <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_adjud_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "ADJ_ID", "ADJ_GRD_CD", "ADJ_OFF_LGL_CD", "ADJ_CHRG_OFF_CD",
        "ADJ_DISP_CD", "ADJ_DISP_OFF_CD", "ADJ_SENT_SERV", "ADJ_SENT_DTH",
        "ADJ_SENT_INC", "ADJ_SENT_PRO", "ADJ_SENT_REST", "ADJ_SENT_SUS",
        "ADJ_SENT_TRT", "ADJ_SENT_FINE", "ADJ_SENT_INC_MIN", "ADJ_SENT_INC_MAX",
        "ADJ_ST_ORI_FIPS", "ADJ_CNTY_ORI_FIPS", "ADJ_DISP_DT_YYYY"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Incarceration
incar <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_incar_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "INC_ID", "INC_ENTRY_DT_YYYY", "INC_ENTRY_DT_MM", "INC_ENTRY_DT_DD",
        "INC_EXIT_DT_YYYY", "INC_EXIT_DT_MM", "INC_EXIT_DT_DD", "INC_FCL_CD",
        "INC_ENTRY_CD", "INC_EXIT_CD", "INC_ST_ORI_FIPS", "INC_CNTY_ORI_FIPS"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Probation
probat <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_probat_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "PRO_ID", "PRO_BGN_DT_YYYY", "PRO_BGN_DT_MM", "PRO_BGN_DT_DD",
        "PRO_END_DT_YYYY", "PRO_END_DT_MM", "PRO_END_DT_DD", "PRO_COND_CD",
        "PRO_END_CD", "PRO_ST_ORI_FIPS", "PRO_CNTY_ORI_FIPS"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Parole
parole <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_parole_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "PAR_ID", "PAR_END_CD", "PAR_BGN_DT_YYYY", "PAR_BGN_DT_MM",
        "PAR_BGN_DT_DD", "PAR_END_DT_YYYY", "PAR_END_DT_MM", "PAR_END_DT_DD",
        "PAR_ST_ORI_FIPS", "PAR_CNTY_ORI_FIPS"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

################################################################################
# Find all individuals in CJARS who were not in MDAC and had CJ contact in at
# least one focal state.
################################################################################
cjars_focalstates_nomdac <-
  cjars_roster_unique %>%
  filter(!(pik %in% mdac_roster$pik)) %>%
  group_by(pik) %>%
  filter(any(SRC_ST %in% focal_states)) %>%
  ungroup() %>%
  as_tibble() %>%
  lazy_dt(key_by = "CJARS_ID")

################################################################################
# Join CJ contact tables to non-MDAC CJARS and MDAC CJARS.
################################################################################
join_sample_cjsubject <- function(sample, cjars_subject) {
  df <-
    sample %>%
    inner_join(cjars_subject, by = "CJARS_ID") %>%
    as_tibble()
  
  sample <- sample %>% as_tibble()
  
  nr_pik_sample <- length(unique(sample$pik))
  nr_pik_cjsubject <- length(unique(df$pik))
  
  return(
    list(
      df = df,
      nr_pik_sample = nr_pik_sample,
      nr_pik_cjsubject = nr_pik_cjsubject,
      prop_in_cjsubject = nr_pik_cjsubject / nr_pik_sample
    )
  )
}

joined_samples_cjsubject <-
  pmap(
    list(
      list(
        cjars_focalstates_nomdac, mdac_cj_sample, mdac_cj_sample_alt,
        cjars_focalstates_nomdac, mdac_cj_sample, mdac_cj_sample_alt,
        cjars_focalstates_nomdac, mdac_cj_sample, mdac_cj_sample_alt,
        cjars_focalstates_nomdac, mdac_cj_sample, mdac_cj_sample_alt,
        cjars_focalstates_nomdac, mdac_cj_sample, mdac_cj_sample_alt
      ),
      list(
        parole, parole, parole,
        probat, probat, probat,
        arrests, arrests, arrests,
        incar, incar, incar,
        adj, adj, adj
      )
    ),
    join_sample_cjsubject
  )
names(joined_samples_cjsubject) <-
  c(
    paste0(c("nomdac", "mdac", "mdac_alt"), "_parole"),
    paste0(c("nomdac", "mdac", "mdac_alt"), "_probat"),
    paste0(c("nomdac", "mdac", "mdac_alt"), "_arr"),
    paste0(c("nomdac", "mdac", "mdac_alt"), "_incar"),
    paste0(c("nomdac", "mdac", "mdac_alt"), "_adj")
  )
saveRDS(
  joined_samples_cjsubject,
  file.path("joined_samples_cjsubject.rds")
)

################################################################################
# Functions for comparing MDAC-CJARS to non-MDAC CJARS.
################################################################################
conduct_prop_test <- function(df, sample1, sample2, column, n_column) {
  prop.test(
    x =
      c(
        df %>% filter(sample == sample1) %>% select(all_of(column)) %>% pull(),
        df %>% filter(sample == sample2) %>% select(all_of(column)) %>% pull()
      ),
    n =
      c(
        df %>% filter(sample == sample1) %>% select(all_of(n_column)) %>% pull(),
        df %>% filter(sample == sample2) %>% select(all_of(n_column)) %>% pull()
      ),
    correct = F
  )
}

find_length <- function(df, sample, d_bcol, m_bcol, y_bcol, d_ecol, m_ecol, y_ecol) {
  df %>%
    lazy_dt() %>%
    mutate(
      begin_date =
        case_when(
          is.na(.data[[y_bcol]]) ~ NA_Date_,
          is.na(.data[[m_bcol]]) ~ ymd(paste0(.data[[y_bcol]], "-01", "-01")),
          is.na(.data[[d_bcol]]) ~ ymd(paste0(.data[[y_bcol]], "-", .data[[m_bcol]], "-01")),
          !is.na(.data[[y_bcol]]) & !is.na(.data[[m_bcol]]) & !is.na(.data[[d_bcol]]) ~
            ymd(paste0(.data[[y_bcol]], "-", .data[[m_bcol]], "-", .data[[d_bcol]]))
        ),
      end_date =
        case_when(
          is.na(.data[[y_ecol]]) ~ NA_Date_,
          is.na(.data[[m_ecol]]) ~ ymd(paste0(.data[[y_ecol]], "-01", "-01")),
          is.na(.data[[d_ecol]]) ~ ymd(paste0(.data[[y_ecol]], "-", .data[[m_ecol]], "-01")),
          !is.na(.data[[y_ecol]]) & !is.na(.data[[m_ecol]]) & !is.na(.data[[d_ecol]]) ~
            ymd(paste0(.data[[y_ecol]], "-", .data[[m_ecol]], "-", .data[[d_ecol]]))
        ),
      length = time_length(end_date - begin_date, "year"),
      status =
        case_when(
          !is.na(begin_date) & is.na(end_date) ~ "ongoing",
          is.na(begin_date) ~ "missing",
          length < 0 ~ "error",
          length >= 0 ~ "finished"
        )
    ) %>%
    select(-matches("BGN_DT|END_DT")) %>%
    mutate(sample = sample) %>%
    as_tibble()
}

find_nr <- function(df, sample) {
  df %>%
    lazy_dt() %>%
    count(pik, status) %>%
    pivot_wider(
      id_cols = "pik",
      names_from = "status",
      values_from = "n",
      values_fill = 0
    ) %>%
    group_by(pik) %>%
    mutate(total = sum(finished, error, ongoing, missing)) %>%
    ungroup() %>%
    mutate(sample = sample) %>%
    as_tibble()
}
