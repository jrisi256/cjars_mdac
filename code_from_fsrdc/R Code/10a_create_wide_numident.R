library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(dtplyr)
library(ggplot2)
library(lubridate)

focal_states <- c("FL", "MI", "NC", "TX", "WI")

# Check if a date is missing, complete, or missing some component (e.g., day).
check_date_incomplete_status_num <- function(year, month, day, date) {
  case_when(
    # Invalid date (e.g.,February 30th).
    year != "" & month != "" & day != "" & is.na(date) ~ "invalid_date",
    # No date components are missing.
    year != "" & month != "" & day != "" ~ "nm_nm_nm",
    # Year is missing.
    year == "" & month != "" & day != "" ~ "m_nm_nm",
    # Year and month are missing.
    year == "" & month == "" & day != "" ~ "m_m_nm",
    # Year and day are missing.
    year == "" & month != "" & day == "" ~ "m_nm_m",
    # Month and day are missing.
    year != "" & month == "" & day == "" ~ "nm_m_m",
    # Day is missing.
    year != "" & month != "" & day == "" ~ "nm_nm_m",
    # Month is missing.
    year != "" & month == "" & day != "" ~ "nm_m_nm",
    # All date components missing.
    year == "" & month == "" & day == "" ~ "m_m_m"
  )
}

# Check if an individual died before they were born.
check_dob_dod_error_num <- function(bday_s, dob, by, bm, dday_s, dod, dy, dm) {
  case_when(
    # If the person has not died, then they cannot be dead before they were born.
    dday_s == "m_m_m" ~ F,
    # When both dates are complete, just check if they died before they were born.
    bday_s == "nm_nm_nm" & dday_s == "nm_nm_nm" ~ dod < dob,
    # When at least one date only has the year, compare years only.
    bday_s %in% c("nm_m_m", "nm_m_nm") | dday_s %in% c("nm_m_m", "nm_m_nm") ~ dy < by,
    # When one date only has the year + month (and the other date also only has
    # year + month or is complete), compare year + month only.
    bday_s == "nm_nm_m" | dday_s == "nm_nm_m" ~ ym(paste0(dy, "-", dm)) < ym(paste0(by, "-", bm))
  )
}

################################################################################
# Read in Numident file and remove problematic individuals.
################################################################################
numident_focal <-
  read_sas(
    file.path("cnum_2024q1.sas7bdat"),
    col_select =
      c(
        "pik", "dobcc", "dobyy", "dobmm", "dobdd", "dodcc", "dodyy", "dodmm",
        "doddd", "pobst", "gender", "bestrace", "citizen", "alien", "pobfin"
      )
  ) %>%
  lazy_dt(key_by = "pobst") %>%
  filter(pobst %in% focal_states) %>%
  as_tibble()

# Remove individuals with bad dates from our sample.
numident_focal_clean <-
  numident_focal %>%
  lazy_dt() %>%
  # Drop individuals who are missing their year of birth.
  filter(dobcc != "" & dobyy != "") %>%
  # Keep individuals who are not dead or who are dead and have their full death year.
  filter((dodcc == "" & dodyy == "") | (dodcc != "" & dodyy != "")) %>%
  mutate(
    birth_year = paste0(dobcc, dobyy),
    death_year = paste0(dodcc, dodyy),
    dob = ymd(paste0(birth_year, "-", dobmm, "-", dobdd)),
    dod = ymd(paste0(death_year, "-", dodmm, "-", doddd)),
    birth_date_status = check_date_incomplete_status_num(birth_year, dobmm, dobdd, dob),
    death_date_status = check_date_incomplete_status_num(death_year, dodmm, doddd, dod)
  ) %>%
  # Drop individuals who were born or who died on invalid dates.
  filter(birth_date_status != "invalid_date" & death_date_status != "invalid_date") %>%
  mutate(
    died_before_born =
      check_dob_dod_error_num(
        birth_date_status, dob, birth_year, dobmm, death_date_status, dod, death_year, dodmm
      )
  ) %>%
  # Drop individuals who died before they were born.
  filter(!died_before_born) %>%
  # Create citizenship flag
  mutate(citizenship_flag = (citizen != "A" & citizen != "") | alien != "0" | pobfin == "*") %>%
  select(-dobcc, -dobyy, -dodcc, -dodyy, -died_before_born, -citizen, -alien, -pobfin) %>%
  as_tibble()

################################################################################
# Read in the CJARS-PIK crosswalk file. Combine w/ Numident sample to id JIIs.
################################################################################
cjars_pik <-
  read_csv("2_joined_cjars_pik.csv") %>%
  lazy_dt(key_by = "pik")

numident_cjars <-
  numident_focal_clean %>%
  lazy_dt(key_by = "pik") %>%
  left_join(cjars_pik, by = "pik") %>%
  as_tibble()

numident_cjars <- numident_cjars %>% lazy_dt(key_by = "CJARS_ID")

################################################################################
# Read in the CJARS subject tables.
################################################################################
cjars_dir <- ""

# Arrests
arrests <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_arrest_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "ARR_ID", "ARR_OFF_CD",
        "ARR_ARR_DT_DD", "ARR_ARR_DT_MM", "ARR_ARR_DT_YYYY", ,
        "ARR_BOOK_DT_YYYY", "ARR_BOOK_DT_MM", "ARR_BOOK_DT_DD"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Adjudications
adj <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_adjud_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "ADJ_ID", "ADJ_GRD_CD", "ADJ_CHRG_OFF_CD", "ADJ_DISP_CD",
        "ADJ_DISP_OFF_CD",
        "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD",
        "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD",
        "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Probation
pro <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_probat_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "PRO_ID", "PRO_END_CD",
        "PRO_BGN_DT_YYYY", "PRO_BGN_DT_MM", "PRO_BGN_DT_DD",
        "PRO_END_DT_YYYY", "PRO_END_DT_MM", "PRO_END_DT_DD"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Incarceration
incar <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_incar_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "INC_ID", "INC_FCL_CD",
        "INC_ENTRY_DT_YYYY", "INC_ENTRY_DT_MM", "INC_ENTRY_DT_DD",
        "INC_EXIT_DT_YYYY", "INC_EXIT_DT_MM", "INC_EXIT_DT_DD"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

# Parole
parole <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_parole_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "PAR_ID", "PAR_END_CD",
        "PAR_BGN_DT_YYYY", "PAR_BGN_DT_MM", "PAR_BGN_DT_DD",
        "PAR_END_DT_YYYY", "PAR_END_DT_MM", "PAR_END_DT_DD"
      )
  ) %>%
  lazy_dt(key_by = "CJARS_ID")

################################################################################
# Clean CJARS data and merge it with Numident.
################################################################################
# Check the status of a CJARS subject table date.
check_date_incomplete_status <- function(year, month, day, date) {
  case_when(
    # Invalid date (e.g.,February 30th).
    !is.na(year) & !is.na(month) & !is.na(day) & is.na(date) ~ "missing",
    # No date components are missing.
    !is.na(year) & !is.na(month) & !is.na(day) ~ "nm_nm_nm",
    # If year is missing, consider the date missing.
    is.na(year) ~ "missing",
    # Month and day are missing.
    is.na(month) & is.na(day) ~ "nm_m_m",
    # Day is missing.
    is.na(day) ~ "nm_nm_m",
    # Month is missing.
    is.na(month) ~ "nm_m_nm"
  )
}

# Check if an individual had a CJ contact before they were born.
check_dob_error <- function(cj_date_s, cj_date, cjy, cjm, b_date_s, b_date, by, bm) {
  case_when(
    # Cannot have a dob error if the CJ date is missing.
    cj_date_s == "missing" ~ F,
    # If the dates did not fail to parse, compare dates normally.
    cj_date_s == "nm_nm_nm" & b_date_s == "nm_nm_nm" ~ cj_date < b_date,
    # When at least one date only has the year, compare years only.
    cj_date_s %in% c("nm_m_m", "nm_m_nm") | b_date_s %in% c("nm_m_m", "nm_m_nm") ~ cjy < by,
    # When one date only has the year + month (and the other date also only has
    # year + month or is complete), compare year + month only.
    cj_date_s == "nm_nm_m" | b_date_s == "nm_nm_m" ~ ym(paste0(cjy, "-", cjm)) < ym(paste0(by, "-", bm))
  )
}

# Check if an individual had a CJ contact before they were born.
check_dod_error <- function(cj_date_s, cj_date, cjy, cjm, d_date_s, d_date, dy, dm) {
  case_when(
    # Cannot have a dod error if the CJ date or death date is missing.
    cj_date_s == "missing" | d_date_s == "m_m_m" ~ F,
    # If the dates did not fail to parse, compare dates normally.
    cj_date_s == "nm_nm_nm" & d_date_s == "nm_nm_nm" ~ cj_date > d_date,
    # When at least one date only has the year, compare years only.
    cj_date_s %in% c("nm_m_m", "nm_m_nm") | d_date_s %in% c("nm_m_m", "nm_m_nm") ~ cjy > dy,
    # When one date only has the year + month (and the other date also only has
    # year + month or is complete), compare year + month only.
    cj_date_s == "nm_nm_m" | d_date_s == "nm_nm_m" ~ ym(paste0(cjy, "-", cjm)) > ym(paste0(dy, "-", dm))
  )
}

create_clean_cjars_subject_dates <- function(df, df2, y1, m1, d1, y2, m2, d2, y3, m3, d3) {
  df_new <-
    df %>%
    inner_join(select(df2, -dobdd, -doddd, -gender, -bestrace, -citizenship_flag), by = "CJARS_ID") %>%
    mutate(
      date1 = ymd(paste0(.data[[y1]], "-", .data[[m1]], "-", .data[[d1]])),
      date2 = ymd(paste0(.data[[y2]], "-", .data[[m2]], "-", .data[[d2]])),
      date1_status = check_date_incomplete_status(.data[[y1]], .data[[m1]], .data[[d1]], date1),
      date2_status = check_date_incomplete_status(.data[[y2]], .data[[m2]], .data[[d2]], date2),
      date1_dob_error =
        check_dob_error(
          date1_status, date1, .data[[y1]], .data[[m1]], birth_date_status, dob, birth_year, dobmm
        ),
      date2_dob_error =
        check_dob_error(
          date2_status, date2, .data[[y2]], .data[[m2]], birth_date_status, dob, birth_year, dobmm
        ),
      date1_dod_error =
        check_dod_error(
          date1_status, date1, .data[[y1]], .data[[m1]], death_date_status, dod, death_year, dodmm
        ),
      date2_dod_error =
        check_dod_error(
          date2_status, date2, .data[[y2]], .data[[m2]], death_date_status, dod, death_year, dodmm
        )
    )

  # No 3rd date column.
  if(is.na(y3) | is.na(y2) | is.na(y1)) {
    df_new <-
      df_new %>%
      mutate(
        # Date has a DOB/DOD error if any dates are in error.
        dob_error = if_else(date1_dob_error | date2_dob_error, T, F),
        dod_error = if_else(date1_dod_error | date2_dod_error, T, F),
        # Date is considered truly missing if all dates are missing.
        missing_date = date1_status == "missing" & date2_status == "missing"
      ) %>%
      as_tibble()
  } else {
    df_new <-
      df_new %>%
      mutate(
        date3 = ymd(paste0(.data[[y3]], "-", .data[[m3]], "-", .data[[d3]])),
        date3_status = check_date_incomplete_status(.data[[y3]], .data[[m3]], .data[[d3]], date3),
        date3_dob_error =
          check_dob_error(
            date3_status, date3, .data[[y3]], .data[[m3]], birth_date_status, dob, birth_year, dobmm
          ),
        date3_dod_error =
          check_dod_error(
            date3_status, date3, .data[[y3]], .data[[m3]], death_date_status, dod, death_year, dodmm
          ),
        dob_error = if_else(date1_dob_error | date2_dob_error | date3_dob_error, T, F),
        dod_error = if_else(date1_dod_error | date2_dod_error | date3_dod_error, T, F),
        missing_date = date1_status == "missing" & date2_status == "missing" & date3_status == "missing"
      ) %>%
      as_tibble()
  }
}

# Merge with Numident and check for dob, dod, and missing date errors.
clean_cjars_subject_tables <-
  pmap(
    list(
      df = list(arrests, adj, pro, incar, parole),
      y1 = list("ARR_ARR_DT_YYYY", "ADJ_FILE_DT_YYYY", "PRO_BGN_DT_YYYY", "INC_ENTRY_DT_YYYY", "PAR_BGN_DT_YYYY"),
      m1 = list("ARR_ARR_DT_MM", "ADJ_FILE_DT_MM", "PRO_BGN_DT_MM", "INC_ENTRY_DT_MM", "PAR_BGN_DT_MM"),
      d1 = list("ARR_ARR_DT_DD", "ADJ_FILE_DT_DD", "PRO_BGN_DT_DD", "INC_ENTRY_DT_DD", "PAR_BGN_DT_DD"),
      y2 = list("ARR_BOOK_DT_YYYY", "ADJ_DISP_DT_YYYY", "PRO_END_DT_YYYY", "INC_EXIT_DT_YYYY", "PAR_END_DT_YYYY"),
      m2 = list("ARR_BOOK_DT_MM", "ADJ_DISP_DT_MM", "PRO_END_DT_MM", "INC_EXIT_DT_MM", "PAR_END_DT_MM"),
      d2 = list("ARR_BOOK_DT_DD", "ADJ_DISP_DT_DD", "PRO_END_DT_DD", "INC_EXIT_DT_DD", "PAR_END_DT_DD"),
      y3 = list(NA, "ADJ_SENT_DT_YYYY", NA, NA, NA),
      m3 = list(NA, "ADJ_SENT_DT_MM", NA, NA, NA),
      d3 = list(NA, "ADJ_SENT_DT_DD", NA, NA, NA)
    ),
    create_clean_cjars_subject_dates,
    df2 = numident_cjars
  )
names(clean_cjars_subject_tables) <- c("arr", "adj", "pro", "incar", "par")

# Check length of time tables for length of time specific date errors.
check_length_of_time_errors <- function(df) {
  df %>%
    mutate(
      # Is the event missing either its start or exit date (but not both)?
      missing_entry_or_exit_not_both =
        if_else(
          (date1_status != "missing" & date2_status != "missing") | (date1_status == "missing" & date2_status == "missing"),
          F,
          T
        ),
      # Is the exit date before the entry date?
      dates_out_of_order =
        case_when(
          is.na(date2) | is.na(date1) ~ F,
          date2 >= date1 ~ F,
          date2 < date1 ~ T
        )
    )
}

length_of_time_errors <-
  map(
    list(
      "pro" = clean_cjars_subject_tables$pro,
      "incar" = clean_cjars_subject_tables$incar,
      "par" = clean_cjars_subject_tables$par
    ),
    check_length_of_time_errors
  )

clean_cjars_subject_tables$pro <- length_of_time_errors$pro
clean_cjars_subject_tables$incar <- length_of_time_errors$incar
clean_cjars_subject_tables$par <- length_of_time_errors$par

# Count number of errors per individual.
individual_errors <-
  pmap(
    list(clean_cjars_subject_tables, names(clean_cjars_subject_tables)),
    function(df, name) {
      if(name %in% c("incar", "pro", "par")) {
        df <-
          df %>%
          lazy_dt(key_by = "pik") %>%
          group_by(pik) %>%
          summarise(
            nr_missing = sum(missing_date),
            nr_dob_errors = sum(dob_error),
            nr_dod_errors = sum(dod_error),
            nrMissing_entryOrExit = sum(missing_entry_or_exit_not_both),
            nr_dates_out_of_order = sum(dates_out_of_order)
          ) %>%
          ungroup() %>%
          as_tibble()
      } else if(name %in% c("arr", "adj")) {
        df <-
          df %>%
          lazy_dt(key_by = "pik") %>%
          group_by(pik) %>%
          summarise(
            nr_missing = sum(missing_date),
            nr_dob_errors = sum(dob_error),
            nr_dod_errors = sum(dod_error)
          ) %>%
          ungroup() %>%
          as_tibble()
      }
    }
  )
names(individual_errors) <- names(clean_cjars_subject_tables)

################################################################################
# Drop problematic CJARS entries.
################################################################################
removed_entries_cjars_subject_tables <-
  pmap(
    list(clean_cjars_subject_tables, names(clean_cjars_subject_tables)),
    function(df, name) {
      df <- df %>% filter(!dob_error & !dod_error & !missing_date)
      
      if(name %in% c("incar", "pro", "par")) {
        df <- df %>% filter(!missing_entry_or_exit_not_both & !dates_out_of_order)
      }
      
      return(df)
    }
  )
names(removed_entries_cjars_subject_tables) <- names(clean_cjars_subject_tables)

################################################################################
# Find age of earliest CJ contact.
################################################################################
meta_dir <- ""

find_earliest_contact <- function(df, y1, y2, y3, name) {
  if(is.na(y3)) {
    df %>%
      lazy_dt() %>%
      mutate(
        year_of_contact = if_else(date1_status != "missing", .data[[y1]], .data[[y2]])
      ) %>%
      group_by(pik, pobst, birth_year) %>%
      summarise(earliest_contact = min(year_of_contact)) %>%
      ungroup() %>%
      mutate(
        birth_year = as.numeric(birth_year),
        table = name,
        age = earliest_contact - birth_year
      ) %>%
      as_tibble()
  } else if(!is.na(y3)) {
    df %>%
      lazy_dt() %>%
      mutate(
        year_of_contact = 
          case_when(
            date1_status == "missing" & date2_status == "missing" ~ .data[[y3]],
            date1_status == "missing" ~ .data[[y2]],
            date1_status != "missing" ~ .data[[y1]]
          )
      ) %>%
      group_by(pik, pobst, birth_year) %>%
      summarise(earliest_contact = min(year_of_contact)) %>%
      ungroup() %>%
      mutate(
        birth_year = as.numeric(birth_year),
        table = name,
        age = earliest_contact - birth_year
      ) %>%
      as_tibble()
  }
}

earliest_contact <-
  pmap(
    list(
      df = removed_entries_cjars_subject_tables,
      y1 = list("ARR_ARR_DT_YYYY", "ADJ_FILE_DT_YYYY", "PRO_BGN_DT_YYYY", "INC_ENTRY_DT_YYYY", "PAR_BGN_DT_YYYY"),
      y2 = list("ARR_BOOK_DT_YYYY", "ADJ_DISP_DT_YYYY", "PRO_END_DT_YYYY", "INC_EXIT_DT_YYYY", "PAR_END_DT_YYYY"),
      y3 = list(NA, "ADJ_SENT_DT_YYYY", NA, NA, NA),
      name = names(removed_entries_cjars_subject_tables)
    ),
    find_earliest_contact
  )
names(earliest_contact) <- names(removed_entries_cjars_subject_tables)

# See overall age distribution.
age_first_contact <-
  pmap(
    list(earliest_contact, names(earliest_contact)),
    function(df, name) {
      df %>%
        count(age) %>%
        mutate(prcnt = n / sum(n) * 100, table = name) %>%
        arrange(age) %>%
        mutate(cumsum = cumsum(prcnt))
    }
  ) %>%
  bind_rows() %>%
  arrange(table, age)

write_csv(age_first_contact, file.path(meta_dir, "age_first_contact.csv"))

age_first_contact_pdf <-
  ggplot(age_first_contact, aes(x = age, y = prcnt)) +
  geom_point() +
  geom_line() +
  facet_wrap(~table) +
  theme_bw() +
  geom_vline(xintercept = 17, alpha = 0.5, color = "red") +
  #scale_x_continuous(breaks = seq(0, <redacted>, <redacted>))

ggsave(
  filename = file.path(meta_dir, "age_of_first_contact.png"),
  age_first_contact_pdf,
  height = 12,
  width = 12
)

################################################################################
# Drop individuals from cohorts that are too old or too young.
################################################################################
cjars_yearly_coverage <-
  read_csv(
    file.path("cjars_yearly_coverage.csv")
  )

cjars_earliest_age <- 17

coverage_by_state <-
  cjars_yearly_coverage %>%
  group_by(state) %>%
  summarise(
    min_birth_cohort = min(year) - cjars_earliest_age,
    max_birth_cohort = max(year) - cjars_earliest_age
  )

numident_cjars_birth_cohorts <-
  numident_cjars %>%
  lazy_dt() %>%
  full_join(coverage_by_state, by = c("pobst" = "state")) %>%
  filter(birth_year >= min_birth_cohort, birth_year <= max_birth_cohort) %>%
  select(-min_birth_cohort, -max_birth_cohort) %>%
  as_tibble()

cjars_subject_tables_cohort <-
  map(
    removed_entries_cjars_subject_tables,
    function(df, df2) {
      df %>% lazy_dt() %>% filter(df$pik %in% df2$pik) %>% as_tibble()
    },
    df2 = numident_cjars_birth_cohorts
  )

individual_errors_cohort <-
  map(
    individual_errors,
    function(df, df2) {
      df %>% lazy_dt() %>% filter(df$pik %in% df2$pik) %>% as_tibble()
    },
    df2 = numident_cjars_birth_cohorts
  )

################################################################################
# Remove individuals who have no records in any CJARS table (but have a CJARS ID).
################################################################################
# The strangest error. Dtplyr doesn't like it when the name is par.
names(clean_cjars_subject_tables)[5] <- "parole"

numident_wide <-
  numident_cjars_birth_cohorts %>%
  lazy_dt() %>%
  select(-dodmm, -doddd, -dobmm, -dobdd, -birth_date_status, -death_date_status) %>%
  mutate(
    in_arr = CJARS_ID %in% clean_cjars_subject_tables$arr$CJARS_ID,
    in_adj = CJARS_ID %in% clean_cjars_subject_tables$adj$CJARS_ID,
    in_incar = CJARS_ID %in% clean_cjars_subject_tables$incar$CJARS_ID,
    in_probat = CJARS_ID %in% clean_cjars_subject_tables$pro$CJARS_ID,
    in_parole = CJARS_ID %in% clean_cjars_subject_tables$parole$CJARS_ID,
    flag_cjars_id = !is.na(CJARS_ID) & !in_arr & !in_adj & !in_incar & !in_probat & !in_parole,
    flag_src_st = !(SRC_ST %in% focal_states) & !is.na(SRC_ST)
  ) %>%
  group_by(pik) %>%
  mutate(
    nr_cjars_ids = sum(!is.na(CJARS_ID)),
    nr_flags_cjars_ids = sum(flag_cjars_id),
    nr_flags_src_st = sum(flag_src_st)
  ) %>%
  ungroup() %>%
  select(-matches("in_"), -flag_cjars_id, -flag_src_st, -CJARS_ID, -SRC_ST) %>%
  distinct(pik, .keep_all = T) %>%
  filter(nr_cjars_ids != nr_flags_cjars_ids | nr_cjars_ids == 0) %>%
  as_tibble()

################################################################################
# Add total number of contacts and problematic entries to the wide file.
################################################################################
write_dir <- ""

individual_errors_all <-
  pmap(
    list(individual_errors_cohort, names(individual_errors_cohort)),
    function(error_df, name_df) {
      error_df %>%
        rename_with(
          .fn = function(col) {paste0(col, "_", name_df)},
          .cols = -pik
        )
    }
  ) %>%
  reduce(.f = function(x, y) {full_join(x, y, by = "pik")}) %>%
  mutate(across(where(is.numeric), function(col) {if_else(is.na(col), 0, col)}))

cj_contact_all <-
  pmap(
    list(clean_cjars_subject_tables, names(clean_cjars_subject_tables)),
    function(df, name) {
      if(name == "arr") {
        df <-
          df %>%
          lazy_dt() %>%
          mutate(
            error =
              if_else(
                dob_error | dod_error | missing_date,
                "nr_arr_dirty",
                "nr_arr_clean"
              )
          ) %>%
          count(pik, error) %>%
          pivot_wider(
            id_cols = "pik", names_from = "error", values_from = "n"
          ) %>%
          as_tibble()
        
      } else if(name %in% c("incar", "parole", "pro")) {
        df <-
          df %>%
          lazy_dt() %>%
          mutate(
            error =
              if_else(
                dob_error | dod_error | missing_date | missing_entry_or_exit_not_both | dates_out_of_order,
                paste0("nr_", name, "_dirty"),
                paste0("nr_", name, "_clean")
              )
          ) %>%
          count(pik, error) %>%
          pivot_wider(
            id_cols = "pik", names_from = "error", values_from = "n"
          ) %>%
          as_tibble()
        
      } else if(name == "adj") {
        df_clean_adj <-
          df %>%
          lazy_dt() %>%
          filter(!dob_error & !dod_error & !missing_date) %>%
          mutate(
            ADJ_DISP_CD =
              case_when(
                ADJ_DISP_CD == "DU" ~ "nr_adj_diversion",
                ADJ_DISP_CD %in% c("GC", "GJ", "GP", "GI", "GU") ~ "nr_adj_guilty",
                ADJ_DISP_CD %in% c("NA", "ND", "NI", "NM", "NU", "NP") ~ "nr_adj_notGuilty",
                ADJ_DISP_CD %in% c("PT", "PU") ~ "nr_adj_procedural",
                ADJ_DISP_CD == "UU" ~ "nr_adj_unknown"
              )
          ) %>%
          count(pik, ADJ_DISP_CD, ADJ_GRD_CD, name = "nr_adj") %>%
          pivot_wider(
            id_cols = "pik",
            names_from = c("ADJ_DISP_CD", "ADJ_GRD_CD"),
            values_from = "nr_adj"
          ) %>%
          as_tibble()
          
          df_dirty_adj <-
            df %>%
            lazy_dt() %>%
            filter(dob_error | dod_error | missing_date) %>%
            count(pik, name = "nr_adj_dirty") %>%
            as_tibble()
          
          df <-
            full_join(df_clean_adj, df_dirty_adj, by = "pik") %>%
            mutate(
              across(
                where(is.numeric), function(col) {if_else(is.na(col), 0, col)}
              )
            )
      }
      
      return(df)
    }
  ) %>%
  reduce(.f = function(x, y) {full_join(x, y, by = "pik")}) %>%
  mutate(
    across(where(is.numeric), function(col) {if_else(is.na(col), 0, col)}),
    nr_arr_total = rowSums(pick(matches("nr_arr"))),
    nr_incar_total = rowSums(pick(matches("nr_incar"))),
    nr_parole_total = rowSums(pick(matches("nr_parole"))),
    nr_pro_total = rowSums(pick(matches("nr_pro"))),
    nr_adj_clean = rowSums(pick(matches("nr_adj_"), -nr_adj_dirty)),
    nr_adj_felony = rowSums(pick(matches("nr_adj_.*_FE$"))),
    nr_adj_misd = rowSums(pick(matches("nr_adj_.*_MI$"))),
    nr_adj_guilty = rowSums(pick(matches("nr_adj_guilty"))),
    nr_adj_notGuilty = rowSums(pick(matches("nr_adj_notGuilty"))),
    nr_adj_diversion = rowSums(pick(matches('nr_adj_diversion'))),
    nr_adj_total = rowSums(pick(matches("nr_adj_dirty|nr_adj_clean")))
  )

numident_wide_final <-
  numident_wide %>%
  lazy_dt() %>%
  mutate(
    bestrace =
      case_when(
        bestrace == "0" | bestrace == "" ~ "unknown",
        bestrace == "1" ~ "white",
        bestrace == "2" ~ "black",
        bestrace == "3" ~ "other",
        bestrace == "4" ~ "asian_or_pi",
        bestrace == "5" ~ "hispanic",
        bestrace == "6" ~ "na_or_an"
      )
  ) %>%
  left_join(individual_errors_all, by = "pik") %>%
  left_join(cj_contact_all, by = "pik") %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), function(col) {if_else(is.na(col), 0, col)})) %>%
  mutate(
    cj_contact =
      case_when(
        nr_cjars_ids == 0 ~ "no_cj_contact",
        nr_arr_clean == 0 & nr_adj_clean == 0 & nr_incar_clean == 0 & nr_pro_clean == 0 & nr_parole_clean == 0 ~ "questionable",
        nr_arr_clean != 0 | nr_adj_clean != 0 | nr_incar_clean != 0 | nr_pro_clean != 0 | nr_parole_clean != 0 ~ "cj_contact"
      )
  )

write_csv(numident_wide_final, file.path(write_dir, "numident_wide.csv"))
write_dta(numident_wide_final, file.path(write_dir, "numident_wide.dta"))

pwalk(
  list(cjars_subject_tables_cohort, names(cjars_subject_tables_cohort)),
  function(df, name, filepath) {
    write_csv(df, file.path(filepath, paste0(name, ".csv")))
    write_dta(df, file.path(filepath, paste0(name, ".dta")))
  },
  filepath = write_dir
)
