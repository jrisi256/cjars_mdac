library(readr)
library(dplyr)
library(purrr)
library(haven)
library(tidyr)
library(dtplyr)
library(lubridate)

################################################################################
# Read in data.
################################################################################
out_dir <- ""
wide_dir <- ""
cjars_dir <- ""

focal_states_sample <-
  read_csv(
    file.path(out_dir, "8_mdac_wide_focal_states.csv"),
    col_select = c(pik, cmid, pnum, dob, dod, state),
    col_types = cols(cmid = "c", pik = "c")
  )

crosswalk <-
  read_csv(
    file.path(out_dir, "3_joined_cjars_mdac.csv"),
    col_types = cols(CMID = "c", pik = "c")
  ) %>%
  rename_with(.fn = tolower)

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
# Clean CJARS data and get it ready to be merged with long data.
################################################################################
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

check_dob_dod_error <- function(date_status, date, dob, dod, year, month) {
  case_when(
    # Cannot have a dob/dod error if the date is missing.
    date_status == "missing" ~ F,
    # If date did not fail to parse, compare dates normally.
    date_status == "nm_nm_nm" ~ date < dob | (date > dod & !is.na(dod)),
    # If only the year is available (or year + day), just compare years.
    date_status %in% c("nm_m_m", "nm_m_nm") ~ year < year(dob) | (year > year(dod) & !is.na(dod)),
    # If only the year + month are available, compare by year and month.
    date_status == "nm_nm_m" ~ 
      ym(paste(year, month)) < ym(paste(year(dob), month(dob))) | (ym(paste0(year, month)) > ym(paste(year(dod), month(dod))) & !is.na(dod))
  )
}

create_clean_cjars_subject_dates <- function(df, df2, y1, m1, d1, y2, m2, d2, y3, m3, d3) {
  df_new <-
    df %>%
    inner_join(df2, by = c("CJARS_ID" = "cjars_id")) %>%
    mutate(
      date1 = ymd(paste0(.data[[y1]], "-", .data[[m1]], "-", .data[[d1]])),
      date2 = ymd(paste0(.data[[y2]], "-", .data[[m2]], "-", .data[[d2]])),
      date1_status = check_date_incomplete_status(.data[[y1]], .data[[m1]], .data[[d1]], date1),
      date2_status = check_date_incomplete_status(.data[[y2]], .data[[m2]], .data[[d2]], date2),
      date1_dob_dod_error = check_dob_dod_error(date1_status, date1, dob, dod, .data[[y1]], .data[[m1]]),
      date2_dob_dod_error = check_dob_dod_error(date2_status, date2, dob, dod, .data[[y2]], .data[[m2]])
    )
  
  # No 3rd date column.
  if(is.na(y3) | is.na(y2) | is.na(y1)) {
    df_new <-
      df_new %>%
      mutate(
        # Date has a DOB/DOD error if any dates are in error.
        dob_dod_error = if_else(date1_dob_dod_error | date2_dob_dod_error, T, F),
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
        date3_dob_dod_error = check_dob_dod_error(date3_status, date3, dob, dod, .data[[y3]], .data[[m3]]),
        dob_dod_error = if_else(date1_dob_dod_error | date2_dob_dod_error | date3_dob_dod_error, T, F),
        missing_date = date1_status == "missing" & date2_status == "missing" & date3_status == "missing"
      ) %>%
      as_tibble()
  }
}

# Join sample to CJARS roster to obtain CJARS IDs for each individual.
mdac_cjars <- left_join(focal_states_sample, crosswalk, by = c("pik", "cmid", "pnum"))

# Check for date of birth errors and number of missing dates.
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
    df2 = mdac_cjars
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

################################################################################
# Add total number of contacts and problematic entries to the wide file.
################################################################################
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
                dob_dod_error | missing_date, "nr_arr_dirty", "nr_arr_clean"
              )
          ) %>%
          count(pik, error) %>%
          pivot_wider(
            id_cols = "pik", names_from = "error", values_from = "n"
          ) %>%
          as_tibble()
        
      } else if(name %in% c("incar", "par", "pro")) {
        df <-
          df %>%
          lazy_dt() %>%
          mutate(
            error =
              if_else(
                dob_dod_error | missing_date | missing_entry_or_exit_not_both | dates_out_of_order,
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
        df_clean_dj <-
          df %>%
          lazy_dt() %>%
          filter(!dob_dod_error & !missing_date) %>%
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
          filter(dob_dod_error | missing_date) %>%
          count(pik, name = "nr_adj_dirty") %>%
          as_tibble()
        
        df <-
          full_join(df_clean_dj, df_dirty_adj, by = "pik") %>%
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
    nr_par_total = rowSums(pick(matches("nr_par"))),
    nr_pro_total = rowSums(pick(matches("nr_pro"))),
    nr_adj_clean = rowSums(pick(matches("nr_adj_"), -nr_adj_dirty)),
    nr_adj_felony = rowSums(pick(matches("nr_adj_.*_FE$"))),
    nr_adj_misd = rowSums(pick(matches("nr_adj_.*_MI$"))),
    nr_adj_guilty = rowSums(pick(matches("nr_adj_guilty"))),
    nr_adj_notGuilty = rowSums(pick(matches("nr_adj_notGuilty"))),
    nr_adj_diversion = rowSums(pick(matches('nr_adj_diversion'))),
    nr_adj_total = rowSums(pick(matches("nr_adj_dirty|nr_adj_clean")))
  ) %>%
  relocate("nr_arr_total", .after = "nr_arr_dirty") %>%
  relocate("nr_incar_total", .after = "nr_incar_dirty") %>%
  relocate("nr_pro_total", .after = "nr_pro_dirty") %>%
  relocate("nr_par_total", .after = "nr_par_dirty") %>%
  relocate("nr_adj_clean", .after = "nr_arr_total") %>%
  relocate("nr_adj_dirty", .after = "nr_adj_clean") %>%
  relocate("nr_adj_total", .after = "nr_adj_dirty") %>%
  mutate(
    cj_contact =
      if_else(
        nr_arr_clean == 0 & nr_adj_clean == 0 & nr_incar_clean == 0 & nr_pro_clean == 0 & nr_par_clean == 0,
        "questionable",
        "cj_contact"
      )
  )

write_csv(cj_contact_all, file.path(wide_dir, "mdac_cjars_wide.csv"))
write_dta(cj_contact_all, file.path(wide_dir, "mdac_cjars_wide.dta"))

pwalk(
  list(
    clean_cjars_subject_tables, names(clean_cjars_subject_tables)
  ),
  function(df, name, filepath) {
    write_csv(df, file.path(filepath, paste0(name, "_cj-event-level.csv")))
    write_dta(df, file.path(filepath, paste0(name, "_cj-event-level.dta")))
  },
  filepath = wide_dir
)
