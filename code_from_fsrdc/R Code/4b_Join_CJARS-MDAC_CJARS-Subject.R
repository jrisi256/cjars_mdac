library(readr)
library(haven)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

output_dir <- ""
input_dir <- ""
cjars_dir <- ""

################################################################################
cjars_mdac <- read_csv(file.path(output_dir, "3_joined_cjars_mdac.csv"))
mdac_demog_recode <- read_csv(file.path(input_dir, "mdac_demog_recode.csv"))

# Need date of birth and date of death.
cjars_mdac <-
  cjars_mdac %>%
  left_join(
    select(mdac_demog_recode, CMID, PNUM, dob, dod), by = c("CMID", "PNUM")
  )

# Arrests
arrests <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_arrest_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "ARR_ARR_DT_YYYY", "ARR_ARR_DT_MM", "ARR_ARR_DT_DD",
        "ARR_BOOK_DT_YYYY", "ARR_BOOK_DT_MM", "ARR_BOOK_DT_DD", "ARR_ID"
      )
  )

# Adjudications
adj <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_adjud_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "ADJ_FILE_DT_YYYY", "ADJ_FILE_DT_MM", "ADJ_FILE_DT_DD",
        "ADJ_DISP_DT_YYYY", "ADJ_DISP_DT_MM", "ADJ_DISP_DT_DD",
        "ADJ_SENT_DT_YYYY", "ADJ_SENT_DT_MM", "ADJ_SENT_DT_DD"
      )
  )

# Incarceration
incar <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_incar_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "INC_ENTRY_DT_YYYY", "INC_ENTRY_DT_MM", "INC_ENTRY_DT_DD",
        "INC_EXIT_DT_YYYY", "INC_EXIT_DT_MM", "INC_EXIT_DT_DD"
      )
  )

# Probation
probat <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_probat_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "PRO_BGN_DT_YYYY", "PRO_BGN_DT_MM", "PRO_BGN_DT_DD",
        "PRO_END_DT_YYYY", "PRO_END_DT_MM", "PRO_END_DT_DD"
      )
  )

# Parole
parole <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_parole_rsch.sas7bdat"),
    col_select =
      c(
        "CJARS_ID", "PAR_BGN_DT_YYYY", "PAR_BGN_DT_MM", "PAR_BGN_DT_DD",
        "PAR_END_DT_YYYY", "PAR_END_DT_MM", "PAR_END_DT_DD"
      )
  )

################################################################################
# Column names for the date columns in the CJARS subject tables.
dfs <- list(arrests, adj, incar, probat, parole)
d1s <- c("ARR_ARR", "ADJ_FILE", "INC_ENTRY", "PRO_BGN", "PAR_BGN")
d2s <- c("ARR_BOOK", "ADJ_DISP", "INC_EXIT", "PRO_END", "PAR_END")

d1yrs <- paste0(d1s, "_DT_YYYY") %>% as.list()
d1months <- paste0(d1s, "_DT_MM") %>% as.list()
d1days <- paste0(d1s, "_DT_DD") %>% as.list()

d2yrs <- paste0(d2s, "_DT_YYYY") %>% as.list()
d2months <- paste0(d2s, "_DT_MM") %>% as.list()
d2days <- paste0(d2s, "_DT_DD") %>% as.list()

d3yrs <- list(NA, "ADJ_SENT_DT_YYYY", NA, NA, NA)
d3months <- list(NA, "ADJ_SENT_DT_MM", NA, NA, NA)
d3days <- list(NA, "ADJ_SENT_DT_DD", NA, NA, NA)

# Join DOD and DOB to each of the CJARS tables
dfs <-
  map(
    dfs,
    function(df, df2) {
      df %>% inner_join(select(df2, CJARS_ID, dob, dod), by = "CJARS_ID")
    },
    df2 = cjars_mdac
  )

################################################################################
# Check for incompleteness
################################################################################
check_incomplete <-
  function(df, d1y, d1m, d1d, d2y, d2m, d2d, d3y, d3m, d3d) {
    df <-
      df %>%
      mutate(
        yr_miss_d1 = if_else(is.na(.data[[d1y]]), "m", "nm"),
        month_miss_d1 = if_else(is.na(.data[[d1m]]), "m", "nm"),
        day_miss_d1 = if_else(is.na(.data[[d1d]]), "m", "nm"),
        incomp_d1 = paste0(yr_miss_d1, "_", month_miss_d1, "_", day_miss_d1),
        yr_miss_d2 = if_else(is.na(.data[[d2y]]), "m", "nm"),
        month_miss_d2 = if_else(is.na(.data[[d2m]]), "m", "nm"),
        day_miss_d2 = if_else(is.na(.data[[d2d]]), "m", "nm"),
        incomp_d2 = paste0(yr_miss_d2, "_", month_miss_d2, "_", day_miss_d2),
        incomp_d1_bin = if_else(!(incomp_d1 %in% c("nm_nm_nm", "m_m_m")), 1, 0),
        incomp_d2_bin = if_else(!(incomp_d2 %in% c("nm_nm_nm", "m_m_m")), 1, 0)
      )

    if (!is.na(d3y) & !is.na(d3m) & !is.na(d3d)) {
      df <-
        df %>%
        mutate(
          yr_miss_d3 = if_else(is.na(.data[[d3y]]), "m", "nm"),
          month_miss_d3 = if_else(is.na(.data[[d3m]]), "m", "nm"),
          day_miss_d3 = if_else(is.na(.data[[d3d]]), "m", "nm"),
          incomp_d3 = paste0(yr_miss_d3, "_", month_miss_d3, "_", day_miss_d3),
          incomp_d3_bin =
            if_else(!(incomp_d3 %in% c("nm_nm_nm", "m_m_m")), 1, 0)
        )
    }

    return(df)
  }

incomplete <-
  pmap(
    list(
      dfs, d1yrs, d1months, d1days, d2yrs, d2months, d2days, d3yrs, d3months,
      d3days
    ),
    check_incomplete
  )

################################################################################
# Determine for each date if it is missing, before/during 2015, or after 2015.
################################################################################
tables <- list("arr", "adj", "inc", "pro", "par")

check_date_pattern <-
  function(df, d1y, d1m, d1d, d2y, d2m, d2d, d3y, d3m, d3d, tbl) {
    df <-
      df %>%
      mutate(
        d1 =
          case_when(
            incomp_d1 %in% c("m_m_m", "m_nm_nm", "m_nm_m", "m_m_nm") ~ NA_Date_,
            incomp_d1 == "nm_m_m" ~ ymd(paste(.data[[d1y]], "12", "31")),
            incomp_d1 == "nm_nm_m" ~ ym(paste(.data[[d1y]], .data[[d1m]])),
            incomp_d1 == "nm_m_nm" ~ ymd(paste(.data[[d1y]], "12", .data[[d1d]])),
            incomp_d1 == "nm_nm_nm" ~ ymd(paste(.data[[d1y]], .data[[d1m]], .data[[d1d]]))
          ),
        d2 =
          case_when(
            incomp_d2 %in% c("m_m_m", "m_nm_nm", "m_nm_m", "m_m_nm") ~ NA_Date_,
            incomp_d2 == "nm_m_m" ~ ymd(paste(.data[[d2y]], "12", "31")),
            incomp_d2 == "nm_nm_m" ~ ym(paste(.data[[d2y]], .data[[d2m]])),
            incomp_d2 == "nm_m_nm" ~ ymd(paste(.data[[d2y]], "12", .data[[d2d]])),
            incomp_d2 == "nm_nm_nm" ~ ymd(paste(.data[[d2y]], .data[[d2m]], .data[[d2d]]))
          )
      ) %>%
      mutate(
        d1_status =
          case_when(
            is.na(d1) ~ paste0(tbl, "_missing"),
            year(d1) > 2015 ~ paste0(tbl, "_post2015"),
            year(d1) <= 2015 ~ paste0(tbl, "_pre2015")
          ),
        d2_status =
          case_when(
            is.na(d2) ~ "_missing",
            year(d2) > 2015 ~ "_post2015",
            year(d2) <= 2015 ~ "_pre2015"
          )
      )

    if (!is.na(d3y) & !is.na(d3m) & !is.na(d3d)) {
      df <-
        df %>%
        mutate(
          d3 =
            case_when(
              incomp_d3 %in% c("m_m_m", "m_nm_nm", "m_nm_m", "m_m_nm") ~ NA_Date_,
              incomp_d3 == "nm_m_m" ~ ymd(paste(.data[[d3y]], "12", "31")),
              incomp_d3 == "nm_nm_m" ~ ym(paste(.data[[d3y]], .data[[d3m]])),
              incomp_d3 == "nm_m_nm" ~ ymd(paste(.data[[d3y]], "12", .data[[d3d]])),
              incomp_d3 == "nm_nm_nm" ~ ymd(paste(.data[[d3y]], .data[[d3m]], .data[[d3d]]))
            )
        ) %>%
        mutate(
          d3_status =
            case_when(
              is.na(d3) ~ "_missing",
              year(d3) > 2015 ~ "_post2015",
              year(d3) <= 2015 ~ "_pre2015"
            ),
          d_status = paste0(d1_status, d2_status, d3_status)
        )
    } else {
      df <- df %>% mutate(d_status = paste0(d1_status, d2_status))
    }
    
    return(df)
  }

date_pattern <-
  pmap(
    list(
      incomplete, d1yrs, d1months, d1days, d2yrs, d2months, d2days, d3yrs,
      d3months, d3days, tables
    ),
    check_date_pattern
  )

################################################################################
# Determine if any of the date columns are in error.
################################################################################
three_date_flags <- list(F, T, F, F, F)

check_error <- function(df, three_date_flag) {
  df <-
    df %>%
    mutate(
      error_d1_d2 =
        case_when(
          is.na(d1) | is.na(d2) ~ 0,
          incomp_d1 %in% c("nm_m_m", "nm_m_nm") | incomp_d2 %in% c("nm_m_m", "nm_m_nm") ~
            year(d1) > year(d2),
          incomp_d1 == "nm_nm_m" | incomp_d2 == "nm_nm_m" ~
            ym(paste(year(d1), month(d1))) > ym(paste(year(d2), month(d2))),
          incomp_d1 == "nm_nm_nm" & incomp_d2 == "nm_nm_nm" ~ d1 > d2,
        )
    )

  if (three_date_flag) {
    df <-
      df %>%
      mutate(
        error_d1_d3 =
          case_when(
            is.na(d1) | is.na(d3) ~ 0,
            incomp_d1 %in% c("nm_m_m", "nm_m_nm") | incomp_d3 %in% c("nm_m_m", "nm_m_nm") ~
              year(d1) > year(d3),
            incomp_d1 == "nm_nm_m" | incomp_d3 == "nm_nm_m" ~
              ym(paste(year(d1), month(d1))) > ym(paste(year(d3), month(d3))),
            incomp_d1 == "nm_nm_nm" & incomp_d3 == "nm_nm_nm" ~ d1 > d3,
          ),
        error_d2_d3 =
          case_when(
            is.na(d2) | is.na(d3) ~ 0,
            incomp_d2 %in% c("nm_m_m", "nm_m_nm") | incomp_d3 %in% c("nm_m_m", "nm_m_nm") ~
              year(d2) > year(d3),
            incomp_d2 == "nm_nm_m" | incomp_d3 == "nm_nm_m" ~
              ym(paste(year(d2), month(d2))) > ym(paste(year(d3), month(d3))),
            incomp_d2 == "nm_nm_nm" & incomp_d3 == "nm_nm_nm" ~ d2 > d3,
          )
      )
  }

  return(df)
}

error <- pmap(list(date_pattern, three_date_flags), check_error)

################################################################################
# Check if any of the CJ contacts occur before someone's DOB or after their DOD.
################################################################################
check_dob_dod_error <- function(df, three_date_flag) {
  df <-
    df %>%
    mutate(
      error_d1_dob_dod =
        case_when(
          is.na(d1) ~ 0,
          incomp_d1 %in% c("nm_m_m", "nm_m_nm") ~
            year(d1) < year(dob) | (year(d1) > year(dod) & !is.na(dod)),
          incomp_d1 == "nm_nm_m" ~
            ym(paste(year(d1), month(d1))) < ym(paste(year(dob), month(dob))) | (ym(paste(year(d1), month(d1))) > ym(paste(year(dod), month(dod))) & !is.na(dod)),
          incomp_d1 == "nm_nm_nm" ~ d1 < dob | (d1 > dod & !is.na(dod))
        ),
      error_d2_dob_dod =
        case_when(
          is.na(d2) ~ 0,
          incomp_d2 %in% c("nm_m_m", "nm_m_nm") ~
            year(d2) < year(dob) | (year(d2) > year(dod) & !is.na(dod)),
          incomp_d2 == "nm_nm_m" ~
            ym(paste(year(d2), month(d2))) < ym(paste(year(dob), month(dob))) | (ym(paste(year(d2), month(d2))) > ym(paste(year(dod), month(dod))) & !is.na(dod)),
          incomp_d2 == "nm_nm_nm" ~ d2 < dob | (d2 > dod & !is.na(dod))
        ),
      
    )
  
  if (three_date_flag) {
    df <-
      df %>%
      mutate(
        error_d3_dob_dod =
          case_when(
            is.na(d3) ~ 0,
            incomp_d3 %in% c("nm_m_m", "nm_m_nm") ~
              year(d3) < year(dob) | (year(d3) > year(dod) & !is.na(dod)),
            incomp_d3 == "nm_nm_m" ~
              ym(paste(year(d3), month(d3))) < ym(paste(year(dob), month(dob))) | (ym(paste(year(d3), month(d3))) > ym(paste(year(dod), month(dod))) | !is.na(dod)),
            incomp_d3 == "nm_nm_nm" ~ d3 < dob | (d3 > dod & !is.na(dod))
          )
      )
  }
  
  return(df)
}

dob_dod_error <- pmap(list(error, three_date_flags), check_dob_dod_error)

################################################################################
# Calculate number of CJ contacts
################################################################################
count_cj_contact <-
  function(df, cols, incomp, error, error_dob_dod, three_date_flag, name) {
    # What columns count as CJ contact
    if (!(any(is.na(cols)))) {
      df <- df %>% filter(d_status %in% cols)
    }
    
    # Include no dob_dod errors, only dob_dod errors, or both errors and non_errors
    if (error_dob_dod == "none") {
      df <- df %>% filter(error_d1_dob_dod == 0, error_d2_dob_dod == 0)
      
      if (three_date_flag) {
        df <- df %>% filter(error_d3_dob_dod == 0)
      }
    } else if (error_dob_dod == "only") {
      if (three_date_flag) {
        df <-
          df %>%
          filter(error_d1_dob_dod == 1 | error_d2_dob_dod == 1 | error_d3_dob_dod == 1)
      } else {
        df <- df %>% filter(error_d1_dob_dod == 1 | error_d2_dob_dod == 1)
      }
    }
    
    # Include no incompletes, only incompletes, or both incompletes and non-incompletes
    if (incomp == "none") {
      df <- df %>% filter(incomp_d1_bin == 0, incomp_d2_bin == 0)
      
      if (three_date_flag) {
        df <- df %>% filter(incomp_d3_bin == 0)
      }
    } else if (incomp == "only") {
      if (three_date_flag) {
        df <-
          df %>%
          filter(incomp_d1_bin == 1 | incomp_d2_bin == 1 | incomp_d3_bin == 1)
      } else {
        df <- df %>% filter(incomp_d1_bin == 1 | incomp_d2_bin == 1)
      }
    }
    
    # Include no errors, only errors, or both errors and non-errors
    if (error == "none") {
      df <- df %>% filter(error_d1_d2 == 0)
      
      if (three_date_flag) {
        df <- df %>% filter(error_d1_d3 == 0, error_d2_d3 == 0)
      }
    } else if (error == "only") {
      if (three_date_flag) {
        df <-
          df %>%
          filter(error_d1_d2 == 1 | error_d1_d3 == 1 | error_d2_d3 == 1)
      } else {
        df <- df %>% filter(error_d1_d2 == 1)
      }
    }
    
    df %>% count(CJARS_ID, name = name)
  }

################################################################################
# Count the total number of CJ contacts (include everything).
################################################################################
cols_all <- as.list(rep(NA, 5))
keep <- as.list(rep(T, 5))
not_keep <- as.list(rep("none",  5))
only <- as.list(rep("only", 5))
cols_all_name <- as.list(paste0("nr_", tables))

nr_contacts <-
  pmap(
    list(
      dob_dod_error, cols_all, keep, keep, keep, three_date_flags, cols_all_name
    ),
    count_cj_contact
  )

################################################################################
# Count # of pre-2015 CJ contacts (includes incomplete, missing, some error).
################################################################################
pre2015_df <-
  expand_grid(
    tables = tables[tables != "adj"],
    cols =
      c(
        "_missing_pre2015", "_pre2015_missing", "_pre2015_post2015",
        "_pre2015_pre2015"
      )
  ) %>%
  mutate(col_name = paste0(tables, cols))

cols_pre2015 <-
  map(
    tables[tables != "adj"],
    function(x, df) {df %>% filter(tables == x) %>% pull(col_name)},
    df = pre2015_df
  )

pre2015_adj <-
  paste0(
    "adj",
    c(
      "_missing_missing_pre2015", "_missing_pre2015_missing",
      "_missing_pre2015_post2015", "_missing_pre2015_pre2015",
      "_pre2015_missing_missing", "_pre2015_missing_post2015",
      "_pre2015_missing_pre2015", "_pre2015_post2015_missing",
      "_pre2015_post2015_post2015", "_pre2015_pre2015_missing",
      "_pre2015_pre2015_post2015", "_pre2015_pre2015_pre2015"
    )
  )

cols_pre2015 <- append(cols_pre2015, list(pre2015_adj), after = 1)
cols_pre2015_name <- as.list(paste0("nr_pre2015_", tables))

nr_pre2015 <-
  pmap(
    list(
      dob_dod_error, cols_pre2015, keep, keep, not_keep, three_date_flags,
      cols_pre2015_name
    ),
    count_cj_contact
  )

################################################################################
# Count # of post-2015 CJ contacts (includes incomplete, missing, some error).
################################################################################
post2015_df <-
  expand_grid(
    tables = tables[tables != "adj"],
    cols = c("_missing_post2015", "_post2015_missing", "_post2015_post2015")
  ) %>%
  mutate(col_name = paste0(tables, cols))

cols_post2015 <-
  map(
    tables[tables != "adj"],
    function(x, df) {df %>% filter(tables == x) %>% pull(col_name)},
    df = post2015_df
  )

post2015_adj <-
  paste0(
    "adj",
    c(
      "_missing_missing_post2015", "_missing_post2015_missing",
      "_missing_post2015_post2015", "_post2015_missing_missing",
      "_post2015_missing_post2015", "_post2015_post2015_missing",
      "_post2015_post2015_post2015"
    )
  )

cols_post2015 <- append(cols_post2015, list(post2015_adj), after = 1)
cols_post2015_name <- as.list(paste0("nr_post2015_", tables))

nr_post2015 <-
  pmap(
    list(
      dob_dod_error, cols_post2015, keep, keep, not_keep, three_date_flags,
      cols_post2015_name
    ),
    count_cj_contact
  )

################################################################################
# Count number of CJ contacts where all dates are missing.
################################################################################
cols_missing <- list()
for (tbl in tables) {
  if (tbl != "adj") {
    cols_missing <- append(cols_missing, list(paste0(tbl, "_missing_missing")))
  } else {
    cols_missing <-
      append(cols_missing, list(paste0(tbl, "_missing_missing_missing")))
  }
}

cols_missing_name <- as.list(paste0("nr_missing_", tables))

nr_missing <-
  pmap(
    list(
      dob_dod_error, cols_missing, keep, keep, keep, three_date_flags,
      cols_missing_name
    ),
    count_cj_contact
  )

################################################################################
# Count the number of CJ contacts whose dates were in fatal error.
################################################################################
cols_fatal_error <-
  as.list(paste0(tables[tables != "adj"], "_post2015_pre2015"))

fatal_error_adj <-
  paste0(
    "adj",
    c(
      "_missing_post2015_pre2015", "_pre2015_post2015_pre2015",
      "_post2015_missing_pre2015", "_post2015_pre2015_pre2015",
      "_post2015_pre2015_missing", "_post2015_pre2015_post2015",
      "_post2015_post2015_pre2015"
    )
  )

cols_fatal_error <- append(cols_fatal_error, list(fatal_error_adj), after = 1)
cols_fatal_error_name <- as.list(paste0("nr_fatal_error_", tables))

nr_fatal_error <-
  pmap(
    list(
      dob_dod_error, cols_fatal_error, keep, keep, not_keep, three_date_flags,
      cols_fatal_error_name
    ),
    count_cj_contact
  )

################################################################################
# Count the number of CJ contacts whose dates were before DOB or after DOD.
################################################################################
col_dob_dod_error_name <- as.list(paste0("nr_error_dob_dod_", tables))

nr_errors_dob_dod <-
  pmap(
    list(
      dob_dod_error, cols_all, keep, keep, only, three_date_flags,
      col_dob_dod_error_name
    ),
    count_cj_contact
  )

################################################################################
# Count # of clean pre2015 CJ contacts (no missing, no incomplete, no error).
################################################################################
pre2015_clean_df <-
  expand_grid(
    tables = tables[tables != "adj"],
    cols = c("_pre2015_post2015", "_pre2015_pre2015")
  ) %>%
  mutate(col_name = paste0(tables, cols))

cols_pre2015_clean <-
  map(
    tables[tables != "adj"],
    function(x, df) {df %>% filter(tables == x) %>% pull(col_name)},
    df = pre2015_clean_df
  )

pre2015_clean_adj <-
  paste0(
    "adj",
    c(
      "_pre2015_post2015_post2015", "_pre2015_pre2015_post2015",
      "_pre2015_pre2015_pre2015"
    )
  )

cols_pre2015_clean <- append(cols_pre2015_clean, list(pre2015_clean_adj), after = 1)
cols_pre2015_clean_name <- as.list(paste0("nr_pre2015_clean_", tables))

nr_pre2015_clean <-
  pmap(
    list(
      dob_dod_error, cols_pre2015_clean, not_keep, not_keep, not_keep,
      three_date_flags, cols_pre2015_clean_name
    ),
    count_cj_contact
  )

################################################################################
# Count # of CJ contacts (include missing, no incomplete, no error).
################################################################################
cols_pre2015_noie_name <- as.list(paste0("nr_pre2015_noie_", tables))

nr_pre2015_noie <-
  pmap(
    list(
      dob_dod_error, cols_pre2015, not_keep, not_keep, not_keep,
      three_date_flags, cols_pre2015_noie_name
    ),
    count_cj_contact
  )

################################################################################
# Join together all the counts.
################################################################################
joined <-
  pmap(
    list(
      nr_contacts, nr_pre2015, nr_post2015, nr_missing, nr_fatal_error,
      nr_errors_dob_dod, nr_pre2015_clean, nr_pre2015_noie 
    ),
    function(df1, df2, df3, df4, df5, df6, df7, df8) {
      reduce(
        list(df1, df2, df3, df4, df5, df6, df7, df8),
        function(x, y) {full_join(x, y, by = "CJARS_ID")}
      ) %>%
        mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, .x)))
    }
  )

################################################################################
# Categorize people: 1) contact pre-2015, 2) no contact pre-2015, 3) uncertain
################################################################################
pre2015_contact_names <- as.list(paste0("pre2015_contact_", tables))
all_post2015_contact_names <- as.list(paste0("all_post2015_", tables))
pre2015_uncertain_names <- as.list(paste0("pre2015_uncertain_", tables))

joined <-
  pmap(
    list(
      joined, cols_all_name, cols_pre2015_name, cols_post2015_name,
      pre2015_contact_names, all_post2015_contact_names, pre2015_uncertain_names
    ),
    function(df, col_total, col_pre2015, col_post2015, pre2015_contact,
             all_post2015, pre2015_uncertain) {
      df %>%
        mutate(
          "{pre2015_contact}" := .data[[col_pre2015]] >= 1,
          "{all_post2015}" := .data[[col_post2015]] == .data[[col_total]],
          "{pre2015_uncertain}" := !.data[[all_post2015]] & !.data[[pre2015_contact]]
        )
    }
  )

################################################################################
# Join w/ MDAC and find # of cases w/ uncertain status across all CJARS tables.
################################################################################
joined_mdac <-
  joined %>%
  reduce(function(x, y) {full_join(x, y, by = "CJARS_ID")}) %>%
  full_join(cjars_mdac, by = "CJARS_ID") %>%
  mutate(across(matches("nr_"), ~ if_else(is.na(.x), 0, .x)))

states <-
  map(
    cols_pre2015_name,
    function(col, df) {
      df <- df %>% filter(.data[[col]] >= 1)
      table(df$SRC_ST)
    },
    df = joined_mdac
  )

# Has a CJARS ID but no record of contact in any of the subject tables.
check_has_cjars_id_but_no_cj_contact <-
  joined_mdac %>%
  filter(
    !is.na(CJARS_ID) & nr_arr == 0 & nr_adj == 0 & nr_inc == 0 & nr_pro == 0 &
      nr_par == 0
  )

# Has a pre-2015 CJ contact.
check_contact_with_cj_pre2015 <-
  joined_mdac %>%
  filter(
    nr_pre2015_arr >=1 | nr_pre2015_adj >= 1 | nr_pre2015_inc >= 1 |
      nr_pre2015_pro >= 1 | nr_pre2015_par >= 1
  )

# Has clean CJ contact pre-2015.
check_contact_with_cj_pre2015_clean <-
  joined_mdac %>%
  filter(
    nr_pre2015_clean_arr >=1 | nr_pre2015_clean_adj >= 1 | nr_pre2015_clean_inc >= 1 |
      nr_pre2015_clean_pro >= 1 | nr_pre2015_clean_par >= 1
  ) %>%
  nrow()

# Has a CJ contact pre-2015 not counting dates in error or incomplete dates.
check_contact_with_cj_pre2015_noie <-
  joined_mdac %>%
  filter(
    nr_pre2015_noie_arr >=1 | nr_pre2015_noie_adj >= 1 | nr_pre2015_noie_inc >= 1 |
      nr_pre2015_noie_pro >= 1 | nr_pre2015_noie_par >= 1
  ) %>%
  nrow()

# All CJ contacts are post-2015.
check_contact_all_post2015 <-
  joined_mdac %>%
  # keep all rows where all contacts from at least one table are post 2015.
  filter(
    all_post2015_arr == T | all_post2015_adj == T | all_post2015_inc == T |
      all_post2015_pro == T | all_post2015_par == T
  ) %>%
  # keep all rows where: 1) all contacts from each table are post 2015, and/or
  # 2) there's no recorded contact of that type (although not for all tables).
  filter(
    (all_post2015_arr == T | is.na(all_post2015_arr)) &
      (all_post2015_adj == T | is.na(all_post2015_adj)) &
      (all_post2015_inc == T | is.na(all_post2015_inc)) &
      (all_post2015_pro == T | is.na(all_post2015_pro)) &
      (all_post2015_par == T | is.na(all_post2015_par))
  )

# Uncertain if person had contact with CJ system pre-2015.
check_pre2015_status_uncertain <-
  joined_mdac %>%
  # Keep all rows where their status is uncertain for at least one table.
  filter(
    pre2015_uncertain_arr == T | pre2015_uncertain_adj == T |
      pre2015_uncertain_inc == T | pre2015_uncertain_pro == T |
      pre2015_uncertain_par == T
  ) %>%
  # Keep all rows: 1) whose status is uncertain across all tables, and/or
  # 2) who had no record in that table (although not for all tables), and/or
  # 3) had all contacts in that table post-2015 (although not for all tables).
  filter(
    (pre2015_uncertain_arr == T | is.na(pre2015_uncertain_arr) | all_post2015_arr == T) &
      (pre2015_uncertain_adj == T | is.na(pre2015_uncertain_adj) | all_post2015_adj == T) &
      (pre2015_uncertain_inc == T | is.na(pre2015_uncertain_inc) | all_post2015_inc == T) &
      (pre2015_uncertain_pro == T | is.na(pre2015_uncertain_pro) | all_post2015_pro == T) &
      (pre2015_uncertain_par == T | is.na(pre2015_uncertain_par) | all_post2015_par == T)
  )

################################################################################
# Write out results.
################################################################################
joined_final <-
  joined_mdac %>%
  select(
    CJARS_ID, nr_arr, nr_pre2015_arr, nr_post2015_arr, nr_adj, nr_pre2015_adj,
    nr_post2015_adj, nr_inc, nr_pre2015_inc, nr_post2015_inc, nr_pro,
    nr_pre2015_pro, nr_post2015_pro, nr_par, nr_pre2015_par, nr_post2015_par,
    SRC_ST, pik, CMID, PNUM
  )

write_csv(
  joined_final, file.path(output_dir, "4_joined_mdac_cjars-subject.csv")
)
