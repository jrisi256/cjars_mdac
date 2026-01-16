library(haven)
library(dplyr)
library(readr)
library(dtplyr)
library(stringr)
library(lubridate)

################################################################################
# FIRST: Read in files.
################################################################################
input_dir <- ""
output_dir <- ""
cjars_dir <- ""
focal_states <- c("FL", "MI", "NC", "TX", "WI")

# Crosswalk for PIK to CMID + PNUM and PIK to CJARS_ID.
crosswalk <-
  read_csv(
    file.path(output_dir, "3_joined_cjars_mdac.csv"),
    col_types = cols(CMID = "c", pik = "c")
  )

# Crosswalk file for causes of death.
cause_of_death_crosswalk <-
  read_csv(
    file.path(input_dir, "cause113_ucause_crosswalk.csv"),
    col_types = cols(cause113 = "c")
  )

# To start, we only need DOD and DOB to compare against Numident.
mdac_dob_dod <-
  read_sas(
    file.path("mdac2008_2015_v1.sas7bdat"),
    col_select = c("CMID", "PNUM", "RDATE", "DBD", "DBM", "DBY", "dod", "matchstat")
  ) %>%
  # Keep only individuals in the crosswalk file.
  right_join(select(crosswalk, CMID, PNUM, pik), by = c("CMID", "PNUM")) %>%
  # crosswalk files' observations are at the CJARS_ID level not individual (pik).
  distinct(pik, .keep_all = T) %>%
  mutate(
    dob = ymd(paste0(DBY, "-", DBM, "-", DBD)),
    dod = ymd("1960-01-01") + days(dod),
    matchstat =
      case_when(
        matchstat == 0 ~ "could_not_find_dod",
        matchstat == 1 ~ "died_before_interview",
        matchstat == 2 ~ "dead",
        matchstat == 3 ~ "alive"
      )
  ) %>%
  select(-DBD, -DBM, -DBY) %>%
  lazy_dt(key_by = "pik")

# Read in Numident file.
numident <-
  read_sas(
    file.path("cnum_2024q1.sas7bdat"),
    col_select = c("pik", "dobcc", "dobyy", "dobmm", "dobdd", "dodcc", "dodyy", "dodmm", "doddd")
  ) %>%
  mutate(temp = 1) %>%
  lazy_dt(key_by = "pik")

################################################################################
# SECOND: Compare birth dates and death dates in Numident and MDAC.
################################################################################
# Join together MDAC and Numident.
mdac_numident <-
  left_join(mdac_dob_dod, numident, by = "pik") %>%
  mutate(
    numident_dob = ymd(paste0(dobcc, dobyy, "-", dobmm, "-", dobdd)),
    numident_dod = ymd(paste0(dodcc, dodyy, "-", dodmm, "-", doddd)),
    numident_dob_status =
      case_when(
        !is.na(dobcc) & !is.na(dobyy) & !is.na(dobmm) & !is.na(dobdd) & is.na(numident_dob) ~ "missing",
        !is.na(dobcc) & !is.na(dobyy) & !is.na(dobmm) & !is.na(dobdd) ~ "nm_nm_nm",
        is.na(dobcc) | is.na(dobyy) ~ "missing",
        is.na(dobmm) & is.na(dobdd) ~ "nm_m_m",
        is.na(dobdd) ~ "nm_nm_m",
        is.na(dobmm) ~ "nm_m_nm"
      ),
    numident_dod_status =
      case_when(
        !is.na(dodcc) & !is.na(dodyy) & !is.na(dodmm) & !is.na(doddd) & is.na(numident_dod) ~ "missing",
        !is.na(dodcc) & !is.na(dodyy) & !is.na(dodmm) & !is.na(doddd) ~ "nm_nm_nm",
        is.na(dodcc) | is.na(dodyy) ~ "missing",
        is.na(dodmm) & is.na(doddd) ~ "nm_m_m",
        is.na(doddd) ~ "nm_nm_m",
        is.na(dodmm) ~ "nm_m_nm"
      )
  ) %>%
  as_tibble()

numident_date_equal <- function(numident_status, cc, yy, mm, dd, numident_date, compare_date) {
  case_when(
    numident_status == "nm_nm_nm" ~ numident_date == compare_date,
    numident_status == "nm_m_m" ~ as.numeric(paste0(cc, yy)) == year(compare_date),
    numident_status == "nm_m_nm" ~ as.numeric(paste0(cc, yy)) == year(compare_date) & as.numeric(dd) == day(compare_date),
    numident_status == "nm_nm_m" ~ as.numeric(paste0(cc, yy)) == year(compare_date) & as.numeric(mm) == month(compare_date)
  )
}
numident_date_lt <- function(numident_status, cc, yy, mm, dd, numident_date, compare_date) {
  case_when(
    numident_status == "nm_nm_nm" ~ numident_date < compare_date,
    numident_status %in% c("nm_m_m", "nm_m_nm") ~ as.numeric(paste0(cc, yy)) < year(compare_date),
    numident_status == "nm_nm_m" ~ ymd(paste0(cc, yy, "-", mm, "-01")) < compare_date
  )
}
numident_date_lte <- function(numident_status, cc, yy, mm, dd, numident_date, compare_date) {
  case_when(
    numident_status == "nm_nm_nm" ~ numident_date <= compare_date,
    numident_status == "nm_m_m" ~ as.numeric(paste0(cc, yy)) <= year(compare_date),
    numident_status == "nm_m_nm" ~ as.numeric(paste0(cc, yy)) < year(compare_date) | (as.numeric(paste0(cc, yy)) == year(compare_date) & as.numeric(dd) == day(compare_date)),
    numident_status == "nm_nm_m" ~ ymd(paste0(cc, yy, "-", mm, "-01")) <= compare_date
  )
}
numident_date_gt <- function(numident_status, cc, yy, mm, dd, numident_date, compare_date) {
  case_when(
    numident_status == "nm_nm_nm" ~ numident_date > compare_date,
    numident_status %in% c("nm_m_m", "nm_m_nm") ~ as.numeric(paste0(cc, yy)) > year(compare_date),
    numident_status == "nm_nm_m" ~ ceiling_date(ymd(paste0(cc, yy, "-", mm, "-01")), "month") - days(1) > compare_date
  )
}
numident_date_gte <- function(numident_status, cc, yy, mm, dd, numident_date, compare_date) {
  case_when(
    numident_status == "nm_nm_nm" ~ numident_date >= compare_date,
    numident_status == "nm_m_m" ~ as.numeric(paste0(cc, yy)) >= year(compare_date),
    numident_status == "nm_m_nm" ~ as.numeric(paste0(cc, yy)) > year(compare_date) | (as.numeric(paste0(cc, yy)) == year(compare_date) & as.numeric(dd) == day(compare_date)),
    numident_status == "nm_nm_m" ~ ceiling_date(ymd(paste0(cc, yy, "-", mm, "-01")), "month") - days(1) >= compare_date
  )
}

# Compare how often the death dates match.
dod_match <-
  mdac_numident %>%
  lazy_dt() %>%
  select(pik, RDATE, dod, matchstat, numident_dod, dodcc, dodyy, dodmm, doddd, numident_dod_status) %>%
  mutate(
    mortality_status =
      case_when(
        matchstat == "alive" & numident_dod_status == "missing" ~ "alive",
        matchstat == "alive" &
          numident_dod_status != "missing" &
          numident_date_gt(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, ymd("2015-12-31")) ~
          "died after MDAC",
        matchstat == "alive" &
          numident_dod_status != "missing" &
          numident_date_lte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, ymd("2015-12-31")) &
          numident_date_gte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "error, died during MDAC (but after or during interview)",
        matchstat == "alive" &
          numident_dod_status != "missing" &
          numident_date_lt(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "error, died during MDAC (before interview)",
        matchstat == "dead" &
          numident_dod_status != "missing" &
          numident_date_equal(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, dod) ~
          "death dates match",
        matchstat == "dead" &
          numident_dod_status != "missing" &
          numident_date_gte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "death dates do not match, numident death happened after/during interview",
        matchstat == "dead" &
          numident_dod_status != "missing" &
          numident_date_lt(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "death dates do not match, numident death happened before interview",
        matchstat == "dead" & numident_dod_status == "missing" ~ "dead in MDAC, alive in Numident",
        matchstat == "could_not_find_dod" & numident_dod_status == "missing" ~ "error in MDAC, alive(?) in Numident",
        matchstat == "could_not_find_dod" &
          numident_dod_status != "missing" &
          numident_date_gt(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, ymd("2015-12-31")) ~
          "error in MDAC, dead in Numident after interview date",
        matchstat == "could_not_find_dod" &
          numident_dod_status != "missing" &
          numident_date_lte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, ymd("2015-12-31")) &
          numident_date_gte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "error in MDAC, dead in Numident during MDAC but after/during interview date",
        matchstat == "could_not_find_dod" &
          numident_dod_status != "missing" &
          numident_date_lt(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "error in MDAC, dead in Numident before interview date",
        matchstat == "died_before_interview" & numident_dod_status == "missing" ~ "error in MDAC (interview), alive(?) in Numident",
        matchstat == "died_before_interview" &
          numident_dod_status != "missing" &
          numident_date_lte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, dod) ~
          "error in both MDAC and Numident",
        matchstat == "died_before_interview" &
          numident_dod_status != "missing" &
          numident_date_gte(numident_dod_status, dodcc, dodyy, dodmm, doddd, numident_dod, RDATE) ~
          "Numident fixed error in MDAC?"
      )
  ) %>%
  as_tibble()

# How different are the death dates which do not match?
dod_match_diff <-
  dod_match %>%
  filter(mortality_status == "death dates do not match, numident death happened after/during interview") %>%
  mutate(
    dod_diff =
      case_when(
        numident_dod_status == "nm_nm_nm" ~ abs(time_length(interval(dod, numident_dod), "day")),
        numident_dod_status == "nm_nm_m" ~ abs(time_length(interval(ym(paste0(year(dod), month(dod))), ym(paste0(dodcc, dodyy, "-", dodmm))), "day"))
      )
  )

# Compare how often the date of births match.
dob_match <-
  mdac_numident %>%
  lazy_dt() %>%
  select(pik, RDATE, dob, numident_dob, dobcc, dobyy, dobmm, dobdd, numident_dob_status) %>%
  mutate(
    birth_status =
      case_when(
        numident_date_equal(numident_dob_status, dobcc, dobyy, dobmm, dobdd, numident_dob, dob) ~ "birth dates match",
        numident_date_lte(numident_dob_status, dobcc, dobyy, dobmm, dobdd, numident_dob, RDATE) ~ "birth dates do no match, born before/during interview date",
        numident_date_gt(numident_dob_status, dobcc, dobyy, dobmm, dobdd, numident_dob, RDATE) ~ "birth dates do not match, numident happened after interview date",
        numident_dob_status == "missing" ~ "Numident DOB is missing"
      )
  ) %>%
  as_tibble()

dob_match_diff <-
  dob_match %>%
  filter(birth_status == "birth dates do no match, born before/during interview date") %>%
  mutate(
    dob_diff =
      case_when(
        numident_dob_status == "nm_nm_nm" ~ abs(time_length(interval(dob, numident_dob), "day")),
        numident_dob_status == "nm_nm_m" ~ abs(time_length(interval(ym(paste0(year(dob), month(dob))), ym(paste0(dobcc, dobyy, "-", dobmm))), "day")),
        numident_dob_status %in% c("nm_m_nm", "nm_m_m") ~ abs((year(dob) - as.numeric(paste0(dobcc, dobyy))) * 365.25)
      )
  )

# Join results together and keep only valid entries.
join_dob_dod <-
  dob_match %>%
  select(pik, numident_dob_status, birth_status) %>%
  full_join(select(dod_match, pik, numident_dod_status, mortality_status), by = "pik") %>%
  full_join(select(dob_match_diff, pik, dob_diff), by = "pik") %>%
  full_join(select(dod_match_diff, pik, dod_diff), by = "pik") %>%
  full_join(select(mdac_numident, pik, CMID, PNUM, matchstat, dob, dod, numident_dob, numident_dod, RDATE), by = "pik")

# Thankfully, all the incomplete dates in Numident are on perfect matches.
# All death dates occur after/during interview date.
# All birth dates occur before/during interview date.
keep <-
  join_dob_dod %>%
  filter(
    (
      (birth_status == "birth dates match" & mortality_status == "alive") |
      (birth_status == "birth dates match" & mortality_status == "death dates match") |
      (birth_status == "birth dates match" & mortality_status == "died after MDAC") |
      (dob_diff <= 366 & mortality_status == "alive") |
      (dob_diff <= 366 & mortality_status == "death dates match") |
      (dob_diff <= 366 & mortality_status == "died after MDAC") |
      (birth_status == "birth dates match" & dod_diff <= 366) |
      (dob_diff <= 366 & dod_diff <= 366)
    ) &
    (matchstat == "alive" | matchstat == "dead")
  ) %>%
  mutate(
    dob_final = if_else(birth_status == "birth dates match", dob, numident_dob),
    dod_final =
      case_when(
        mortality_status == "alive" ~ NA_Date_,
        mortality_status == "death dates match" ~ dod,
        mortality_status == "died after MDAC" ~ numident_dod,
        mortality_status == "death dates do not match, numident death happened after/during interview" ~ numident_dod
      )
  ) %>%
  select(pik, CMID, PNUM, dod_final, dob_final) %>%
  rename(dod = dod_final, dob = dob_final)

drop <-
  join_dob_dod %>%
  filter(
    birth_status == "birth dates do not match, numident happened after interview date" |
    birth_status == "Numident DOB is missing" |
    dob_diff > 366 |
    mortality_status == "error, died during MDAC (but after or during interview)" |
    mortality_status == "error, died during MDAC (before interview)" |
    mortality_status == "death dates do not match, numident death happened before interview" |
    mortality_status == "dead in MDAC, alive in Numident" |
    dod_diff > 366 |
    matchstat == "died_before_interview" |
    matchstat == "could_not_find_dod"
  )

################################################################################
# THIRD: Drop individuals who were incarcerated in MDAC but have no CJARS ID.
################################################################################
mdac_gqtyp <-
  read_sas(
    file.path("mdac2008_2015_v1.sas7bdat"),
    col_select = c("CMID", "PNUM", "GQMAJTYP")
  ) %>%
  mutate(
    group_quarters =
      case_when(
        GQMAJTYP == "" ~ "Not in a group quarters",
        GQMAJTYP == "1" ~ "Adult correctional facility",
        GQMAJTYP == "2" ~ "Juvenile facilities",
        GQMAJTYP == "3" ~ "Nursing facilities",
        GQMAJTYP == "4" ~ "Other health care facilities",
        GQMAJTYP == "5" ~ "College/university student housing",
        GQMAJTYP == "6" ~ "Military quarters/military ships",
        GQMAJTYP == "7" ~ "Other noninstitutional facilities"
      )
  ) %>%
  select(-GQMAJTYP)

mdac_cjars <-
  left_join(keep, mdac_gqtyp, by = c("CMID", "PNUM")) %>%
  left_join(crosswalk, by = c("pik", "CMID", "PNUM")) %>%
  filter(group_quarters != "Adult correctional facility" | !is.na(CJARS_ID))
  
################################################################################
# FOURTH: Collapse CJARS records into one row per person (i.e., per PIK).
################################################################################
arrests <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_arrest_rsch.sas7bdat"),
    col_select = c("CJARS_ID")
  )

adj <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_adjud_rsch.sas7bdat"),
    col_select = c("CJARS_ID")
  )

incar <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_incar_rsch.sas7bdat"),
    col_select = c("CJARS_ID")
  )

probat <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_probat_rsch.sas7bdat"),
    col_select = c("CJARS_ID")
  )

parole <-
  read_sas(
    file.path(cjars_dir, "um_cjars_2023q3_parole_rsch.sas7bdat"),
    col_select = c("CJARS_ID")
  )

mdac_cjars_clean <-
  mdac_cjars %>%
  lazy_dt() %>%
  mutate(
    in_arr = CJARS_ID %in% arrests$CJARS_ID,
    in_adj = CJARS_ID %in% adj$CJARS_ID,
    in_incar = CJARS_ID %in% incar$CJARS_ID,
    in_probat = CJARS_ID %in% probat$CJARS_ID,
    in_parole = CJARS_ID %in% parole$CJARS_ID,
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
# FIFTH: Combine final sample with full MDAC data (and sub-sample focal states).
################################################################################
# MDAC
mdac_full <-
  read_sas(
    file.path("mdac2008_2015_v1.sas7bdat"),
    col_select =
      c(
        "CMID", "PNUM", "ZIP", "MEMI", "UR", "ST", "CTY", "TR", "BLK", "SEX",
        "IMPRC", "HSGP", "NP", "REL", "HHT", "PAOC", "SFR", "PSF", "NR", "R18",
        "R65", "MAR", "SCHL", "CIT", "HHL", "ENG", "ESR", "COW", "OCC", "OCCG",
        "IND", "indg", "WKL", "WKH", "WKW", "JWD", "JWMN", "PINC", "POVPI",
        "HINC", "FINC", "WIF", "FS", "HICOV", "HINS4", "DIS", "MIL", "MIG",
        "MVY", "TEN", "VAL", "MRG", "MH", "INS", "GRNT", "GRPI", "BLD", "YBL",
        "RMS", "BDS", "PLM", "KIT", "TEL", "GAS", "ELE", "FUL", "WAT", "ucause",
        "cause113", "mdac_wgt"
      )
  ) %>%
  left_join(cause_of_death_crosswalk, by = "cause113") %>%
  mutate(
    icd_code_alpha = str_sub(ucause, 1, 1),
    icd_code_num =
      if_else(
        str_sub(ucause, 2, 2) == "0",
        str_sub(ucause, 3, 3),
        str_sub(ucause, 2, 3)
      ),
    cause114_label = 
      case_when(
        (icd_code_alpha == "X" & icd_code_num %in% c(40:44, 60:64)) | (icd_code_alpha == "Y" & icd_code_num %in% 10:14) ~ "Drug overdose",
        T ~ cause113_label
      ),
    cause114_condensed = 
      case_when(
        (icd_code_alpha == "X" & icd_code_num %in% c(40:44, 60:64)) | (icd_code_alpha == "Y" & icd_code_num %in% 10:14) ~ "Drug overdose",
        T ~ cause113_condensed
      ),
    cause5_label =
      case_when(
        cause114_condensed == "Assault (homicide)" ~ "Homicide",
        cause114_condensed == "Intentional self-harm" ~ "Suicide",
        cause114_condensed == "Drug overdose" ~ "Drug overdose",
        cause114_condensed == "Accidents (unintentional injuries)" ~ "Accident",
        cause114_condensed == "Alive" ~ "Alive",
        T ~ "Natural Causes"
      )
  ) %>%
  select(-icd_code_alpha, -icd_code_num)

################################################################################
# Clean MDAC data and make variables ready for analysis.
################################################################################
mdac_cjars_full_clean <-
  left_join(mdac_cjars_clean, mdac_full, by = c("CMID", "PNUM")) %>%
  lazy_dt() %>%
  mutate(
    zip = if_else(ZIP == "", NA_character_, ZIP),
    memi =
      case_when(
        MEMI == 1 ~ "metro",
        MEMI == 2 ~ "micro",
        MEMI == 9 ~ "neither"
      ),
    state_fips = str_sub(ST, 2, 3),
    state =
      case_when(
        ST == "001" ~ "AL",
        ST == "002" ~ "AK",
        ST == "004" ~ "AZ",
        ST == "005" ~ "AR",
        ST == "006" ~ "CA",
        ST == "008" ~ "CO",
        ST == "009" ~ "CT",
        ST == "010" ~ "DE",
        ST == "011" ~ "DC",
        ST == "012" ~ "FL",
        ST == "013" ~ "GA",
        ST == "015" ~ "HI",
        ST == "016" ~ "ID",
        ST == "017" ~ "IL",
        ST == "018" ~ "IN",
        ST == "019" ~ "IA",
        ST == "020" ~ "KS",
        ST == "021" ~ "KY",
        ST == "022" ~ "LA",
        ST == "023" ~ "ME",
        ST == "024" ~ "MD",
        ST == "025" ~ "MA",
        ST == "026" ~ "MI",
        ST == "027" ~ "MN",
        ST == "028" ~ "MS",
        ST == "029" ~ "MO",
        ST == "030" ~ "MT",
        ST == "031" ~ "NE",
        ST == "032" ~ "NV",
        ST == "033" ~ "NH",
        ST == "034" ~ "NJ",
        ST == "035" ~ "NM",
        ST == "036" ~ "NY",
        ST == "037" ~ "NC",
        ST == "038" ~ "ND",
        ST == "039" ~ "OH",
        ST == "040" ~ "OK",
        ST == "041" ~ "OR",
        ST == "042" ~ "PA",
        ST == "044" ~ "RI",
        ST == "045" ~ "SC",
        ST == "046" ~ "SD",
        ST == "047" ~ "TN",
        ST == "048" ~ "TX",
        ST == "049" ~ "UT",
        ST == "050" ~ "VT",
        ST == "051" ~ "VA",
        ST == "053" ~ "WA",
        ST == "054" ~ "WV",
        ST == "055" ~ "WI",
        ST == "056" ~ "WY"
      ),
    sex = if_else(SEX == 1, "male", "female"),
    race_long =
      case_when(
        IMPRC == "01" & HSGP == 1 ~ "white_nh",
        IMPRC == "02" & HSGP == 1 ~ "black_nh",
        IMPRC == "03" & HSGP == 1 ~ "aian_nh",
        IMPRC == "04" & HSGP == 1 ~ "asian_nh",
        IMPRC == "05" & HSGP == 1 ~ "nhopi_nh",
        !(IMPRC %in% c("01", "02", "03", "04", "05")) & HSGP == 1 ~ "multi_nh",
        IMPRC == "01" & HSGP != 1 ~ "white_hisp",
        IMPRC == "02" & HSGP != 1 ~ "black_hisp",
        IMPRC == "03" & HSGP != 1 ~ "aian_hisp",
        IMPRC == "04" & HSGP != 1 ~ "asian_hisp",
        IMPRC == "05" & HSGP != 1 ~ "nhopi_hisp",
        !(IMPRC %in% c("01", "02", "03", "04", "05")) & HSGP != 1 ~ "multi_hisp"
      ),
    race_short =
      case_when(
        IMPRC == "01" & HSGP == 1 ~ "white_nh",
        IMPRC == "02" & HSGP == 1 ~ "black_nh",
        IMPRC == "03" & HSGP == 1 ~ "aian_nh",
        IMPRC == "04" & HSGP == 1 ~ "asian_nh",
        IMPRC == "05" & HSGP == 1 ~ "nhopi_nh",
        !(IMPRC %in% c("01", "02", "03", "04", "05")) & HSGP == 1 ~ "multi_nh",
        HSGP != 1 ~ "hispanic"
      ),
    rel =
      case_when(
        REL == "" ~ NA_character_,
        REL == "00" ~ "reference_person",
        REL == "01" ~ "spouse",
        REL == "02" ~ "bio_child",
        REL == "03" ~ "adopted_child",
        REL == "04" ~ "step_child",
        REL == "05" ~ "sibling",
        REL == "06" ~ "parent",
        REL == "07" ~ "grandchild",
        REL == "08" ~ "parent_in_law",
        REL == "09" ~ "child_in_law",
        REL == "10" ~ "other_relative",
        REL == "11" ~ "roomer_or_boarder",
        REL == "12" ~ "housemate_or_roommate",
        REL == "13" ~ "unmarried_partner",
        REL == "14" ~ "foster_child",
        REL == "15" ~ "other_nonrelative"
    ),
    hht =
      case_when(
        HHT == "" ~ NA_character_,
        HHT == "1" ~ "married_couple_family_household",
        HHT == "2" ~ "male_headed_family_household",
        HHT == "3" ~ "female_headed_family_household",
        HHT == "4" ~ "male_head_live_alone_nonfamily_household",
        HHT == "5" ~ "male_head_live_not_alone_nonfamily_household",
        HHT == "6" ~ "female_head_live_alone_nonfamily_household",
        HHT == "7" ~ "female_head_live_not_alone_nonfamily_household"
      ),
    paoc = 
      case_when(
        PAOC == "" ~ NA_character_,
        PAOC == "1" ~ "female_own_child_under_6_only",
        PAOC == "2" ~ "female_own_child_6_17_only",
        PAOC == "3" ~ "female_own_child_under_6_or_6_17",
        PAOC == "4" ~ "female_no_own_children"
      ),
    sfr = 
      case_when(
        SFR == "" ~ NA_character_,
        SFR == 1 ~ "spouse_no_children",
        SFR == 2 ~ "spouse_with_children",
        SFR == 3 ~ "parent_in_parentchid_subfamily",
        SFR == 4 ~ "child_in_marriedcouple_subfamily",
        SFR == 5 ~ "child_in_motherchild_subfamily",
        SFR == 6 ~ "child_in_fatherchild_subfamily"
      ),
    psf = 
      case_when(
        PSF == "" ~ NA_character_,
        PSF == "0" ~ "no_subfamily",
        PSF == "1" ~ "one_or_more_subfamily"
      ),
    nr = 
      case_when(
        NR == "" ~ NA_character_,
        NR == "0" ~ "no_nonrelative",
        NR == "1" ~ "one_or_more_nonrelative"
      ),
    r18 =
      case_when(
        R18 == "" ~ NA_character_,
        R18 == "0" ~ "no_under_18_in_household",
        R18 == "1" ~ "one_or_more_under_18_in_household"
      ),
    r65 =
      case_when(
        R65 == "" ~ NA_character_,
        R65 == "0" ~ "no_65_or_older_in_household",
        R65 == "1" ~ "one_65_or_older_in_household",
        R65 == "2" ~ "two_or_more_65_or_older_in_household"
      ),
    marriage =
      case_when(
        MAR == "1" ~ "married",
        MAR == "2" ~ "widowed",
        MAR == "3" ~ "divorced",
        MAR == "4" ~ "separated",
        MAR == "5" ~ "never_married"
      ),
    education =
      case_when(
        SCHL %in% c(paste0(0, 1:9), 10:15) ~ "less_than_high_school",
        SCHL == "16" ~ "high_school_diploma",
        SCHL == "17" ~ "ged",
        SCHL %in% c("18", "19") ~ "some_college_no_degree",
        SCHL == "20" ~ "associate",
        SCHL %in% 21:24 ~ "bachelor_or_higher"
      ),
    citizen =
      case_when(
        CIT %in% 1:3 ~ "born_in_usa",
        CIT == 4 ~ "naturalized_citizen",
        CIT == 5 ~ "not_a_citizen"
      ),
    hhl = 
      case_when(
        HHL == "" ~ NA_character_,
        HHL == "1" ~ "english_only",
        HHL %in% 2:5 ~ "non_english"
      ),
    english_ability = 
      case_when(
        ENG == "" ~ NA_character_,
        ENG == "1" ~ "very_well",
        ENG == "2" ~ "well",
        ENG == "3" ~ "not_well",
        ENG == "4" ~ "none"
      ),
    employment =
      case_when(
        ESR == "" ~ NA_character_,
        ESR %in% 1:2 ~ "employed",
        ESR == 3 ~ "unemployed",
        ESR %in% 4:5 ~ "armed_forces",
        ESR == 6 ~ "not_in_labor_force"
      ),
    cow =
      case_when(
        COW == "" ~ NA_character_,
        COW == 1 ~ "private_for_profit",
        COW == 2 ~ "private_not_for_profit",
        COW == 3 ~ "local_government",
        COW %in% 4:5 ~ "state_or_federal_government",
        COW %in% 6:7 ~ "self_employed",
        COW == 8 ~ "unpaid_family_worker",
        COW == 9 ~ "unemployed"
      ),
    occ = if_else(OCC == "", NA_character_, OCC),
    occg = 
      case_when(
        OCCG == "" ~ NA_character_,
        OCCG == "01" ~ "health_diagnostician_lawyer",
        OCCG == "02" ~ "scientist_professor",
        OCCG == "03" ~ "business_professional",
        OCCG == "04" ~ "engineer_mathematician",
        OCCG == "05" ~ "health_assessment",
        OCCG == "06" ~ "teacher_other_professional",
        OCCG == "07" ~ "health_engineering_technician",
        OCCG == "08" ~ "other_technician",
        OCCG == "09" ~ "sales_supervisor_rep",
        OCCG == "10" ~ "sales_worker",
        OCCG == "11" ~ "office_worker_admin",
        OCCG == "12" ~ "protective_service",
        OCCG == "13" ~ "service_worker",
        OCCG == "14" ~ "precision_production_mechanic",
        OCCG == "15" ~ "operator_fabricator",
        OCCG == "16" ~ "handler_laborer",
        OCCG == "17" ~ "farm_fish_forest_mining",
        OCCG == "99" ~ "other"
      ),
    ind = if_else(IND == "", NA_character_, IND),
    indg = 
      case_when(
        indg == "" ~ NA_character_,
        indg == "01" ~ "farm_fish_forest_hunting",
        indg == "02" ~ "mining_quarry_oil_gas",
        indg == "03" ~ "construction",
        indg == "04" ~ "manufacturing",
        indg == "05" ~ "wholesale_trade",
        indg == "06" ~ "retail_trade",
        indg == "07" ~ "transportation_warehousing",
        indg == "08" ~ "utilities",
        indg == "09" ~ "information",
        indg == "10" ~ "finance_insurance",
        indg == "11" ~ "real_estate_rental_leasing",
        indg == "12" ~ "professional_scientific_technical",
        indg == "13" ~ "management",
        indg == "14" ~ "admin_support_waste_management",
        indg == "15" ~ "education",
        indg == "16" ~ "health_care_social_assistance",
        indg == "17" ~ "arts_entertainment_recreation",
        indg == "18" ~ "accomodation_food_services",
        indg == "19" ~ "other_services",
        indg == "20" ~ "public_admin"
      ),
    wkl =
      case_when(
        WKL == "" ~ NA_character_,
        WKL == "1" ~ "last_worked_past_12_months",
        WKL == "2" ~ "last_worked_1_to_5_years_ago",
        WKL == "3" ~ "last_worked_over_5_years_ago_or_never"
      ),
    wkh = case_when(
      WKH == "" ~ NA_real_,
      WKH != "" ~ as.numeric(str_sub(WKH, 2, 2))
    ),
    wkw =
      case_when(
        WKW == "" ~ NA_character_,
        WKW == "1" ~ "50_to_52_weeks",
        WKW == "2" ~ "48_to_49_weeks",
        WKW == "3" ~ "40_to_47_weeks",
        WKW == "4" ~ "27_to_39_weeks",
        WKW == "5" ~ "14_to_26_weeks",
        WKW == "6" ~ "13_weeks_or_less"
      ),
    poverty = 
      case_when(
        POVPI == "" ~ NA_real_,
        POVPI %in% paste0("00", 0:9) ~ as.numeric(str_sub(POVPI, 3, 3)),
        POVPI %in% paste0("0", 10:99) ~ as.numeric(str_sub(POVPI, 2, 3)),
        POVPI %in% 100:999 ~ as.numeric(POVPI)
      ),
    health_insurance =
      case_when(
        is.na(HICOV) ~ NA_character_,
        HICOV == "1" ~ "has_health_insurance",
        HICOV == "2" ~ "uninsured"
      ),
    medicaid =
      case_when(
        is.na(HINS4) ~ NA_character_,
        HINS4 == "1" ~ "has_medicaid",
        HINS4 == "2" ~ "no_medicaid"
      ),
    disability =
      case_when(
        is.na(DIS) ~ NA_character_,
        DIS == "1" ~ "disability",
        DIS == "2" ~ "no_disability"
      ),
    ever_in_military =
      case_when(
        is.na(MIL) ~ NA_character_,
        MIL %in% 1:3 ~ "yes",
        MIL %in% 4:5 ~ "no"
      ),
    moved_past_year =
      case_when(
        is.na(MIG) ~ NA_character_,
        MIG == 1 ~ "no",
        MIG %in% 2:3 ~ "yes"
      ),
    workers_in_family =
      case_when(
        WIF == "" ~ NA_character_,
        WIF == 0 ~ "no_workers",
        WIF == 1 ~ "one_worker",
        WIF == 2 ~ "two_workers",
        WIF == 3 ~ "three_or_more_workers"
      ),
    food_stamps =
      case_when(
        FS == "" ~ NA_character_,
        FS == 1 ~ "receives_food_stamps",
        FS == 2 ~ "does_not_receive_food_stamps"
      ),
    home_ownership =
      case_when(
        TEN == "" ~ NA_character_,
        TEN == 1 ~ "owned_with_mortgage",
        TEN == 2 ~ "owned_without_mortgage",
        TEN == 3 ~ "renter",
        TEN == 4 ~ "occupied_without_rent"
      ),
    type_of_home =
      case_when(
        BLD == "" ~ NA_character_,
        BLD == "01" ~ "mobile_home",
        BLD == "02" ~ "detached_one_family_home",
        BLD == "03" ~ "attached_one_family_home",
        BLD %in% c(paste0("0", 4:9), 10) ~ "apartment_building"
      ),
    year_home_built =
      case_when(
        YBL == "" ~ NA_character_,
        YBL == "01" ~ "2008",
        YBL == "02" ~ "2007",
        YBL == "03" ~ "2006",
        YBL == "04" ~ "2005",
        YBL == "05" ~ "2000_to_2004",
        YBL == "06" ~ "1990_to_1999",
        YBL == "07" ~ "1980_to_1989",
        YBL == "08" ~ "1970_to_1979",
        YBL == "09" ~ "1960_to_1969",
        YBL == "10" ~ "1950_to_1959",
        YBL == "11" ~ "1940_to_1949",
        YBL == "12" ~ "1939_or_earlier"
      ),
    plumbing = 
      case_when(
        PLM == "" ~ NA_character_,
        PLM == 1 ~ "complete_plumbing",
        PLM == 2 ~ "not_complete_plumbing"
      ),
    kitchen = 
      case_when(
        KIT == "" ~ NA_character_,
        KIT == 1 ~ "complete_kitchen",
        KIT == 2 ~ "not_complete_kitchen"
      ),
    telephone_services =
      case_when(
        TEL == "" ~ NA_character_,
        TEL == 1 ~ "has_telephone_services",
        TEL == 2 ~ "no_telephone_services"
      ),
    monthly_gas_cost =
      case_when(
        is.na(GAS) ~ NA_character_,
        GAS %in% 1:2 ~ "included_in_rent_or_condofee_or_electricity",
        GAS == 3 ~ "does_not_use_gas_or_not_charged",
        GAS == 4 ~ as.character(GAS)
      ),
    monthly_elec_cost =
      case_when(
        is.na(ELE) ~ NA_character_,
        ELE == 1 ~ "included_in_rent_or_condofee",
        ELE == 2 ~ "does_not_use_elec_or_not_charged",
        ELE == 3 ~ as.character(ELE)
      ),
    yearly_other_fuel_cost =
      case_when(
        is.na(FUL) ~ NA_character_,
        FUL == 1 ~ "included_in_rent_or_condofee",
        FUL == 2 ~ "does_not_use_otherfuel_or_not_charged",
        FUL == 3 ~ as.character(FUL)
      ),
    yearly_water_cost =
      case_when(
        is.na(WAT) ~ NA_character_,
        WAT == 1 ~ "included_in_rent_or_condofee",
        WAT == 2 ~ "no_charge",
        WAT == 3 ~ as.character(WAT)
      )
  ) %>%
  select(
    -ST, -MEMI, -SEX, -IMPRC, -HSGP, -REL, -HHT, -PAOC, -SFR, -PSF, -NR, -R18,
    -R65, -MAR, -SCHL, -CIT, -HHL, -ENG, -ESR, -COW, -OCC, -OCCG, -IND, -WKL,
    -WKH, -WKW, -POVPI, -HICOV, -HINS4, -DIS, -MIL, -MIG, -WIF, -FS, -TEN, -BLD,
    -YBL, -PLM, -KIT, -TEL, -GAS, -ELE, -FUL, -WAT, -ZIP
  ) %>%
  rename_with(tolower) %>%
  as_tibble()

mdac_cjars_full_clean_focal_states <-
  mdac_cjars_full_clean %>%
  filter(state %in% focal_states)

################################################################################
# Save data.
################################################################################
write_csv(mdac_cjars_full_clean, file.path(output_dir, "8_mdac_wide.csv"))
write_csv(mdac_cjars_full_clean_focal_states, file.path(output_dir, "8_mdac_wide_focal_states.csv"))
write_dta(mdac_cjars_full_clean, file.path(output_dir, "8_mdac_wide.dta"))
write_dta(mdac_cjars_full_clean_focal_states, file.path(output_dir, "8_mdac_wide_focal_states.dta"))
