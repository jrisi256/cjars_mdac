library(readr)
library(haven)
library(dplyr)
library(dtplyr)
library(lubridate)

################################################################################
# Read in MDAC files and clean up the demographic variables.
################################################################################
output_dir <- ""
input_dir <- ""

mdac_demog <-
  read_sas(
    file.path("mdac2008_2015_v1.sas7bdat"),
    col_select =
      c(
        "CMID", "PNUM", "ST", "matchstat", "ucause", "cause113", "sod", "dod",
        "SEX", "IMPRC", "HSGP", "POB", "DBD", "DBM", "DBY", "AGE", "RDATE",
        "mdac_wgt", "GQMAJTYP"
      )
  )

################################################# Recode the variables fro MDAC.
mdac_demog_recode <-
  mdac_demog %>%
  mutate(
    ST =
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
        ST == "056" ~ "WY",
      ),
    matchstat =
      case_when(
        matchstat == 0 ~ "no_match_ndi",
        matchstat == 1 ~ "date_error",
        matchstat == 2 ~ "dead",
        matchstat == 3 ~ "alive"
      ),
    sod =
      case_when(
        sod == "001" ~ "AL",
        sod == "002" ~ "AK",
        sod == "003" ~ "AZ",
        sod == "004" ~ "AR",
        sod == "005" ~ "CA",
        sod == "006" ~ "CO",
        sod == "007" ~ "CT",
        sod == "008" ~ "DE",
        sod == "009" ~ "DC",
        sod == "010" ~ "FL",
        sod == "011" ~ "GA",
        sod == "012" ~ "HI",
        sod == "013" ~ "ID",
        sod == "014" ~ "IL",
        sod == "015" ~ "IN",
        sod == "016" ~ "IA",
        sod == "017" ~ "KS",
        sod == "018" ~ "KY",
        sod == "019" ~ "LA",
        sod == "020" ~ "ME",
        sod == "021" ~ "MD",
        sod == "022" ~ "MA",
        sod == "023" ~ "MI",
        sod == "024" ~ "MN",
        sod == "025" ~ "MS",
        sod == "026" ~ "MO",
        sod == "027" ~ "MT",
        sod == "028" ~ "NE",
        sod == "029" ~ "NV",
        sod == "030" ~ "NH",
        sod == "031" ~ "NJ",
        sod == "032" ~ "NM",
        sod == "033" ~ "NY",
        sod == "33C" ~ "NY",
        sod == "034" ~ "NC",
        sod == "035" ~ "ND",
        sod == "036" ~ "OH",
        sod == "037" ~ "OK",
        sod == "038" ~ "OR",
        sod == "039" ~ "PA",
        sod == "040" ~ "RI",
        sod == "041" ~ "SC",
        sod == "042" ~ "SD",
        sod == "043" ~ "TN",
        sod == "044" ~ "TX",
        sod == "045" ~ "UT",
        sod == "046" ~ "VT",
        sod == "047" ~ "VA",
        sod == "048" ~ "WA",
        sod == "049" ~ "WV",
        sod == "050" ~ "WI",
        sod == "051" ~ "WY",
        sod %in% c("052", "053") ~ "USA territory",
        sod == "" ~ "alive"
      ),
    dod = ymd("1960-01-01") + days(dod),
    SEX = case_when(SEX == 1 ~ "Male", SEX == 2 ~ "Female"),
    race_ethnicity_long =
      case_when(
        IMPRC == "01" & HSGP == 1 ~ "White alone, not Hispanic",
        IMPRC == "02" & HSGP == 1 ~ "Black alone, not Hispanic",
        IMPRC == "03" & HSGP == 1 ~ "American Indian/Alaskan Native alone, not Hispanic",
        IMPRC == "04" & HSGP == 1 ~ "Asian alone, not Hispanic",
        IMPRC == "05" & HSGP == 1 ~ "Native Hawaiian/Other Pacific Islander alone, not Hispanic",
        !(IMPRC %in% c("01", "02", "03", "04", "05")) & HSGP == 1 ~ "Multiracial, not Hispanic",
        IMPRC == "01" & HSGP != 1 ~ "White alone, Hispanic",
        IMPRC == "02" & HSGP != 1 ~ "Black alone, Hispanic",
        IMPRC == "03" & HSGP != 1 ~ "American Indian/Alaskan Native alone, Hispanic",
        IMPRC == "04" & HSGP != 1 ~ "Asian alone, Hispanic",
        IMPRC == "05" & HSGP != 1 ~ "Native Hawaiian/Other Pacific Islander alone, Hispanic",
        !(IMPRC %in% c("01", "02", "03", "04", "05")) & HSGP != 1 ~ "Multiracial, Hispanic",
      ),
    race_ethnicity_short =
      case_when(
        IMPRC == "01" & HSGP == 1 ~ "White alone, not Hispanic",
        IMPRC == "02" & HSGP == 1 ~ "Black alone, not Hispanic",
        IMPRC == "03" & HSGP == 1 ~ "American Indian/Alaskan Native alone, not Hispanic",
        IMPRC == "04" & HSGP == 1 ~ "Asian alone, not Hispanic",
        IMPRC == "05" & HSGP == 1 ~ "Native Hawaiian/Other Pacific Islander alone, not Hispanic",
        !(IMPRC %in% c("01", "02", "03", "04", "05")) & HSGP == 1 ~ "Multiracial, not Hispanic",
        HSGP != 1 ~ "Hispanic",
      ),
    POB =
      case_when(
        POB == "001" ~ "AL",
        POB == "002" ~ "AK",
        POB == "004" ~ "AZ",
        POB == "005" ~ "AR",
        POB == "006" ~ "CA",
        POB == "008" ~ "CO",
        POB == "009" ~ "CT",
        POB == "010" ~ "DE",
        POB == "011" ~ "DC",
        POB == "012" ~ "FL",
        POB == "013" ~ "GA",
        POB == "015" ~ "HI",
        POB == "016" ~ "ID",
        POB == "017" ~ "IL",
        POB == "018" ~ "IN",
        POB == "019" ~ "IA",
        POB == "020" ~ "KS",
        POB == "021" ~ "KY",
        POB == "022" ~ "LA",
        POB == "023" ~ "ME",
        POB == "024" ~ "MD",
        POB == "025" ~ "MA",
        POB == "026" ~ "MI",
        POB == "027" ~ "MN",
        POB == "028" ~ "MS",
        POB == "029" ~ "MO",
        POB == "030" ~ "MT",
        POB == "031" ~ "NE",
        POB == "032" ~ "NV",
        POB == "033" ~ "NH",
        POB == "034" ~ "NJ",
        POB == "035" ~ "NM",
        POB == "036" ~ "NY",
        POB == "037" ~ "NC",
        POB == "038" ~ "ND",
        POB == "039" ~ "OH",
        POB == "040" ~ "OK",
        POB == "041" ~ "OR",
        POB == "042" ~ "PA",
        POB == "044" ~ "RI",
        POB == "045" ~ "SC",
        POB == "046" ~ "SD",
        POB == "047" ~ "TN",
        POB == "048" ~ "TX",
        POB == "049" ~ "UT",
        POB == "050" ~ "VT",
        POB == "051" ~ "VA",
        POB == "053" ~ "WA",
        POB == "054" ~ "WV",
        POB == "055" ~ "WI",
        POB == "056" ~ "WY",
        POB %in% paste0("0", 60:96) ~ "USA territory",
        POB %in% 100:554 ~ "Outside USA"
      ),
    dob = ymd(paste0(DBY, "-", DBM, "-", DBD)),
    GQMAJTYP = 
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
  select(-DBY, -DBM, -DBD, -HSGP, -IMPRC)

# Age and DOB match. We only need to keep DOB.
compare_age <-
  mdac_demog_recode %>%
  mutate(
    age_check = floor(time_length(interval(dob, ymd(RDATE)), "year")),
    age_match = age_check == AGE
  )

mdac_demog_recode <- mdac_demog_recode %>% select(-AGE, -RDATE)

# Save results
write_csv(
  mdac_demog_recode,
  file = file.path(input_dir, "mdac_demog_recode.csv")
)
