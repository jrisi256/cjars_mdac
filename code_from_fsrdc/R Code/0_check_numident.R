library(haven)
library(dplyr)
library(readr)
library(dtplyr)
library(lubridate)

# Read in Numident.
numident <-
  read_sas(
    file.path("cnum_2024q1.sas7bdat"),
    col_select =
      c(
        "pik", "dobcc", "dobyy", "dobmm", "dobdd", "dodcc", "dodyy", "dodmm",
        "doddd", "num_dodcc", "num_dodyy", "num_dodmm", "num_doddd", "pobst"
      )
  ) %>%
  lazy_dt()

gc()

numident_max <-
  numident %>%
  as_tibble() %>%
  mutate(
    date_dob = ymd(paste0(dobcc, dobyy, "-", dobmm, "-", dobdd)),
    date_dod_census = ymd(paste0(dodcc, dodyy, "-", dodmm, "-", doddd)),
    date_dod_numident = ymd(paste0(num_dodcc, num_dodyy, "-", num_dodmm, "-", num_doddd))
  ) %>%
  summarise(
    max_dob = max(date_dob, na.rm = T),
    max_dod_census = max(date_dod_census, na.rm = T),
    max_dod_numident = max(date_dod_numident, na.rm = T)
  ) %>%
  as_tibble()

gc()

write_csv(
  numident_max,
  file.path("numident_max.csv")
)

numident_nomatch <-
  numident %>%
  mutate(
    cc_match = (is.na(dodcc) & is.na(num_dodcc)) | dodcc == num_dodcc,
    yy_match = (is.na(dodyy) & is.na(num_dodyy)) | dodyy == num_dodyy,
    mm_match = (is.na(dodmm) & is.na(num_dodmm)) | dodmm == num_dodmm,
    dd_match = (is.na(doddd) & is.na(num_doddd)) | doddd == num_doddd
  ) %>%
  filter(!cc_match | !yy_match | !mm_match | !dd_match) %>%
  as_tibble()

gc()

write_csv(
  numident_nomatch,
  file.path("numident_nomatch.csv")
)
