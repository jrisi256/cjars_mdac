library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)

focal_states <- c("FL", "MI", "NC", "TX", "WI")
out_dir <- here("cjars_data_availability", "output")

################################################################################
# Download CJARS file.
################################################################################
download.file(
  "https://raw.githubusercontent.com/umcjars/cjars_meta/refs/heads/main/CJARS_2023Q3_coverage_FSRDC.csv",
  here("cjars_data_availability", "input", "cjars_coverage.csv")
)

################################################################################
# Read in file.
################################################################################
# Read in coverage data and only keep primary sources of data.
cjars_coverage <-
  read_csv(here("cjars_data_availability", "input", "cjars_coverage.csv")) |>
  mutate(
    st_fips =
      case_when(
        st_fips == 12 ~ "FL",
        st_fips == 26 ~ "MI",
        st_fips == 37 ~ "NC",
        st_fips == 48 ~ "TX",
        st_fips == 55 ~ "WI",
        T ~ st_fips
      )
  ) |>
  filter(coverage == "primary")

nr_counties <-
  cjars_coverage |>
  distinct(st_fips, cnty_fips) |>
  count(st_fips, name = "nr_total_counties") |>
  filter(st_fips %in% focal_states)

################################################################################
# Get a sense of monthly coverage per state per CJARS table.
################################################################################
focal_coverage_monthly <-
  cjars_coverage |>
  filter(st_fips %in% focal_states) |>
  mutate(date_coverage = ym(paste0(dt_yyyy, "-", dt_mm))) |>
  count(st_fips, date_coverage, cjars_table, name = "nr_counties") |>
  full_join(nr_counties, by = "st_fips") |>
  arrange(st_fips, cjars_table, date_coverage) |>
  mutate(prcnt_coverage = nr_counties / nr_total_counties * 100)

plots_monthly <-
  map(
    focal_states,
    function(state, df) {
      df |>
        filter(st_fips == state) |>
        ggplot(aes(x = date_coverage, y = prcnt_coverage)) +
        geom_point() +
        geom_line() +
        facet_wrap(~st_fips + cjars_table, ncol = 1) +
        theme_bw() +
        labs(
          x = "Date (Month + Year)",
          y = "Percent of counties reporting in a given month"
        ) +
        scale_x_date(date_breaks = "3 years", date_labels = "%Y")
    },
    df = focal_coverage_monthly
  )
names(plots_monthly) <- focal_states
pwalk(
  list(plots_monthly, names(plots_monthly)),
  function(plot, name, dir) {
    ggsave(
      file.path(dir, paste0(name, "_monthly.png")),
      plot = plot, height = 12, width = 8
    )
  },
  dir = out_dir
)

################################################################################
# Get a sense of yearly coverage per state per CJARS table.
################################################################################
focal_coverage_yearly <-
  cjars_coverage |>
  filter(st_fips %in% focal_states) |>
  count(st_fips, dt_yyyy, cjars_table, name = "nr_county_months") |>
  full_join(nr_counties, by = "st_fips") |>
  mutate(nr_total_county_months = nr_total_counties * 12) |>
  arrange(st_fips, cjars_table, dt_yyyy) |>
  mutate(prcnt_coverage = nr_county_months / nr_total_county_months * 100)

plots_yearly <-
  map(
    focal_states,
    function(state, df) {
      df |>
        filter(st_fips == state) |>
        ggplot(aes(x = dt_yyyy, y = prcnt_coverage)) +
        geom_point() +
        geom_line() +
        facet_wrap(~st_fips + cjars_table, ncol = 1) +
        theme_bw() +
        labs(
          x = "Year",
          y = "Percent of county-months reporting in a given year"
        ) +
        scale_x_continuous(breaks = seq(1970, 2023, 3))
    },
    df = focal_coverage_yearly
  )
names(plots_yearly) <- focal_states
pwalk(
  list(plots_yearly, names(plots_yearly)),
  function(plot, name, dir) {
    ggsave(file.path(dir, paste0(name, "_yearly.png")), plot = plot, height = 12, width = 8)
  },
  dir = out_dir
)

################################################################################
# Save yearly results.
################################################################################
focal_coverage_save <-
  focal_coverage_yearly |>
  rename(state = st_fips, year = dt_yyyy, cjars = cjars_table) |>
  select(state, year, cjars, prcnt_coverage) |>
  mutate(
    coverage = if_else(prcnt_coverage < 100, "partial", "full"),
    cjars = tolower(cjars)
  )

adj_only <-
  focal_coverage_save |>
  filter(str_detect(cjars, "adj")) |>
  pivot_wider(
    id_cols = c("state", "year"),
    names_from = "cjars",
    values_from = c("coverage", "prcnt_coverage")
  ) |>
  rename(adj_fe = coverage_adj_fe, adj_mi = coverage_adj_mi) |>
  mutate(
    cjars = "adj",
    coverage = 
      case_when(
        adj_fe == "full" & is.na(adj_mi) ~ "fullFelony_noMisd",
        adj_fe == "partial" & is.na(adj_mi) ~ "partialFelony_noMisd",
        is.na(adj_fe) & adj_mi == "full" ~ "noFelony_fullMisd",
        is.na(adj_fe) & adj_mi == "partial" ~ "noFelony_partialMisd",
        adj_fe == "full" & adj_mi == "full" ~ "full",
        adj_fe == "full" & adj_mi == "partial" ~ "fullFelony_partialMisd",
        adj_fe == "partial" & adj_mi == "partial" ~ "partial",
        adj_fe == "partial" & adj_mi == "full" ~ "partialFelony_fullMisd",
      )
  ) |>
  select(-adj_fe, -adj_mi)

focal_coverage_save <-
  focal_coverage_save |>
  filter(cjars != "adj_mi" & cjars != "adj_fe") |>
  bind_rows(adj_only) |>
  arrange(state, cjars, year)

write_csv(focal_coverage_save, file.path(out_dir, "cjars_yearly_coverage.csv"))
