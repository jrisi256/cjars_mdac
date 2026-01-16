library(readr)
library(dplyr)

table_dir <- ""
bootstrap_sig1 <-
  read_csv(file.path(table_dir, "bootstrap_sig_age_adjusted_mortality_1.csv"))

bootstrap_sig2 <-
  read_csv(file.path(table_dir, "bootstrap_sig_age_adjusted_mortality_2.csv")) %>%
  rename_with(function(col) {paste0(col, "_2")}, matches("bootstrap|original"))

bootstrap_compare <-
  bootstrap_sig1 %>% full_join(bootstrap_sig2, by = c("cod", "race", "sex", "measure")) %>%
  relocate(basic_bootstrap_2, .after = basic_bootstrap) %>%
  relocate(basic_original_2, .after = basic_original) %>%
  relocate(normal_bootstrap_2, .after = normal_bootstrap) %>%
  relocate(normal_original_2, .after = normal_original) %>%
  relocate(percent_bootstrap_2, .after = percent_bootstrap) %>%
  relocate(percent_original_2, .after = percent_original) %>%
  mutate(
    basic_bootstrap_match = basic_bootstrap == basic_bootstrap_2,
    basic_original_match = basic_original == basic_original_2,
    normal_bootstrap_match = normal_bootstrap == normal_bootstrap_2,
    normal_original_match = normal_original == normal_original_2,
    percent_bootstrap_match = percent_bootstrap == percent_bootstrap_2,
    percent_original_match = percent_original == percent_original_2,
  )

write_csv(bootstrap_compare, file.path(table_dir, "bootstrap_compare.csv"))
