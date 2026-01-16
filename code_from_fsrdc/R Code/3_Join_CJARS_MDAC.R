library(readr)
library(dplyr)
library(dtplyr)

read_dir <- ""

################################################################################
# Read in files
cjars_pik <- read_csv(file.path(read_dir, "2_joined_cjars_pik.csv"))

mdac_pik <-
  read_csv(file.path(read_dir, "1_joined_mdac_pik.csv")) %>%
  lazy_dt(key_by = "pik")

################################################################################
# Join results
join_cjars_mdac <-
  cjars_pik %>%
  lazy_dt(key_by = "pik") %>%
  inner_join(mdac_pik, by = "pik") %>%
  as_tibble()

length_unique_state_pik <-
  join_cjars_mdac %>%
  group_by(SRC_ST) %>%
  summarise(length_unique_pik = length(unique(pik)))

length_unique_pik <- length(unique(join_cjars_mdac$pik))
table_n_pik <- table(count(join_cjars_mdac, pik)$n)
states <- table(join_cjars_mdac$SRC_ST)

mdac_sample <-
  cjars_pik %>%
  lazy_dt(key_by = "pik") %>%
  right_join(mdac_pik, by = "pik") %>%
  as_tibble()

length_unique_pik_mdac <- length(unique(mdac_sample$pik))

write_csv(
  mdac_sample,
  file.path(read_dir, "3_joined_cjars_mdac.csv")
)
