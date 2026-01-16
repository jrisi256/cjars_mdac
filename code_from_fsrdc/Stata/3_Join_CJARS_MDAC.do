* Set directories
global read_dir ""

* Read in CJARS_PIK data
cd "$read_dir"
use 2b_joined_cjars_pik

* Join CJARS with MDAC
merge m:1 pik using 1_joined_mdac_pik, keep(match)

* keep the entire MDAC sample regardless of whether or not they're in CJARS
clear
use 2b_joined_cjars_pik
joinby pik using 1_joined_mdac_pik, unmatched(using)
drop _merge

* Save resulting table
save "3_joined_cjars_mdac", replace
