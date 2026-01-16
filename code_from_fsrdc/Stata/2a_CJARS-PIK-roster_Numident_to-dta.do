* Set directories
global cjars_dir ""
global numident_dir ""
global input_dir ""

* Read in CJARS_PIK crosswalk file and save to Stata format
cd "$cjars_dir"
import sas DQB_SOURCE_ID pik using "um_cjars_2023q3_roster_pvs.sas7bdat"
keep if pik != ""
save "${input_dir}cjars_pik_crosswalk.dta", replace
clear

* Read in Census Numident file and save to Stata format
cd "$numident_dir"
import sas pik dobcc dobyy pobst using "cnum_2024q1.sas7bdat"
save "${input_dir}numident.dta", replace
clear
