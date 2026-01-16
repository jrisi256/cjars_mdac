* Set directories
global crosswalk_dir ""
global input_dir ""
cd "$crosswalk_dir"

* Read in ACS crosswalk file and save to Stata format
import sas using "crosswalk_acs2008.sas7bdat"
drop VERFLG
save "${input_dir}crosswalk_acs2008.dta", replace
clear

* Read in ACS deleted files and save to Stata format
import sas using "crosswalk_acs2008_del2.sas7bdat"
drop verflg
save "${input_dir}crosswalk_acs2008_del2.dta", replace
clear

* Read in MDAC and save to Stata format
import sas CMID PNUM using "mdac2008_2015_v1.sas7bdat"
save "${input_dir}mdac.dta", replace

* Append ACS crosswalk files together
cd "$input_dir"
use crosswalk_acs2008
append using crosswalk_acs2008_del2

* Join crosswalk files with MDAC
merge 1:1 CMID PNUM using mdac

* Keep only 1-to-1 matches for (CMID, PNUM) to PIK
egen count_pik = count(pik), by(pik)
keep if count_pik == 1
drop count_pik _merge
save "1_joined_mdac_pik", replace
