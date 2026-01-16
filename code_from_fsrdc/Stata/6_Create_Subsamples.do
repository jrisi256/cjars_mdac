global output_dir ""
global mortality_dir ""
global input_dir ""

********************************************************************************
* Save cause113 crosswalk to .dta file format
********************************************************************************
cd "$input_dir"
import delimited cause113_ucause_crosswalk, stringcols(1)
save "${input_dir}cause113_ucause_crosswalk", replace
clear

********************************************************************************
* Read in sample.
********************************************************************************
cd "$output_dir"
use 5_mdac_cjars_demographics

* Join with crosswalk file to get cause113 labels.
joinby using "${input_dir}cause113_ucause_crosswalk", unmatched(master)
drop _merge

* Recode causes of death.
gen cause113_new = cause113_label

replace cause113_new = "Drug overdose" if usubstr(ucause, 1, 3) == "X40" | usubstr(ucause, 1, 3) == "X41" | usubstr(ucause, 1, 3) == "X42" | usubstr(ucause, 1, 3) == "X43" | usubstr(ucause, 1, 3) == "X44" | usubstr(ucause, 1, 3) == "X60" | usubstr(ucause, 1, 3) == "X61" | usubstr(ucause, 1, 3) == "X62" | usubstr(ucause, 1, 3) == "X63" | usubstr(ucause, 1, 3) == "X64" | usubstr(ucause, 1, 3) == "Y10" | usubstr(ucause, 1, 3) == "Y11" | usubstr(ucause, 1, 3) == "Y12" | usubstr(ucause, 1, 3) == "Y13" | usubstr(ucause, 1, 3) == "Y14"

gen cause113_new_condensed = cause113_condensed

replace cause113_new_condensed = "Drug overdose" if usubstr(ucause, 1, 3) == "X40" | usubstr(ucause, 1, 3) == "X41" | usubstr(ucause, 1, 3) == "X42" | usubstr(ucause, 1, 3) == "X43" | usubstr(ucause, 1, 3) == "X44" | usubstr(ucause, 1, 3) == "X60" | usubstr(ucause, 1, 3) == "X61" | usubstr(ucause, 1, 3) == "X62" | usubstr(ucause, 1, 3) == "X63" | usubstr(ucause, 1, 3) == "X64" | usubstr(ucause, 1, 3) == "Y10" | usubstr(ucause, 1, 3) == "Y11" | usubstr(ucause, 1, 3) == "Y12" | usubstr(ucause, 1, 3) == "Y13" | usubstr(ucause, 1, 3) == "Y14"

********************************************************************************
* Those w/ a CJARS ID but no record count are dropped. MAIN SAMPLE.
********************************************************************************
gen cj_pre2015_contact = sum_pre2015_arr > 0 | sum_pre2015_adj > 0 | sum_pre2015_inc > 0 | sum_pre2015_pro > 0 | sum_pre2015_par > 0 | group_quarters == "Adult correctional facility"
gen cj_pre2015_contact_in_fs = (SRC_ST == "FL" | SRC_ST == "MI" | SRC_ST == "NC" | SRC_ST == "TX" | SRC_ST == "WI") & cj_pre2015_contact
drop if sum_arr == 0 & sum_adj == 0 & sum_inc == 0 & sum_pro == 0 & sum_par == 0 & SRC_ST != "" & group_quarters != "Adult correctional facility"

********************************************************************************
* Combine CJ records from multiple states into one record (individual-level).
********************************************************************************
egen sum_arr_indvdl = sum(sum_arr), by(pik)
egen sum_adj_indvdl = sum(sum_adj), by(pik)
egen sum_inc_indvdl = sum(sum_inc), by(pik)
egen sum_pro_indvdl = sum(sum_pro), by(pik)
egen sum_par_indvdl = sum(sum_par), by(pik)
egen sum_pre2015_arr_indvdl = sum(sum_pre2015_arr), by(pik)
egen sum_pre2015_adj_indvdl = sum(sum_pre2015_adj), by(pik)
egen sum_pre2015_inc_indvdl = sum(sum_pre2015_inc), by(pik)
egen sum_pre2015_pro_indvdl = sum(sum_pre2015_pro), by(pik)
egen sum_pre2015_par_indvdl = sum(sum_pre2015_par), by(pik)
egen sum_post2015_arr_indvdl = sum(sum_post2015_arr), by(pik)
egen sum_post2015_adj_indvdl = sum(sum_post2015_adj), by(pik)
egen sum_post2015_inc_indvdl = sum(sum_post2015_inc), by(pik)
egen sum_post2015_pro_indvdl = sum(sum_post2015_pro), by(pik)
egen sum_post2015_par_indvdl = sum(sum_post2015_par), by(pik)
bysort pik: egen cj_pre2015_contact_indvdl = max(cj_pre2015_contact)
bysort pik: egen cj_pre2015_contact_in_fs_indvdl = max(cj_pre2015_contact_in_fs)
duplicates drop pik, force

drop sum_arr sum_adj sum_inc sum_pro sum_par sum_pre2015_arr sum_pre2015_inc sum_pre2015_pro sum_pre2015_par sum_post2015_arr sum_post2015_adj sum_post2015_inc sum_post2015_pro sum_post2015_par cj_pre2015_contact cj_pre2015_contact_in_fs SRC_ST

* Drop individuals whose pre/during 2015 CJ status is uncertain. If an
* individual did not have a pre/during 2015 CJ contact and they have at least
* one date-uncertain interaction, then their status is uncertain.
keep if cj_pre2015_contact_indvdl | (sum_post2015_arr_indvdl == sum_arr_indvdl & sum_post2015_adj_indvdl == sum_adj_indvdl & sum_post2015_inc_indvdl == sum_inc_indvdl & sum_post2015_pro_indvdl == sum_pro_indvdl & sum_post2015_par_indvdl == sum_par_indvdl)

* Save results
save "${mortality_dir}6_main_mortality_sample", replace
clear

********************************************************************************
* Read in sample.
********************************************************************************
cd "$output_dir"
use 5_mdac_cjars_demographics

* Join with crosswalk file to get cause113 labels.
joinby using "${input_dir}cause113_ucause_crosswalk", unmatched(master)
drop _merge

* Recode causes of death.
gen cause113_new = cause113_label

replace cause113_new = "Drug overdose" if usubstr(ucause, 1, 3) == "X40" | usubstr(ucause, 1, 3) == "X41" | usubstr(ucause, 1, 3) == "X42" | usubstr(ucause, 1, 3) == "X43" | usubstr(ucause, 1, 3) == "X44" | usubstr(ucause, 1, 3) == "X60" | usubstr(ucause, 1, 3) == "X61" | usubstr(ucause, 1, 3) == "X62" | usubstr(ucause, 1, 3) == "X63" | usubstr(ucause, 1, 3) == "X64" | usubstr(ucause, 1, 3) == "Y10" | usubstr(ucause, 1, 3) == "Y11" | usubstr(ucause, 1, 3) == "Y12" | usubstr(ucause, 1, 3) == "Y13" | usubstr(ucause, 1, 3) == "Y14"

gen cause113_new_condensed = cause113_condensed

replace cause113_new_condensed = "Drug overdose" if usubstr(ucause, 1, 3) == "X40" | usubstr(ucause, 1, 3) == "X41" | usubstr(ucause, 1, 3) == "X42" | usubstr(ucause, 1, 3) == "X43" | usubstr(ucause, 1, 3) == "X44" | usubstr(ucause, 1, 3) == "X60" | usubstr(ucause, 1, 3) == "X61" | usubstr(ucause, 1, 3) == "X62" | usubstr(ucause, 1, 3) == "X63" | usubstr(ucause, 1, 3) == "X64" | usubstr(ucause, 1, 3) == "Y10" | usubstr(ucause, 1, 3) == "Y11" | usubstr(ucause, 1, 3) == "Y12" | usubstr(ucause, 1, 3) == "Y13" | usubstr(ucause, 1, 3) == "Y14"

********************************************************************************
* Those w/ a CJARS ID but no record count are considered as having no contact.
********************************************************************************
gen cj_pre2015_contact = sum_pre2015_arr > 0 | sum_pre2015_adj > 0 | sum_pre2015_inc > 0 | sum_pre2015_pro > 0 | sum_pre2015_par > 0 | group_quarters == "Adult correctional facility"
gen cj_pre2015_contact_in_fs = (SRC_ST == "FL" | SRC_ST == "MI" | SRC_ST == "NC" | SRC_ST == "TX" | SRC_ST == "WI") & cj_pre2015_contact

********************************************************************************
* Combine CJ records from multiple states into one record (individual-level).
********************************************************************************
egen sum_arr_indvdl = sum(sum_arr), by(pik)
egen sum_adj_indvdl = sum(sum_adj), by(pik)
egen sum_inc_indvdl = sum(sum_inc), by(pik)
egen sum_pro_indvdl = sum(sum_pro), by(pik)
egen sum_par_indvdl = sum(sum_par), by(pik)
egen sum_pre2015_arr_indvdl = sum(sum_pre2015_arr), by(pik)
egen sum_pre2015_adj_indvdl = sum(sum_pre2015_adj), by(pik)
egen sum_pre2015_inc_indvdl = sum(sum_pre2015_inc), by(pik)
egen sum_pre2015_pro_indvdl = sum(sum_pre2015_pro), by(pik)
egen sum_pre2015_par_indvdl = sum(sum_pre2015_par), by(pik)
egen sum_post2015_arr_indvdl = sum(sum_post2015_arr), by(pik)
egen sum_post2015_adj_indvdl = sum(sum_post2015_adj), by(pik)
egen sum_post2015_inc_indvdl = sum(sum_post2015_inc), by(pik)
egen sum_post2015_pro_indvdl = sum(sum_post2015_pro), by(pik)
egen sum_post2015_par_indvdl = sum(sum_post2015_par), by(pik)
bysort pik: egen cj_pre2015_contact_indvdl = max(cj_pre2015_contact)
bysort pik: egen cj_pre2015_contact_in_fs_indvdl = max(cj_pre2015_contact_in_fs)
duplicates drop pik, force

drop sum_arr sum_adj sum_inc sum_pro sum_par sum_pre2015_arr sum_pre2015_inc sum_pre2015_pro sum_pre2015_par sum_post2015_arr sum_post2015_adj sum_post2015_inc sum_post2015_pro sum_post2015_par cj_pre2015_contact cj_pre2015_contact_in_fs SRC_ST

* Drop individuals whose pre/during 2015 CJ status is uncertain. If an
* individual did not have a pre/during 2015 CJ contact and they have at least
* one date-uncertain interaction, then their status is uncertain.
keep if cj_pre2015_contact_indvdl | (sum_post2015_arr_indvdl == sum_arr_indvdl & sum_post2015_adj_indvdl == sum_adj_indvdl & sum_post2015_inc_indvdl == sum_inc_indvdl & sum_post2015_pro_indvdl == sum_pro_indvdl & sum_post2015_par_indvdl == sum_par_indvdl)

* Save results
save "${mortality_dir}6_cjars_id_no_contact_sample", replace
clear

********************************************************************************
* Read in sample.
********************************************************************************
cd "$output_dir"
use 5_mdac_cjars_demographics

* Join with crosswalk file to get cause113 labels.
joinby using "${input_dir}cause113_ucause_crosswalk", unmatched(master)
drop _merge

* Recode causes of death.
gen cause113_new = cause113_label

replace cause113_new = "Drug overdose" if usubstr(ucause, 1, 3) == "X40" | usubstr(ucause, 1, 3) == "X41" | usubstr(ucause, 1, 3) == "X42" | usubstr(ucause, 1, 3) == "X43" | usubstr(ucause, 1, 3) == "X44" | usubstr(ucause, 1, 3) == "X60" | usubstr(ucause, 1, 3) == "X61" | usubstr(ucause, 1, 3) == "X62" | usubstr(ucause, 1, 3) == "X63" | usubstr(ucause, 1, 3) == "X64" | usubstr(ucause, 1, 3) == "Y10" | usubstr(ucause, 1, 3) == "Y11" | usubstr(ucause, 1, 3) == "Y12" | usubstr(ucause, 1, 3) == "Y13" | usubstr(ucause, 1, 3) == "Y14"

gen cause113_new_condensed = cause113_condensed

replace cause113_new_condensed = "Drug overdose" if usubstr(ucause, 1, 3) == "X40" | usubstr(ucause, 1, 3) == "X41" | usubstr(ucause, 1, 3) == "X42" | usubstr(ucause, 1, 3) == "X43" | usubstr(ucause, 1, 3) == "X44" | usubstr(ucause, 1, 3) == "X60" | usubstr(ucause, 1, 3) == "X61" | usubstr(ucause, 1, 3) == "X62" | usubstr(ucause, 1, 3) == "X63" | usubstr(ucause, 1, 3) == "X64" | usubstr(ucause, 1, 3) == "Y10" | usubstr(ucause, 1, 3) == "Y11" | usubstr(ucause, 1, 3) == "Y12" | usubstr(ucause, 1, 3) == "Y13" | usubstr(ucause, 1, 3) == "Y14"

********************************************************************************
* Those w/ a CJARS ID but no record count are considered as having had contact.
********************************************************************************
gen cj_pre2015_contact = (sum_pre2015_arr > 0 | sum_pre2015_adj > 0 | sum_pre2015_inc > 0 | sum_pre2015_pro > 0 | sum_pre2015_par > 0)  | (sum_arr == 0 & sum_adj == 0 & sum_inc == 0 & sum_pro == 0 & sum_par == 0 & SRC_ST != "") | group_quarters == "Adult correctional facility"
gen cj_pre2015_contact_in_fs = (SRC_ST == "FL" | SRC_ST == "MI" | SRC_ST == "NC" | SRC_ST == "TX" | SRC_ST == "WI") & cj_pre2015_contact

********************************************************************************
* Combine CJ records from multiple states into one record (individual-level).
********************************************************************************
egen sum_arr_indvdl = sum(sum_arr), by(pik)
egen sum_adj_indvdl = sum(sum_adj), by(pik)
egen sum_inc_indvdl = sum(sum_inc), by(pik)
egen sum_pro_indvdl = sum(sum_pro), by(pik)
egen sum_par_indvdl = sum(sum_par), by(pik)
egen sum_pre2015_arr_indvdl = sum(sum_pre2015_arr), by(pik)
egen sum_pre2015_adj_indvdl = sum(sum_pre2015_adj), by(pik)
egen sum_pre2015_inc_indvdl = sum(sum_pre2015_inc), by(pik)
egen sum_pre2015_pro_indvdl = sum(sum_pre2015_pro), by(pik)
egen sum_pre2015_par_indvdl = sum(sum_pre2015_par), by(pik)
egen sum_post2015_arr_indvdl = sum(sum_post2015_arr), by(pik)
egen sum_post2015_adj_indvdl = sum(sum_post2015_adj), by(pik)
egen sum_post2015_inc_indvdl = sum(sum_post2015_inc), by(pik)
egen sum_post2015_pro_indvdl = sum(sum_post2015_pro), by(pik)
egen sum_post2015_par_indvdl = sum(sum_post2015_par), by(pik)
bysort pik: egen cj_pre2015_contact_indvdl = max(cj_pre2015_contact)
bysort pik: egen cj_pre2015_contact_in_fs_indvdl = max(cj_pre2015_contact_in_fs)
duplicates drop pik, force

drop sum_arr sum_adj sum_inc sum_pro sum_par sum_pre2015_arr sum_pre2015_inc sum_pre2015_pro sum_pre2015_par sum_post2015_arr sum_post2015_adj sum_post2015_inc sum_post2015_pro sum_post2015_par cj_pre2015_contact cj_pre2015_contact_in_fs SRC_ST

* Drop individuals whose pre/during 2015 CJ status is uncertain. If an
* individual did not have a pre/during 2015 CJ contact and they have at least
* one date-uncertain interaction, then their status is uncertain.
keep if cj_pre2015_contact_indvdl | (sum_post2015_arr_indvdl == sum_arr_indvdl & sum_post2015_adj_indvdl == sum_adj_indvdl & sum_post2015_inc_indvdl == sum_inc_indvdl & sum_post2015_pro_indvdl == sum_pro_indvdl & sum_post2015_par_indvdl == sum_par_indvdl)

* Save results
save "${mortality_dir}6_cjars_id_contact_sample", replace
