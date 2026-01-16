* Set directories
global cjars_dir ""
global input_dir ""
cd "$cjars_dir"

* Read in CJARS Roster file
import sas DQB_SOURCE_ID CJARS_ID ALIAS SRC_ST DOB_YYYY using "um_cjars_2023q3_roster_rsch.sas7bdat", bcat(um_cjars_2023q3_roster)

* Merge CJARS roster with PIK crosswalk
cd "$input_dir"
merge 1:1 DQB_SOURCE_ID using cjars_pik_crosswalk, keep(match)

* Drop merge column
drop _merge

* Drop duplicate rows (implicitly)
contract CJARS_ID pik ALIAS SRC_ST DOB_YYYY

* If all records associated with a specific CJARS ID are aliases, keep them all.
* Then, if a record is not an alias, keep it, otherwise drop it.
bysort CJARS_ID: egen nr_alias = total(ALIAS)
bysort CJARS_ID: gen all_alias = cond(nr_alias == _N, 1, 0)
gen keep_col = cond(all_alias == 1 | ALIAS == 0, 1, 0)
keep if keep_col == 1
drop nr_alias all_alias keep_col ALIAS

* Left-join with Numident to obtain the year of birth.
join, by(pik) from(numident) keep(match master)
drop _merge

* Keep those records which are closest to the birth year listed in Numident.
* If all records for a given CJARS ID are missing the birth year, keep them all.
gen dobcc_missing = string(.)
replace dobcc_missing = dobcc if dobcc != ""

gen dobyy_missing = string(.)
replace dobyy_missing = dobyy if dobyy != ""

gen yob_numident = real(dobcc_missing + dobyy_missing)
gen year_diff = abs(yob_numident - DOB_YYYY)

bysort CJARS_ID: egen min_diff = min(year_diff)
gen keep_col = cond(year_diff == min_diff | min_diff == ., 1, 0)
keep if keep_col == 1
drop dobcc_missing dobyy_missing yob_numident year_diff min_diff keep_col dobcc dobyy DOB_YYYY

* Keep records whose 1st two characters of CJARS_ID match Numident birth state.
* If all records are missing (and/or don't match with) the Numident birth state, keep them all.
* If record is 1-to-1 match with PIK already, keep it.
gen cjars_st = usubstr(CJARS_ID, 1, 2)
gen states_match = pobst == cjars_st

bysort CJARS_ID: egen nr_state_matches = total(states_match)
bysort CJARS_ID: gen no_state_matches = cond(nr_state_matches == 0, 1, 0)
bysort CJARS_ID: gen nr_matches = _N
gen keep_col = cond(states_match == 1 | no_state_matches == 1 | nr_matches == 1, 1, 0)
keep if keep_col == 1
drop cjars_st states_match nr_state_matches no_state_matches nr_matches keep_col pobst

* Keep those CJARS_ID-to-PIK matches where the PIK retained is the modal PIK.
bysort CJARS_ID SRC_ST pik: egen nr_rows_with_this_pik = sum(_freq)
duplicates drop CJARS_ID SRC_ST pik nr_rows_with_this_pik, force
drop _freq

bysort CJARS_ID: egen max_nr = max(nr_rows_with_this_pik)
keep if nr_rows_with_this_pik == max_nr
drop max_nr nr_rows_with_this_pik

* Keep only those records which are 1-to-1 matches on CJARS_ID
egen count_cjars_id = count(CJARS_ID), by(CJARS_ID)
keep if count_cjars_id == 1
drop count_cjars_id

* save resulting table
save "2b_joined_cjars_pik", replace
