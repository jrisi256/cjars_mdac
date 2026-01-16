**************************************************************** Set directories
global cjars_dir ""
global input_dir ""
global output_dir ""

******************************************** Read in MDAC and clean up variables
import sas CMID PNUM ST matchstat dod SEX IMPRC HSGP DBD DBM DBY mdac_wgt GQMAJTYP ucause cause113 using "mdac2008_2015_v1.sas7bdat"

gen state = "none"
replace state = "AL" if ST == "001"
replace state = "AK" if ST == "002"
replace state = "AZ" if ST == "004"
replace state = "AR" if ST == "005"
replace state = "CA" if ST == "006"
replace state = "CO" if ST == "008"
replace state = "CT" if ST == "009"
replace state = "DE" if ST == "010"
replace state = "DC" if ST == "011"
replace state = "FL" if ST == "012"
replace state = "GA" if ST == "013"
replace state = "HI" if ST == "015"
replace state = "ID" if ST == "016"
replace state = "IL" if ST == "017"
replace state = "IN" if ST == "018"
replace state = "IA" if ST == "019"
replace state = "KS" if ST == "020"
replace state = "KY" if ST == "021"
replace state = "LA" if ST == "022"
replace state = "ME" if ST == "023"
replace state = "MD" if ST == "024"
replace state = "MA" if ST == "025"
replace state = "MI" if ST == "026"
replace state = "MN" if ST == "027"
replace state = "MS" if ST == "028"
replace state = "MO" if ST == "029"
replace state = "MT" if ST == "030"
replace state = "NE" if ST == "031"
replace state = "NV" if ST == "032"
replace state = "NH" if ST == "033"
replace state = "NJ" if ST == "034"
replace state = "NM" if ST == "035"
replace state = "NY" if ST == "036"
replace state = "NC" if ST == "037"
replace state = "ND" if ST == "038"
replace state = "OH" if ST == "039"
replace state = "OK" if ST == "040"
replace state = "OR" if ST == "041"
replace state = "PA" if ST == "042"
replace state = "RI" if ST == "044"
replace state = "SC" if ST == "045"
replace state = "SD" if ST == "046"
replace state = "TN" if ST == "047"
replace state = "TX" if ST == "048"
replace state = "UT" if ST == "049"
replace state = "VT" if ST == "050"
replace state = "VA" if ST == "051"
replace state = "WA" if ST == "053"
replace state = "WV" if ST == "054"
replace state = "WI" if ST == "055"
replace state = "WY" if ST == "056"

gen mortality = .
replace mortality = -1 if matchstat == "0"
replace mortality = -1 if matchstat == "1"
replace mortality = 0 if matchstat == "2"
replace mortality = 1 if matchstat == "3"

gen sex = "none"
replace sex = "Male" if SEX == "1"
replace sex = "Female" if SEX == "2"

gen race = "none"
replace race = "White" if IMPRC == "01" & HSGP == "1"
replace race = "Black" if IMPRC == "02" & HSGP == "1"
replace race = "AIAN" if IMPRC == "03" & HSGP == "1"
replace race = "Asian" if IMPRC == "04" & HSGP == "1"
replace race = "NHPI" if IMPRC == "05" & HSGP == "1"
replace race = "Multiracial" if IMPRC != "01" & IMPRC != "02" & IMPRC != "03" & IMPRC != "04" & IMPRC != "05" & HSGP == "1"
replace race = "Hispanic" if HSGP != "1"

gen dob = date(string(DBY) + "-" + string(DBM) + "-" + string(DBD), "YMD")
gen age = .
replace age = (date("2015-12-31", "YMD") - dob) / 365.25 if missing(dod)
replace age = (dod - dob) / 365.25 if !missing(dod)

gen age_bucket = ""
replace age_bucket = "16 and under" if round(age) <= 16
replace age_bucket = "17 - 25" if round(age) >= 17 & round(age) <= 25
replace age_bucket = "26 - 35" if round(age) >= 26 & round(age) <= 35
replace age_bucket = "36 - 45" if round(age) >= 36 & round(age) <= 45
replace age_bucket = "46 - 55" if round(age) >= 46 & round(age) <= 55
replace age_bucket = "56 - 65" if round(age) >= 56 & round(age) <= 65
replace age_bucket = "66 - 75" if round(age) >= 66 & round(age) <= 75
replace age_bucket = "76 and over" if round(age) >= 76

gen group_quarters = "none"
replace group_quarters = "Not in a group quarters" if GQMAJTYP == ""
replace group_quarters = "Adult correctional facility" if GQMAJTYP == "1"
replace group_quarters = "Juvenile facilities" if GQMAJTYP == "2"
replace group_quarters = "Nursing facilities" if GQMAJTYP == "3"
replace group_quarters = "Other health care facilities" if GQMAJTYP == "4"
replace group_quarters = "College/university student housing" if GQMAJTYP == "5"
replace group_quarters = "Military quarters/military ships" if GQMAJTYP == "6"
replace group_quarters = "Other noninstitutional facilities" if GQMAJTYP == "7"

drop ST SEX IMPRC HSGP matchstat DBD DBM DBY GQMAJTYP

********************************************************************************
* Join with the CJARS-MDAC sample thus merging CJ variables w/ demog. variables.
********************************************************************************
joinby CMID PNUM using "${output_dir}4_joined_mdac_cjars-subject"

* Filter out individuals whose mortality status is unknown.
keep if mortality == 0 | mortality == 1

********************************************************************************
* Collapse rows based on PIK and SRC_ST (state of CJ involvement)
********************************************************************************
egen sum_arr = sum(nr_arr), by(pik SRC_ST)
egen sum_adj = sum(nr_adj), by(pik SRC_ST)
egen sum_inc = sum(nr_inc), by(pik SRC_ST)
egen sum_pro = sum(nr_pro), by(pik SRC_ST)
egen sum_par = sum(nr_par), by(pik SRC_ST)
egen sum_pre2015_arr = sum(nr_pre2015_arr), by(pik SRC_ST)
egen sum_pre2015_adj = sum(nr_pre2015_adj), by(pik SRC_ST)
egen sum_pre2015_inc = sum(nr_pre2015_inc), by(pik SRC_ST)
egen sum_pre2015_pro = sum(nr_pre2015_pro), by(pik SRC_ST)
egen sum_pre2015_par = sum(nr_pre2015_par), by(pik SRC_ST)
egen sum_post2015_arr = sum(nr_post2015_arr), by(pik SRC_ST)
egen sum_post2015_adj = sum(nr_post2015_adj), by(pik SRC_ST)
egen sum_post2015_inc = sum(nr_post2015_inc), by(pik SRC_ST)
egen sum_post2015_pro = sum(nr_post2015_pro), by(pik SRC_ST)
egen sum_post2015_par = sum(nr_post2015_par), by(pik SRC_ST)
duplicates drop pik SRC_ST, force

drop nr_arr nr_pre2015_arr nr_post2015_arr nr_adj nr_pre2015_adj nr_post2015_adj nr_inc nr_pre2015_inc nr_post2015_inc nr_pro nr_pre2015_pro nr_post2015_pro nr_par nr_pre2015_par nr_post2015_par CJARS_ID

* Save results
save "${output_dir}5_mdac_cjars_demographics", replace
