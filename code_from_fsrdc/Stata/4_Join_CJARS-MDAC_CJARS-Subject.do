**************************************************************** Set directories
global cjars_dir ""
global input_dir ""
global output_dir ""

********************************************** Read in MDAC and keep DOB and DOD
import sas CMID PNUM DBD DBM DBY dod using "mdac2008_2015_v1.sas7bdat"
merge 1:m CMID PNUM using "${output_dir}3_joined_cjars_mdac", keep(match)
drop _merge CMID PNUM pik SRC_ST

* Create DOB variable
gen dob = date(string(DBY) + "-" + string(DBM) + "-" + string(DBD), "YMD")
drop DBD DBM DBY

* Save results
save "${input_dir}mdac_dob_dod", replace
clear

************************************************************************ Arrests
* Read in arrests and keep only arrests associated w/ individuals in MDAC.
cd "$cjars_dir"
import sas CJARS_ID ARR_ARR_DT_DD ARR_ARR_DT_MM ARR_ARR_DT_YYYY ARR_BOOK_DT_DD ARR_BOOK_DT_MM ARR_BOOK_DT_YYYY using "um_cjars_2023q3_arrest_rsch.sas7bdat"
joinby CJARS_ID using "${input_dir}mdac_dob_dod"

* Create arrest date and booking date
gen arr_date = .
replace arr_date = date(string(ARR_ARR_DT_YYYY) + "-" + string(ARR_ARR_DT_MM), "YM") if (missing(ARR_ARR_DT_DD) & !missing(ARR_ARR_DT_MM) & !missing(ARR_ARR_DT_YYYY))
replace arr_date = date(string(ARR_ARR_DT_YYYY), "Y") if (missing(ARR_ARR_DT_DD) & missing(ARR_ARR_DT_MM) & !missing(ARR_ARR_DT_YYYY))
replace arr_date = date(string(ARR_ARR_DT_YYYY) + "-" + string(ARR_ARR_DT_MM) + "-" + string(ARR_ARR_DT_DD), "YMD") if (!missing(ARR_ARR_DT_DD) & !missing(ARR_ARR_DT_MM) & !missing(ARR_ARR_DT_YYYY))

gen book_date = .
replace book_date = date(string(ARR_BOOK_DT_YYYY) + "-" + string(ARR_BOOK_DT_MM), "YM") if (missing(ARR_BOOK_DT_DD) & !missing(ARR_BOOK_DT_MM) & !missing(ARR_BOOK_DT_YYYY))
replace book_date = date(string(ARR_BOOK_DT_YYYY), "Y") if (missing(ARR_BOOK_DT_DD) & missing(ARR_BOOK_DT_MM) & !missing(ARR_BOOK_DT_YYYY))
replace book_date = date(string(ARR_BOOK_DT_YYYY) + "-" + string(ARR_BOOK_DT_MM) + "-" + string(ARR_BOOK_DT_DD), "YMD") if (!missing(ARR_BOOK_DT_DD) & !missing(ARR_BOOK_DT_MM) & !missing(ARR_BOOK_DT_YYYY))

drop ARR_ARR_DT_YYYY ARR_ARR_DT_MM ARR_ARR_DT_DD ARR_BOOK_DT_YYYY ARR_BOOK_DT_MM ARR_BOOK_DT_DD

* Count number of arrests for each CJARS_ID
egen nr_arr = count(CJARS_ID), by(CJARS_ID)

* Determine if an arrest happened before/during 2015 (and not before birth or after death) and then count them.
gen arr_status = "none"
replace arr_status = "post2015" if year(arr_date) > 2015
replace arr_status = "pre2015" if year(arr_date) <= 2015
replace arr_status = "dob_dod_error" if arr_date < dob | (arr_date > dod & !missing(dod))
replace arr_status = "missing" if missing(arr_date)

gen book_status = "none"
replace book_status = "post2015" if year(book_date) > 2015
replace book_status = "pre2015" if year(book_date) <= 2015
replace book_status = "dob_dod_error" if book_date < dob | (book_date > dod & !missing(dod))
replace book_status = "missing" if missing(book_date)

gen pre2015 = 0
replace pre2015 = 1 if (arr_status == "pre2015" & book_status != "dob_dod_error") | (arr_status != "post2015" & arr_status != "dob_dod_error" & book_status == "pre2015")
egen nr_pre2015_arr = sum(pre2015), by(CJARS_ID)

gen post2015 = 0
replace post2015 = 1 if (arr_status == "post2015" & book_status != "dob_dod_error" & book_status != "pre2015") | (arr_status != "dob_dod_error" & arr_status != "pre2015" & book_status == "post2015")
egen nr_post2015_arr = sum(post2015), by(CJARS_ID)

* Keep CJARS ID along with associated number of arrests.
duplicates drop CJARS_ID, force
drop arr_date book_date arr_status book_status pre2015 post2015 dod dob

* Merge with MDAC
cd "$output_dir"
joinby CJARS_ID using 3_joined_cjars_mdac, unmatched(using)
drop _merge
replace nr_arr = 0 if missing(nr_arr)
replace nr_pre2015_arr = 0 if missing(nr_pre2015_arr)
replace nr_post2015_arr = 0 if missing(nr_post2015_arr)

* Save intermediate results
save "mdac_arrests", replace
clear

****************************************************************** Incarceration
* Read in incarcerations and keep only incarcerations associated w/ individuals in MDAC.
cd "$cjars_dir"
import sas CJARS_ID INC_ENTRY_DT_DD INC_ENTRY_DT_MM INC_ENTRY_DT_YYYY INC_EXIT_DT_DD INC_EXIT_DT_MM INC_EXIT_DT_YYYY using "um_cjars_2023q3_incar_rsch.sas7bdat"
joinby CJARS_ID using "${input_dir}mdac_dob_dod"

* Create incarceration entry and exit dates
gen inc_entry_date = .
replace inc_entry_date = date(string(INC_ENTRY_DT_YYYY) + "-" + string(INC_ENTRY_DT_MM), "YM") if (missing(INC_ENTRY_DT_DD) & !missing(INC_ENTRY_DT_MM) & !missing(INC_ENTRY_DT_YYYY))
replace inc_entry_date = date(string(INC_ENTRY_DT_YYYY), "Y") if (missing(INC_ENTRY_DT_DD) & missing(INC_ENTRY_DT_MM) & !missing(INC_ENTRY_DT_YYYY))
replace inc_entry_date = date(string(INC_ENTRY_DT_YYYY) + "-" + string(INC_ENTRY_DT_MM) + "-" + string(INC_ENTRY_DT_DD), "YMD") if (!missing(INC_ENTRY_DT_DD) & !missing(INC_ENTRY_DT_MM) & !missing(INC_ENTRY_DT_YYYY))

gen inc_exit_date = .
replace inc_exit_date = date(string(INC_EXIT_DT_YYYY) + "-" + string(INC_EXIT_DT_MM), "YM") if (missing(INC_EXIT_DT_DD) & !missing(INC_EXIT_DT_MM) & !missing(INC_EXIT_DT_YYYY))
replace inc_exit_date = date(string(INC_EXIT_DT_YYYY), "Y") if (missing(INC_EXIT_DT_DD) & missing(INC_EXIT_DT_MM) & !missing(INC_EXIT_DT_YYYY))
replace inc_exit_date = date(string(INC_EXIT_DT_YYYY) + "-" + string(INC_EXIT_DT_MM) + "-" + string(INC_EXIT_DT_DD), "YMD") if (!missing(INC_EXIT_DT_DD) & !missing(INC_EXIT_DT_MM) & !missing(INC_EXIT_DT_YYYY))

drop INC_ENTRY_DT_DD INC_ENTRY_DT_MM INC_ENTRY_DT_YYYY INC_EXIT_DT_DD INC_EXIT_DT_MM INC_EXIT_DT_YYYY

* Count number of incarcerations for each CJARS_ID
egen nr_inc = count(CJARS_ID), by(CJARS_ID)

* Determine if an incarceration happened before 2015 (and not before birth or after death) and then count them.
gen inc_entry_status = "none"
replace inc_entry_status = "post2015" if year(inc_entry_date) > 2015
replace inc_entry_status = "pre2015" if year(inc_entry_date) <= 2015
replace inc_entry_status = "dob_dod_error" if inc_entry_date < dob | (inc_entry_date > dod & !missing(dod))
replace inc_entry_status = "missing" if missing(inc_entry_date)

gen inc_exit_status = "none"
replace inc_exit_status = "post2015" if year(inc_exit_date) > 2015
replace inc_exit_status = "pre2015" if year(inc_exit_date) <= 2015
replace inc_exit_status = "dob_dod_error" if inc_exit_date < dob | (inc_exit_date > dod & !missing(dod))
replace inc_exit_status = "missing" if missing(inc_exit_date)

gen pre2015 = 0
replace pre2015 = 1 if (inc_entry_status == "pre2015" & inc_exit_status != "dob_dod_error") | (inc_entry_status != "post2015" & inc_entry_status != "dob_dod_error" & inc_exit_status == "pre2015")
egen nr_pre2015_inc = sum(pre2015), by(CJARS_ID)

gen post2015 = 0
replace post2015 = 1 if (inc_entry_status == "post2015" & inc_exit_status != "dob_dod_error" & inc_exit_status != "pre2015") | (inc_entry_status != "dob_dod_error" & inc_entry_status != "pre2015" & inc_exit_status == "post2015")
egen nr_post2015_inc = sum(post2015), by(CJARS_ID)

* Keep CJARS ID along with associated number of incarcerations.
duplicates drop CJARS_ID, force
drop inc_entry_date inc_exit_date inc_entry_status inc_exit_status pre2015 post2015 dod dob

* Merge with MDAC
cd "$output_dir"
joinby CJARS_ID using 3_joined_cjars_mdac, unmatched(using)
drop _merge
replace nr_inc = 0 if missing(nr_inc)
replace nr_pre2015_inc = 0 if missing(nr_pre2015_inc)
replace nr_post2015_inc = 0 if missing(nr_post2015_inc)

* Save intermediate results
save "mdac_inc", replace
clear

********************************************************************** Probation
* Read in probations and keep only probations associated w/ individuals in MDAC.
cd "$cjars_dir"
import sas CJARS_ID PRO_BGN_DT_DD PRO_BGN_DT_MM PRO_BGN_DT_YYYY PRO_END_DT_DD PRO_END_DT_MM PRO_END_DT_YYYY using "um_cjars_2023q3_probat_rsch.sas7bdat"
joinby CJARS_ID using "${input_dir}mdac_dob_dod"

* Create probation entry and exit dates.
gen pro_bgn_date = .
replace pro_bgn_date = date(string(PRO_BGN_DT_YYYY) + "-" + string(PRO_BGN_DT_MM), "YM") if (missing(PRO_BGN_DT_DD) & !missing(PRO_BGN_DT_MM) & !missing(PRO_BGN_DT_YYYY))
replace pro_bgn_date = date(string(PRO_BGN_DT_YYYY), "Y") if (missing(PRO_BGN_DT_DD) & missing(PRO_BGN_DT_MM) & !missing(PRO_BGN_DT_YYYY))
replace pro_bgn_date = date(string(PRO_BGN_DT_YYYY) + "-" + string(PRO_BGN_DT_MM) + "-" + string(PRO_BGN_DT_DD), "YMD") if (!missing(PRO_BGN_DT_DD) & !missing(PRO_BGN_DT_MM) & !missing(PRO_BGN_DT_YYYY))

gen pro_end_date = .
replace pro_end_date = date(string(PRO_END_DT_YYYY) + "-" + string(PRO_END_DT_MM), "YM") if (missing(PRO_END_DT_DD) & !missing(PRO_END_DT_MM) & !missing(PRO_END_DT_YYYY))
replace pro_end_date = date(string(PRO_END_DT_YYYY), "Y") if (missing(PRO_END_DT_DD) & missing(PRO_END_DT_MM) & !missing(PRO_END_DT_YYYY))
replace pro_end_date = date(string(PRO_END_DT_YYYY) + "-" + string(PRO_END_DT_MM) + "-" + string(PRO_END_DT_DD), "YMD") if (!missing(PRO_END_DT_DD) & !missing(PRO_END_DT_MM) & !missing(PRO_END_DT_YYYY))

drop PRO_BGN_DT_DD PRO_BGN_DT_MM PRO_BGN_DT_YYYY PRO_END_DT_DD PRO_END_DT_MM PRO_END_DT_YYYY

* Count number of probations for each CJARS_ID
egen nr_pro = count(CJARS_ID), by(CJARS_ID)

* Determine if a probation happened before 2015 (and not before birth or after death) and then count them.
gen pro_bgn_status = "none"
replace pro_bgn_status = "post2015" if year(pro_bgn_date) > 2015
replace pro_bgn_status = "pre2015" if year(pro_bgn_date) <= 2015
replace pro_bgn_status = "dob_dod_error" if pro_bgn_date < dob | (pro_bgn_date > dod & !missing(dod))
replace pro_bgn_status = "missing" if missing(pro_bgn_date)

gen pro_end_status = "none"
replace pro_end_status = "post2015" if year(pro_end_date) > 2015
replace pro_end_status = "pre2015" if year(pro_end_date) <= 2015
replace pro_end_status = "dob_dod_error" if pro_end_date < dob | (pro_end_date > dod & !missing(dod))
replace pro_end_status = "missing" if missing(pro_end_date)

gen pre2015 = 0
replace pre2015 = 1 if (pro_bgn_status == "pre2015" & pro_end_status != "dob_dod_error") | (pro_bgn_status != "dob_dod_error" & pro_bgn_status != "post2015" & pro_end_status == "pre2015")
egen nr_pre2015_pro = sum(pre2015), by(CJARS_ID)

gen post2015 = 0
replace post2015 = 1 if (pro_bgn_status == "post2015" & pro_end_status != "dob_dod_error" & pro_end_status != "pre2015") | (pro_bgn_status != "dob_dod_error" & pro_bgn_status != "pre2015" & pro_end_status == "post2015")
egen nr_post2015_pro = sum(post2015), by(CJARS_ID)

* Keep CJARS ID along with associated number of probations.
duplicates drop CJARS_ID, force
drop pro_bgn_date pro_end_date pro_bgn_status pro_end_status pre2015 post2015 dod dob

* Merge with MDAC
cd "$output_dir"
joinby CJARS_ID using 3_joined_cjars_mdac, unmatched(using)
drop _merge
replace nr_pro = 0 if missing(nr_pro)
replace nr_pre2015_pro = 0 if missing(nr_pre2015_pro)
replace nr_post2015_pro = 0 if missing(nr_post2015_pro)

* Save intermediate results
save "mdac_pro", replace
clear

************************************************************************ Paroles
* Read in paroles and keep only paroles associated w/ individuals in MDAC.
cd "$cjars_dir"
import sas CJARS_ID PAR_BGN_DT_DD PAR_BGN_DT_MM PAR_BGN_DT_YYYY PAR_END_DT_DD PAR_END_DT_MM PAR_END_DT_YYYY using "um_cjars_2023q3_parole_rsch.sas7bdat"
joinby CJARS_ID using "${input_dir}mdac_dob_dod"

* Create arrest date and booking date
gen par_bgn_date = .
replace par_bgn_date = date(string(PAR_BGN_DT_YYYY) + "-" + string(PAR_BGN_DT_MM), "YM") if (missing(PAR_BGN_DT_DD) & !missing(PAR_BGN_DT_MM) & !missing(PAR_BGN_DT_YYYY))
replace par_bgn_date = date(string(PAR_BGN_DT_YYYY), "Y") if (missing(PAR_BGN_DT_DD) & missing(PAR_BGN_DT_MM) & !missing(PAR_BGN_DT_YYYY))
replace par_bgn_date = date(string(PAR_BGN_DT_YYYY) + "-" + string(PAR_BGN_DT_MM) + "-" + string(PAR_BGN_DT_DD), "YMD") if (!missing(PAR_BGN_DT_DD) & !missing(PAR_BGN_DT_MM) & !missing(PAR_BGN_DT_YYYY))

gen par_end_date = .
replace par_end_date = date(string(PAR_END_DT_YYYY) + "-" + string(PAR_END_DT_MM), "YM") if (missing(PAR_END_DT_DD) & !missing(PAR_END_DT_MM) & !missing(PAR_END_DT_YYYY))
replace par_end_date = date(string(PAR_END_DT_YYYY), "Y") if (missing(PAR_END_DT_DD) & missing(PAR_END_DT_MM) & !missing(PAR_END_DT_YYYY))
replace par_end_date = date(string(PAR_END_DT_YYYY) + "-" + string(PAR_END_DT_MM) + "-" + string(PAR_END_DT_DD), "YMD") if (!missing(PAR_END_DT_DD) & !missing(PAR_END_DT_MM) & !missing(PAR_END_DT_YYYY))

drop PAR_BGN_DT_DD PAR_BGN_DT_MM PAR_BGN_DT_YYYY PAR_END_DT_DD PAR_END_DT_MM PAR_END_DT_YYYY

* Count number of paroles for each CJARS_ID
egen nr_par = count(CJARS_ID), by(CJARS_ID)

* Determine if a parole happened before 2015 (and not before birth or after death) and then count them.
gen par_bgn_status = "none"
replace par_bgn_status = "post2015" if year(par_bgn_date) > 2015
replace par_bgn_status = "pre2015" if year(par_bgn_date) <= 2015
replace par_bgn_status = "dob_dod_error" if par_bgn_date < dob | (par_bgn_date > dod & !missing(dod))
replace par_bgn_status = "missing" if missing(par_bgn_date)

gen par_end_status = "none"
replace par_end_status = "post2015" if year(par_end_date) > 2015
replace par_end_status = "pre2015" if year(par_end_date) <= 2015
replace par_end_status = "dob_dod_error" if par_end_date < dob | (par_end_date > dod & !missing(dod))
replace par_end_status = "missing" if missing(par_end_date)

gen pre2015 = 0
replace pre2015 = 1 if (par_bgn_status == "pre2015" & par_end_status != "dob_dod_error") | (par_bgn_status != "post2015" & par_bgn_status != "dob_dod_error" & par_end_status == "pre2015")
egen nr_pre2015_par = sum(pre2015), by(CJARS_ID)

gen post2015 = 0
replace post2015 = 1 if (par_bgn_status == "post2015" & par_end_status != "dob_dod_error" & par_end_status != "pre2015") | (par_bgn_status != "dob_dod_error" & par_bgn_status != "pre2015" & par_end_status == "post2015")
egen nr_post2015_par = sum(post2015), by(CJARS_ID)

* Keep CJARS ID along with associated number of paroles and pre2015-paroles
duplicates drop CJARS_ID, force
drop par_bgn_date par_end_date par_bgn_status par_end_status pre2015 post2015 dod dob

* Merge with MDAC
cd "$output_dir"
joinby CJARS_ID using 3_joined_cjars_mdac, unmatched(using)
drop _merge
replace nr_par = 0 if missing(nr_par)
replace nr_pre2015_par = 0 if missing(nr_pre2015_par)
replace nr_post2015_par = 0 if missing(nr_post2015_par)

* Save intermediate results
save "mdac_paroles", replace
clear

****************************************************************** Adjudications
* Read in adjudications and keep only adjudications associated w/ individuals in MDAC.
cd "$cjars_dir"
import sas CJARS_ID ADJ_FILE_DT_DD ADJ_FILE_DT_MM ADJ_FILE_DT_YYYY ADJ_DISP_DT_DD ADJ_DISP_DT_MM ADJ_DISP_DT_YYYY ADJ_SENT_DT_DD ADJ_SENT_DT_MM ADJ_SENT_DT_YYYY using "um_cjars_2023q3_adjud_rsch.sas7bdat"
joinby CJARS_ID using "${input_dir}mdac_dob_dod"

* Create filing, disposition, and sentencing dates.
gen file_date = .
replace file_date = date(string(ADJ_FILE_DT_YYYY) + "-" + string(ADJ_FILE_DT_MM), "YM") if (missing(ADJ_FILE_DT_DD) & !missing(ADJ_FILE_DT_MM) & !missing(ADJ_FILE_DT_YYYY))
replace file_date = date(string(ADJ_FILE_DT_YYYY), "Y") if (missing(ADJ_FILE_DT_DD) & missing(ADJ_FILE_DT_MM) & !missing(ADJ_FILE_DT_YYYY))
replace file_date = date(string(ADJ_FILE_DT_YYYY) + "-" + string(ADJ_FILE_DT_MM) + "-" + string(ADJ_FILE_DT_DD), "YMD") if (!missing(ADJ_FILE_DT_DD) & !missing(ADJ_FILE_DT_MM) & !missing(ADJ_FILE_DT_YYYY))

gen disp_date = .
replace disp_date = date(string(ADJ_DISP_DT_YYYY) + "-" + string(ADJ_DISP_DT_MM), "YM") if (missing(ADJ_DISP_DT_DD) & !missing(ADJ_DISP_DT_MM) & !missing(ADJ_DISP_DT_YYYY))
replace disp_date = date(string(ADJ_DISP_DT_YYYY), "Y") if (missing(ADJ_DISP_DT_DD) & missing(ADJ_DISP_DT_MM) & !missing(ADJ_DISP_DT_YYYY))
replace disp_date = date(string(ADJ_DISP_DT_YYYY) + "-" + string(ADJ_DISP_DT_MM) + "-" + string(ADJ_DISP_DT_DD), "YMD") if (!missing(ADJ_DISP_DT_DD) & !missing(ADJ_DISP_DT_MM) & !missing(ADJ_DISP_DT_YYYY))

gen sent_date = .
replace sent_date = date(string(ADJ_SENT_DT_YYYY) + "-" + string(ADJ_SENT_DT_MM), "YM") if (missing(ADJ_SENT_DT_DD) & !missing(ADJ_SENT_DT_MM) & !missing(ADJ_SENT_DT_YYYY))
replace sent_date = date(string(ADJ_SENT_DT_YYYY), "Y") if (missing(ADJ_SENT_DT_DD) & missing(ADJ_SENT_DT_MM) & !missing(ADJ_SENT_DT_YYYY))
replace sent_date = date(string(ADJ_SENT_DT_YYYY) + "-" + string(ADJ_SENT_DT_MM) + "-" + string(ADJ_SENT_DT_DD), "YMD") if (!missing(ADJ_SENT_DT_DD) & !missing(ADJ_SENT_DT_MM) & !missing(ADJ_SENT_DT_YYYY))

drop ADJ_FILE_DT_DD ADJ_FILE_DT_MM ADJ_FILE_DT_YYYY ADJ_DISP_DT_DD ADJ_DISP_DT_MM ADJ_DISP_DT_YYYY ADJ_SENT_DT_DD ADJ_SENT_DT_MM ADJ_SENT_DT_YYYY

* Count number of adjudications for each CJARS_ID
egen nr_adj = count(CJARS_ID), by(CJARS_ID)

* Determine if an adjudication happened before 2015 (and not before birth or after death) and then count them.
gen file_status = "none"
replace file_status = "post2015" if year(file_date) > 2015
replace file_status = "pre2015" if year(file_date) <= 2015
replace file_status = "dob_dod_error" if file_date < dob | (file_date > dod & !missing(dod))
replace file_status = "missing" if missing(file_date)

gen disp_status = "none"
replace disp_status = "post2015" if year(disp_date) > 2015
replace disp_status = "pre2015" if year(disp_date) <= 2015
replace disp_status = "dob_dod_error" if disp_date < dob | (disp_date > dod & !missing(dod))
replace disp_status = "missing" if missing(disp_date)

gen sent_status = "none"
replace sent_status = "post2015" if year(sent_date) > 2015
replace sent_status= "pre2015" if year(sent_date) <= 2015
replace sent_status = "dob_dod_error" if sent_date < dob | (sent_date > dod & !missing(dod))
replace sent_status = "missing" if missing(sent_date)

gen pre2015 = 0
replace pre2015 = 1 if ((file_status == "pre2015") | (file_status != "post2015" & disp_status == "pre2015") | (file_status != "post2015" & disp_status != "post2015" & sent_status == "pre2015")) & !(file_status == "pre2015" & disp_status == "post2015" & sent_status == "pre2015") & !(file_status == "dob_dod_error" | disp_status == "dob_dod_error" | sent_status == "dob_dod_error")
egen nr_pre2015_adj = sum(pre2015), by(CJARS_ID)

gen post2015 = 0
replace post2015 = 1 if (file_status == "missing" & disp_status == "missing" & sent_status == "post2015") | (file_status == "missing" & disp_status == "post2015" & sent_status == "missing") | (file_status == "missing" & disp_status == "post2015" & sent_status == "post2015") | (file_status == "post2015" & disp_status == "missing" & sent_status == "missing") | (file_status == "post2015" & disp_status == "missing" & sent_status == "post2015") | (file_status == "post2015" & disp_status == "post2015" & sent_status == "missing") | (file_status == "post2015" & disp_status == "post2015" & sent_status == "post2015")
egen nr_post2015_adj = sum(post2015), by(CJARS_ID)

* Keep CJARS ID along with associated number of adjudications.
duplicates drop CJARS_ID, force
drop file_date disp_date sent_date file_status disp_status sent_status pre2015 post2015 dod dob

* Merge with MDAC
cd "$output_dir"
joinby CJARS_ID using 3_joined_cjars_mdac, unmatched(using)
drop _merge
replace nr_adj = 0 if missing(nr_adj)
replace nr_pre2015_adj = 0 if missing(nr_pre2015_adj)
replace nr_post2015_adj = 0 if missing(nr_post2015_adj)

* Save intermediate results
save "mdac_adj", replace

********************************************************* Merge results together
cd "$input_dir"

merge 1:1 CJARS_ID pik CMID PNUM using mdac_arrests
drop _merge

merge 1:1 CJARS_ID pik CMID PNUM using mdac_paroles
drop _merge

merge 1:1 CJARS_ID pik CMID PNUM using mdac_inc
drop _merge

merge 1:1 CJARS_ID pik CMID PNUM using mdac_pro
drop _merge

* Save results
save "4_joined_mdac_cjars-subject", replace
