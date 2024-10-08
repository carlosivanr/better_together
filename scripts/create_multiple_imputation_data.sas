* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Carlos Rodriguez, Ph.D. Dept. of Family Medicine. CU Anschutz
July 20, 2023
Better Together Clinician Coaching 2023
Create Multiple Imputation Dataset

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

* Import .csv file -----------------------------------------------;
%web_drop_table(WORK.bt_data);
FILENAME REFFILE 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data/better_together_sas_data.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.bt_data;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=WORK.bt_data; RUN;
%web_open_table(WORK.bt_data);


* Convert the variables to numeric format ----------------------;
data bt_data;
	set bt_data(rename=(mbi_ee_sum = mbi_ee_char
					   mbi_dp_sum = mbi_dp_char
					   mbi_pa_sum = mbi_pa_char
					   misshp_tot = misshp_char
					   yis_sum = yis_char
					   yis_bin = yis_ispos_char
					   ls_sum = ls3_char
					   scssf_sum_tot = scssf_char
					   fi_tot = fi_char
					   sfi_tot = sfi_char
					   age = age_char
					   years_since_training = years_tr_char
					   years_at_clinic = years_cl_char));
	mbi_ee_sum = input(mbi_ee_char, 15.);
	mbi_dp_sum = input(mbi_dp_char, 15.);
	mbi_pa_sum = input(mbi_pa_char, 15.);
	misshp_tot = input(misshp_char, 15.);
	yissum = input(yis_char, 15.);
	yis_ispos = input(yis_ispos_char, 15.);
	ls3_tot = input(ls3_char, 15.);
	scssf_sum_tot = input(scssf_char, 15.);
	fi_tot = input(fi_char, 15.);
	sfi_tot = input(sfi_char, 15.);
	age = input(age_char, 15.);
	years_since_training = input(years_tr_char, 15.);
	years_at_clinic = input(years_cl_char, 15.);
	drop mbi_ee_char mbi_dp_char mbi_pa_char misshp_char yis_char ls3_char scssf_char fi_char sfi_char yis_char yis_ispos_char age_char years_tr_char years_cl_char;
run;

* Create a copy of the original imported set;
data bt_data_orig;
	set bt_data;
run;


* dependent variables
1. mbi_ee_sum
2. mbi_dp_sum
3. mbi_pa_sum
4. misshp_tot
5. yis_sum
6. yis_bin
7. ls_sum
8. scssf_sum_tot
9. fi_tot
10. sfi_tot

* predictor variables
1. redcap_event_name (pre vs post)
2. randomization (treatment)
3. gender
4. degree
5. specialty
6. years at clinic
8. years of experience
9. race_ethnicity
10 sexual orientation;
	
/*data bt_flag;
	set bt_data_orig;
	if sexual_orienation = "" then orientation_flag = 1; else orientation_flag = 0;
	keep identifier randomization redcap_event_name sexual_orientation orientation_flag;
run;*/
	
data bt_flag;
	set bt_data_orig;
	if mbi_ee_sum = .  then mbi_ee_flag = 1; else mbi_ee_flag =0;
	if mbi_dp_sum = .  then mbi_dp_flag = 1; else mbi_dp_flag =0;
	if mbi_pa_sum = .  then mbi_pa_flag = 1; else mbi_pa_flag =0;
	if misshp_tot = .  then misshp_flag = 1; else misshp_flag =0;
	if yissum = .  then yis_flag = 1; else yis_flag =0;
	if ls3_tot = . then ls_flag = 1; else ls_flag = 0;
	if scssf_sum_tot = . then scssf_flag = 1; else scssf_flag = 0;
	if fi_tot = . then fi_flag = 1; else fi_flag = 0;
	if sfi_tot = . then sfi_flag = 1; else sfi_flag = 0;
	if age = . then age_flag = 1; else age_flag = 0;
	if gender_identity = "" then gender_flag = 1; else gender_flag = 0;
	if race_ethnicity = "" then race_flag = 1; else race_flag = 0;
	if sexual_orientation = "" then orientation_flag = 1; else orientation_flag = 0;
	if specialty = "" then specialty_flag = 1; else specialty_flag = 0;
	if bhc = "" then bhc_flag = 1; else bhc_flag = 0;
	if degree = "" then degree_flag = 1; else degree_flag = 0;
	if years_since_training = . then yrs_tr_flag = 1; else yrs_tr_flag = 0;
	if years_at_clinic = . then yrs_cl_flag = 1; else yrs_cl_flag = 0;
	keep identifier randomization redcap_event_name mbi_ee_flag mbi_dp_flag mbi_pa_flag misshp_flag yis_flag ls_flag scssf_flag fi_flag sfi_flag age_flag gender_flag race_flag orientation_flag  specialty_flag bhc_flag degree_flag yrs_tr_flag yrs_cl_flag;
run;


proc sort data = bt_flag;
	by redcap_event_name;
run; 

* Frequencies of binary flag variables where 1 indicates missing;
proc freq data=bt_flag;
by redcap_event_name;
tables mbi_ee_flag mbi_dp_flag mbi_pa_flag misshp_flag yis_flag ls_flag scssf_flag fi_flag sfi_flag age_flag gender_flag race_flag orientation_flag  specialty_flag bhc_flag degree_flag yrs_tr_flag yrs_cl_flag;
run;




* Prepare data, by compressing values with spaces;
data bt_data;
	set bt_data_orig;
run;

data bt_data;
	set bt_data;
	gender_identity = compress(gender_identity);
	department_randomization = compress(department_randomization);
	degree = compress(degree);
	sexual_orientation = compress(sexual_orientation);
	specialty = compress(specialty);
run;


* Prepare a design matrix;
proc logistic data=bt_data outdesign=bt_design (drop = Intercept) outdesignonly;
	class gender_identity(ref = "Cisfemale") race_ethnicity(ref = 'White') department_randomization(ref = "IM_PM_Rehab") bhc(ref = "Ye") degree(ref = "MDorDO") / param = reference;
	model identifier = age gender_identity race_ethnicity department_randomization degree years_since_training years_at_clinic;
run;


* Get the var names;
proc sql noprint;
	select name into :varnames separated by ' '
	from sashelp.vcolumn
	where libname='WORK' and memname = 'BT_DESIGN' and lowcase(name)^in ('identifier');
quit;

* Print the variable names to the log to inspect;
%put &varnames;

data merged;
	merge bt_data bt_design(drop= identifier);
run;

* Patterns of Missingness;
proc mi data=merged nimpute=0;
*class gender_identity race_ethnicity sexual_orientation specialty bhc degree ;
var mbi_ee_sum mbi_dp_sum mbi_pa_sum misshp_tot yissum ls3_tot scssf_sum_tot fi_tot sfi_tot &varnames;
ods select misspattern;
fcs regpmm;
run;

proc corr data = bt_design cov outp=test;
var &varnames ;
run;

* Impute the dependent variables;
proc mi data=merged nimpute=10 out = mi_mvn seed = 54321;
var mbi_ee_sum mbi_dp_sum mbi_pa_sum misshp_tot yissum ls3_tot scssf_sum_tot fi_tot sfi_tot &varnames;
fcs regpmm(mbi_ee_sum mbi_dp_sum mbi_pa_sum misshp_tot yissum ls3_tot scssf_sum_tot fi_tot sfi_tot);
run;

* Map a library to a directory name ----------------------------;
libname out 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data';

data out.mi_mvn;
	set mi_mvn;
run;
