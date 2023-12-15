* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Carlos Rodriguez, Ph.D. Dept. of Family Medicine. CU Anschutz
July 20, 2023
Better Together Clinician Coaching 2023
Mixed models & plots of estimated change

Variables --------
randomization --> Waitlist Control or Intervention

redcap_event_name --> Pre or Post intervention data collection

Maslach's Burnout Inventory (MBI) - 3 subscales
- scoring ee_sum with or without NAs doesn't affect end result
1. Emotional Exhaustion (MBI_EE)
2. Depersonalization (MBI_DP)
3. Personal Accomplishment (MBI_PA)

Young's Impostor Syndrome Scale (YIS)

Moral Injury Symptom Scale for Healcare Providers (MISSHP)

UCLA Loneliness Scale (LS3)

Neff's Self-Compassion Scale Short Form (SCSSF)

Flourishing Index (FI)

Secure Flourishing Index (SFI)

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

* Map a library to a directory name ----------------------------;
libname out 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data/multiple_imputation';


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

* Patterns of Missingness;
proc mi data=bt_data nimpute=0;
class gender_identity race_ethnicity sexual_orientation specialty bhc degree ;
var mbi_ee_sum mbi_dp_sum mbi_pa_sum misshp_tot yissum ls3_tot scssf_sum_tot fi_tot sfi_tot age gender_identity race_ethnicity sexual_orientation specialty bhc degree years_since_training years_at_clinic;
ods select misspattern;
fcs regpmm;
run;


* Remove outliers before imputation;
* mbi_dp_sum;
data bt_data;
	set bt_data;
	if mbi_dp_sum >= 23 and redcap_event_name = "Post" and randomization = "Intervention" then mbi_dp_sum = .;
run;

* scssf_sum_tot;
data bt_data;
	set bt_data;
	if scssf_sum_tot >= 85.5 and redcap_event_name = "Pre" and randomization = "Waitlist" then scssf_sum_tot = .;
	if scssf_sum_tot >= 90.25 and redcap_event_name = "Pre" and randomization = "Intervention" then scssf_sum_tot = .;
run;

* fi_tot;
data bt_data;
	set bt_data;
	if fi_tot <= 2.5 and redcap_event_name = "Pre" and randomization = "Waitlist" then fi_tot = .;
run;

* sfi_tot;
data bt_data;
	set bt_data;
	if sfi_tot <= 3.0833 and redcap_event_name = "Pre" and randomization = "Intervention" then sfi_tot = .;
run;

* 1. Imputation phase - Runs into warnings;
/*WARNING: The covariates are not specified in an FCS discriminant method for variable 
          race_ethnicity, only remaining continuous variables will be used as covariates with the 
          default CLASSEFFECTS=EXCLUDE option.
 WARNING: The covariates are not specified in an FCS discriminant method for variable 
          sexual_orientation, only remaining continuous variables will be used as covariates with 
          the default CLASSEFFECTS=EXCLUDE option.
 WARNING: An effect for variable mbi_ee_sum is a linear combination of other effects. The 
          coefficient of the effect will be set to zero in the imputation.
 WARNING: An effect for variable mbi_dp_sum is a linear combination of other effects. The 
          coefficient of the effect will be set to zero in the imputation.
 WARNING: An effect for variable mbi_pa_sum is a linear combination of other effects. The 
          coefficient of the effect will be set to zero in the imputation.
 WARNING: An effect for variable misshp_tot is a linear combination of other effects. The 
          coefficient of the effect will be set to zero in the imputation.
 WARNING: An effect for variable yissum is a linear combination of other effects. The coefficient 
          of the effect will be set to zero in the imputation.
 WARNING: An effect for variable ls3_tot is a linear combination of other effects. The coefficient 
          of the effect will be set to zero in the imputation.
 WARNING: An effect for variable scssf_sum_tot is a linear combination of other effects. The 
          coefficient of the effect will be set to zero in the imputation.
 WARNING: An effect for variable fi_tot is a linear combination of other effects. The coefficient 
          of the effect will be set to zero in the imputation.
 WARNING: An effect for variable sfi_tot is a linear combination of other effects. The coefficient 
          of the effect will be set to zero in the imputation.*/
         
proc mi data=bt_data nimpute=10 out = mi_mvn seed = 54321;
class gender_identity race_ethnicity sexual_orientation specialty bhc degree;
var mbi_ee_sum age gender_identity race_ethnicity sexual_orientation specialty bhc degree years_since_training years_at_clinic;
*var mbi_ee_sum mbi_dp_sum mbi_pa_sum misshp_tot yissum ls3_tot scssf_sum_tot sfi_tot age gender_identity race_ethnicity sexual_orientation specialty bhc degree years_since_training years_at_clinic;
fcs discrim(gender_identity race_ethnicity sexual_orientation specialty bhc degree) regpmm(mbi_ee_sum) ;
*fcs reg;
run;

PROC CORR DATA=bt_data;
   var mbi_ee_sum mbi_dp_sum mbi_pa_sum misshp_tot yissum ls3_tot scssf_sum_tot fi_tot sfi_tot age;
RUN;

* 2. Analyze & pool each DV;

* 2.1 mbi_ee_sum -----------------------------------------------;
proc mixed data=mi_mvn noclprint covtest namelen=100 namelen=100;
	by _imputation_;
	class identifier randomization(ref = "Waitlist") redcap_event_name degree(ref = "MD or DO");
 	model mbi_ee_sum = degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output SolutionF=mixparms;	
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.mbi_ee_sum_mi;
run;



*2.2 mbi_dp_sum ------------------------------------;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_dp_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.mbi_dp_sum_mi;
run;



*2.3 mbi_pa_sum ------------------------------------;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_pa_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.mbi_pa_sum_mi;
run;

*2.4 misshp_tot;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name degree department_randomization;
 	model misshp_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
	lsmeans randomization * redcap_event_name / pdiff cl;
	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.misshp_tot_mi;
run;


*2.5 yissum;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name degree department_randomization;
 	model yissum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.yissum_mi;
run;

* 2.6 yis_sum binary;

*2.7 ls3;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name degree department_randomization;
 	model ls3_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.ls3_tot_mi;
run;

* 2.8 scssf;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name degree department_randomization;
 	model scssf_sum_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name degree randomization*redcap_event_name;
	ods output ParameterEstimates=out.scssf_sum_tot_mi;
run;


* 2.9 fi_tot;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name;
 	model fi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name randomization*redcap_event_name;
	ods output ParameterEstimates=out.fi_tot_mi;
run;

* 2.10 sfi_tot;
proc mixed data=bt_data noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name;
 	model sfi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name degree;
	modeleffects intercept randomization redcap_event_name randomization*redcap_event_name;
	ods output ParameterEstimates=out.sfi_tot_mi;
run;
