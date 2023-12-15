/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

Moral Injury Symptom Scale for Healthcare Providers (MISSHP)

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
libname out 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data/complete_case';


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
					   sfi_tot = sfi_char));
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
	drop mbi_ee_char mbi_dp_char mbi_pa_char misshp_char yis_char ls3_char  scssf_char fi_char sfi_char yis_ispos_char;
run;

* Load the plotting macro;
%include "C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/scripts/functions/MACRO_plot_est_change.sas";

* Create a copy of the original imported set;
data bt_data_orig;
	set bt_data;
run;

* MBI_EE_SUM  -------------------------------------------------;
title "MBI EE SUM";
* Find who has a missing value in mbi_ee_sum;
data mbi_ee_cc;
	set bt_data_orig;
	where mbi_ee_sum is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from mbi_ee_cc); 
quit;

proc sgplot data = bt_data;
	vbox mbi_ee_sum / group = randomization category = redcap_event_name;
run;

* Unstructured default ddfm. Unstructured covariance, 
  with ddfm = res reproduces gls() results with corSymm() in R;
* included degree due to missingness, but does not impact the 
	results when compared to excluding degree;	
proc mixed data=bt_data noclprint covtest;
	class identifier randomization(ref = "Waitlist") redcap_event_name degree(ref = "MD or DO");
 	model mbi_ee_sum = degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.mbi_ee_sum_diffs lsmeans=out.mbi_ee_sum_lsmeans tests3=out.mbi_ee_sum_t3 SolutionF=out.mbi_ee_sum_sol lsmestimates=out.mbi_ee_sum_did;
run;
*%plot_est_change(var=MBI_EE);
title;

* MBI_DP_SUM  -------------------------------------------------;
title "MBI DP SUM";
* Find who has a missing value;
data mbi_dp_cc;
	set bt_data_orig;
	where mbi_dp_sum is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from mbi_dp_cc); 
quit;

/*ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox mbi_dp_sum / group = randomization category = redcap_event_name;
run;

proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	if mbi_dp_sum >= 23 and redcap_event_name = "Post" and randomization = "Intervention" then exclude = 1;
run;*/


* include department degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_dp_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.mbi_dp_sum_diffs lsmeans=out.mbi_dp_sum_lsmeans tests3=out.mbi_dp_sum_t3 SolutionF=out.mbi_dp_sum_sol lsmestimates=out.mbi_dp_sum_did;
run;
*%plot_est_change(var=MBI_DP);
title;

/*title "MBI DP SUM Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_dp_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.mbi_dp_sum_diffs lsmeans=out.mbi_dp_sum_lsmeans tests3=out.mbi_dp_sum_t3 SolutionF=out.mbi_dp_sum_sol lsmestimates=out.mbi_dp_sum_did;
run;
title;*/

* MBI_PA_SUM  -------------------------------------------------;
title "MBI PA SUM";
* Find who has a missing value;
data mbi_pa_cc;
	set bt_data_orig;
	where mbi_pa_sum is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from mbi_pa_cc); 
quit;

/*proc sgplot data = bt_data;
	vbox mbi_pa_sum / group = randomization category = redcap_event_name;
run;*/

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_pa_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.mbi_pa_sum_diffs lsmeans=out.mbi_pa_sum_lsmeans tests3=out.mbi_pa_sum_t3 SolutionF=out.mbi_pa_sum_sol lsmestimates=out.mbi_pa_sum_did;
run;
*%plot_est_change(var=MBI_PA);


* MISSHP_TOT  -------------------------------------------------;
title "MISSHP";
* Find who has a missing value;
data mbi_miss_cc;
	set bt_data_orig;
	where misshp_tot is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from mbi_miss_cc); 
quit;

/*proc sgplot data = bt_data;
	vbox misshp_tot / group = randomization category = redcap_event_name;
run;*/

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model misshp_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
	lsmeans randomization * redcap_event_name / pdiff cl;
	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.misshp_tot_diffs lsmeans=out.misshp_tot_lsmeans tests3=out.misshp_tot_t3 SolutionF=out.misshp_tot_sol lsmestimates=out.misshp_tot_did;
run;
*%plot_est_change(var=MISS);
title;

* YISSUM ---------------------------------------------------;
title "YIS SUM Linear Mixed Model";
* Find who has a missing value;
data yissum_cc;
	set bt_data_orig;
	where yissum is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from mbi_dp_cc); 
quit;

/*proc sgplot data = bt_data;
	vbox yissum / group = randomization category = redcap_event_name;
run;*/

* can also be analyzed as binary variable yis_ispos;
* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model yissum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.yissum_diffs lsmeans=out.yissum_lsmeans tests3=out.yissum_t3 SolutionF=out.yissum_sol lsmestimates=out.yissum_did;
run;
*%plot_est_change(var=YIS);
title;

title "YIS Logistic Regression";
* These will omit missing values from the analysis because they don't have a DV;
proc glimmix data=bt_data noclprint;
	class identifier randomization redcap_event_name degree department_randomization;
 	model yis_ispos(event = "1") = department_randomization degree randomization|redcap_event_name / solution DIST=bin LINK=Logit oddsratio;
 	random intercept / subject=identifier type=un;
 	lsmeans randomization * redcap_event_name / oddsratio pdiff cl ilink;
 	ods output diffs=out.yis_ispos_diffs lsmeans=out.yis_ispos_lsmeans tests3=out.yis_ispos_t3 SolutionF=out.yis_ispos_sol;
 	output out=out.yis_ispos_predOut predicted(blup ilink)=predProbs;
run;
title;

* LS3_TOT  ----------------------------------------------------;
title "UCLA Loneliness Scale";
* Find who has a missing value;
data ls3_tot_cc;
	set bt_data_orig;
	where ls3_tot is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from ls3_tot_cc); 
quit;

/*proc sgplot data = bt_data;
	vbox ls3_tot / group = randomization category = redcap_event_name;
run;*/

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model ls3_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.ls3_tot_diffs lsmeans=out.ls3_tot_lsmeans tests3=out.ls3_tot_t3 SolutionF=out.ls3_tot_sol lsmestimates=out.ls3_tot_did;
run;

*%plot_est_change(var=LS3);
title;

* SCSSF_TOT ---------------------------------------------------;
title "SCSSF";
* Find who has a missing value;
data scssf_cc;
	set bt_data_orig;
	where scssf_sum_tot is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from scssf_cc); 
quit;

* Four data points were identified as outliers, after incomplete cases were filtered out of the data; 
ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox scssf_sum_tot / group = randomization category = redcap_event_name;
run;

proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
/*data bt_data;
	set bt_data;
	exclude = 0;
	if scssf_sum_tot >= 87.0833 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
	if scssf_sum_tot >= 80.75 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
run;*/

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model scssf_sum_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.scssf_sum_tot_diffs lsmeans=out.scssf_sum_tot_lsmeans tests3=out.scssf_sum_tot_t3 SolutionF=out.scssf_sum_tot_sol lsmestimates=out.scssf_sum_tot_did;
run;
*%plot_est_change(var=SCS);
title;

/*title "SCSSF Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name degree department_randomization;
 	model scssf_sum_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.scssf_sum_tot_diffs lsmeans=out.scssf_sum_tot_lsmeans tests3=out.scssf_sum_tot_t3 SolutionF=out.scssf_sum_tot_sol lsmestimates=out.scssf_sum_tot_did;
 	run;
title;*/


* FI_TOT ------------------------------------------------------;
* Does excluding outliers change the outcome of the results?;
title "Flourishing Index";
* Find who has a missing value;
data fi_cc;
	set bt_data_orig;
	where fi_tot is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from fi_cc); 
quit;

* Two data points were identified as outliers;
/*ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox fi_tot / group = randomization category = redcap_event_name;
run;

proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	if fi_tot <= 4.1 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
	if fi_tot <= 2.5 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
run;*/

* no covariates needed;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name;
 	model fi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.fi_tot_diffs lsmeans=out.fi_tot_lsmeans tests3=out.fi_tot_t3 SolutionF=out.fi_tot_sol;
run;
*%plot_est_change(var=FI);
title;

/*title "Flourishing Index Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name;
 	model fi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.fi_tot_diffs lsmeans=out.fi_tot_lsmeans tests3=out.fi_tot_t3 SolutionF=out.fi_tot_sol lsmestimates=out.fi_tot_did;
run;*/

* SFI_TOT -----------------------------------------------------;
title "Secure Flourishing Index";
* Find who has a missing value;
data sfi_cc;
	set bt_data_orig;
	where sfi_tot is missing;
	keep identifier;
run;

* Create a complete case data set;
proc sql;
  create table bt_data as
  select * from bt_data_orig
  where identifier not in (select identifier from sfi_cc); 
quit;


/*ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox sfi_tot / group = randomization category = redcap_event_name;
run;

proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	if sfi_tot <= 4 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
	if sfi_tot >= 9 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
run;*/

* no covariates needed;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name;
 	model sfi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.sfi_tot_diffs lsmeans=out.sfi_tot_lsmeans tests3=out.sfi_tot_t3 SolutionF=out.sfi_tot_sol lsmestimates=out.sfi_tot_did;
run;
*%plot_est_change(var=SFI);

/*title "Secure Flourishing Index Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name;
 	model sfi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.sfi_tot_diffs lsmeans=out.sfi_tot_lsmeans tests3=out.sfi_tot_t3 SolutionF=out.sfi_tot_sol lsmestimates=out.sfi_tot_did;
run;*/