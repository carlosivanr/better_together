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
libname out 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data/itt';


* Convert the variables to numeric format ----------------------;
data bt_data;
	set bt_data(rename=(mbi_ee_sum = mbi_ee_char
					   mbi_dp_sum = mbi_dp_char
					   mbi_pa_sum = mbi_pa_char
					   mbi_bin = mbi_bin_char
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
	mbi_bin = input(mbi_bin_char, 15.);
	misshp_tot = input(misshp_char, 15.);
	yissum = input(yis_char, 15.);
	yis_ispos = input(yis_ispos_char, 15.);
	ls3_tot = input(ls3_char, 15.);
	scssf_sum_tot = input(scssf_char, 15.);
	fi_tot = input(fi_char, 15.);
	sfi_tot = input(sfi_char, 15.);
	drop mbi_ee_char mbi_dp_char mbi_pa_char mbi_bin_char misshp_char yis_char ls3_char scssf_char fi_char sfi_char yis_ispos_char;
run;

* Load the plotting macro;
%include "C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/scripts/functions/MACRO_plot_est_change.sas";


* MBI_EE_SUM  -------------------------------------------------;
title "MBI EE SUM";
proc sgplot data = bt_data;
	vbox mbi_ee_sum / group = randomization category = redcap_event_name;
run;

* Unstructured default ddfm. Unstructured covariance, 
  with ddfm = res reproduces gls() results with corSymm() in R;
* included degree due to missingness, but does not impact the 
	results when compared to excluding degree;	

*ods trace on;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization(ref = "Waitlist") redcap_event_name degree(ref = "MD or DO");
 	model mbi_ee_sum = degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.mbi_ee_sum_diffs lsmeans=out.mbi_ee_sum_lsmeans LSMEstimates=out.mbi_ee_sum_did tests3=out.mbi_ee_sum_t3 SolutionF=out.mbi_ee_sum_sol;
run;
*%plot_est_change(var=MBI_EE);
title;

proc freq data = bt_data;
	tables degree;
	
run;

* MBI_DP_SUM  -------------------------------------------------;
title "MBI DP SUM";
ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox mbi_dp_sum / group = randomization category = redcap_event_name;
run;

*proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	if mbi_dp_sum >= 23 and redcap_event_name = "Post" and randomization = "Intervention" then exclude = 1;
run;


* include department degree due to missingness;
/*proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_dp_sum = department_randomization degree randomization|redcap_event_name / solution ddfm=kenwardroger2;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	*ods output diffs=out.mbi_dp_sum_diffs lsmeans=out.mbi_dp_sum_lsmeans LSMEstimates=out.mbi_dp_sum_did tests3=out.mbi_dp_sum_t3 SolutionF=out.mbi_dp_sum_sol;
run;*/
*%plot_est_change(var=MBI_DP);
title;

title "MBI DP SUM Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_dp_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.mbi_dp_sum_diffs lsmeans=out.mbi_dp_sum_lsmeans LSMEstimates=out.mbi_dp_sum_did tests3=out.mbi_dp_sum_t3 SolutionF=out.mbi_dp_sum_sol;
run;
title;

* MBI_PA_SUM  -------------------------------------------------;
title "MBI PA SUM";
proc sgplot data = bt_data;
	vbox mbi_pa_sum / group = randomization category = redcap_event_name;
run;

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model mbi_pa_sum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.mbi_pa_sum_diffs lsmeans=out.mbi_pa_sum_lsmeans LSMEstimates=out.mbi_pa_sum_did tests3=out.mbi_pa_sum_t3 SolutionF=out.mbi_pa_sum_sol;
run;
*%plot_est_change(var=MBI_PA);

title "MBI Burnt Out Logistic Regression";
* These will omit missing values from the analysis because they don't have a DV;
proc glimmix data=bt_data noclprint;
	class identifier randomization redcap_event_name department_randomization degree(ref = "MD or DO");;
 	model mbi_bin(event = "1") = department_randomization degree randomization|redcap_event_name / solution DIST=bin LINK=Logit oddsratio;
 	random intercept / subject=identifier type=un;
 	lsmeans randomization * redcap_event_name / oddsratio pdiff cl ilink;
 	*ods output diffs=out.mbi_bin_diffs lsmeans=out.mbi_bin_lsmeans tests3=out.mbi_bin_t3 SolutionF=out.mbi_bin_sol;
 	output out=out.mbi_bin_predOut predicted(blup ilink)=predProbs;
run;
title;

* MISSHP_TOT  -------------------------------------------------;
title "MISSHP";
proc sgplot data = bt_data;
	vbox misshp_tot / group = randomization category = redcap_event_name;
run;

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model misshp_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
	lsmeans randomization * redcap_event_name / pdiff cl;
	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.misshp_tot_diffs lsmeans=out.misshp_tot_lsmeans LSMEstimates=out.misshp_tot_did tests3=out.misshp_tot_t3 SolutionF=out.misshp_tot_sol;
run;
*%plot_est_change(var=MISS);
title;

* YISSUM ---------------------------------------------------;
title "YIS SUM Linear Mixed Model";
proc sgplot data = bt_data;
	vbox yissum / group = randomization category = redcap_event_name;
run;

* can also be analyzed as binary variable yis_ispos;
* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model yissum = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.yissum_diffs lsmeans=out.yissum_lsmeans LSMEstimates=out.yissum_did tests3=out.yissum_t3 SolutionF=out.yissum_sol;
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
proc sgplot data = bt_data;
	vbox ls3_tot / group = randomization category = redcap_event_name;
run;

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model ls3_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.ls3_tot_diffs lsmeans=out.ls3_tot_lsmeans LSMEstimates=out.ls3_tot_did tests3=out.ls3_tot_t3 SolutionF=out.ls3_tot_sol;
run;

*%plot_est_change(var=LS3);
title;

* SCSSF_TOT ---------------------------------------------------;
* The variable is named scssf_sum_tot, but it's not really a sum.
	It's the average of all the scssf sub domains which are each
	scored as sums indvidually;
title "SCSSF";
ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox scssf_sum_tot / group = randomization category = redcap_event_name;
run;
proc print data=boxplot_data;

* include department, degree due to missingness;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name degree department_randomization;
 	model scssf_sum_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	*ods output diffs=out.scssf_sum_tot_diffs lsmeans=out.scssf_sum_tot_lsmeans LSMEstimates=out.scssf_sum_tot_did tests3=out.scssf_sum_tot_t3 SolutionF=out.scssf_sum_tot_sol;
run;
*%plot_est_change(var=SCS);
title;

title "SCSSF Sans Outliers";
* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	if scssf_sum_tot >= 54 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
	if scssf_sum_tot >= 57 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
run;

proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name degree department_randomization;
 	model scssf_sum_tot = department_randomization degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output diffs=out.scssf_sum_tot_diffs lsmeans=out.scssf_sum_tot_lsmeans LSMEstimates=out.scssf_sum_tot_did tests3=out.scssf_sum_tot_t3 SolutionF=out.scssf_sum_tot_sol;
run;
title;


* FI_TOT ------------------------------------------------------;
* Does excluding outliers change the outcome of the results?;
title "Flourishing Index";
ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox fi_tot / group = randomization category = redcap_event_name;
run;
proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	*if fi_tot <= 4 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
	*if fi_tot >= 9 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
	if fi_tot <= 2.5 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
run;

* no covariates needed;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name;
 	model fi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	*ods output diffs=out.fi_tot_diffs lsmeans=out.fi_tot_lsmeans LSMEstimates=out.fi_tot_did tests3=out.fi_tot_t3 SolutionF=out.fi_tot_sol;
run;
*%plot_est_change(var=FI);
title;

title "Flourishing Index Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name;
 	model fi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.fi_tot_diffs lsmeans=out.fi_tot_lsmeans LSMEstimates=out.fi_tot_did tests3=out.fi_tot_t3 SolutionF=out.fi_tot_sol;
run;

* SFI_TOT -----------------------------------------------------;
title "Secure Flourishing Index";
ods output sgplot=boxplot_data;
proc sgplot data = bt_data;
	vbox sfi_tot / group = randomization category = redcap_event_name;
run;
proc print data=boxplot_data;

* To include or exclude outliers, a new variable can be used to filter
 the data;
data bt_data;
	set bt_data;
	exclude = 0;
	*if sfi_tot <= 4 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
	*if sfi_tot >= 9 and redcap_event_name = "Pre" and randomization = "Intervention" then exclude = 1;
	if sfi_tot <= 3.0833 and redcap_event_name = "Pre" and randomization = "Waitlist" then exclude = 1;
run;

* no covariates needed;
proc mixed data=bt_data noclprint covtest;
	class identifier randomization redcap_event_name;
 	model sfi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	*ods output diffs=out.sfi_tot_diffs lsmeans=out.sfi_tot_lsmeans tests3=out.sfi_tot_t3 SolutionF=out.sfi_tot_sol;
run;
*%plot_est_change(var=SFI);

title "Secure Flourishing Index Sans Outliers";
proc mixed data=bt_data noclprint covtest;
	where exclude = 0;
	class identifier randomization redcap_event_name;
 	model sfi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output diffs=out.sfi_tot_diffs lsmeans=out.sfi_tot_lsmeans LSMEstimates=out.sfi_tot_did tests3=out.sfi_tot_t3 SolutionF=out.sfi_tot_sol;
run;

* Comparison of X for responders and non responders;

* Number of responders;
proc sql;
    select pre_post_complete, count(distinct identifier) as distinct_var2
    from my_data
    group by var1;
quit;

* P values;
PROC FREQ data=bt_data;
	where pre_post_complete = 1;
    TABLE  pre_post_complt/ binomial alpha = .05;
RUN;

* Confidence intervals;
proc sort data=bt_data;
	by randomization;
run;

PROC FREQ data=bt_data;
	by randomization;
    TABLE  pre_post_complete/ binomial alpha = .05;
RUN;




* Group by identifier and get the first row only;
proc sort data=bt_data;
	by identifier;
run;

data prop_data;
	set bt_data;
	where redcap_event_name = "Post";
run;



proc sort data=bt_data;
	key randomization / ascending;
run;

proc freq data=prop_data;
where pre_post_complete = 1;
tables randomization / binomial(level = "Intervention" CL=
 WALD
 WILSON
 CLOPPERPEARSON
 MIDP
 LIKELIHOODRATIO
 JEFFREYS
 AGRESTICOULL
 LOGIT
 BLAKER
 );
*weight Count;
run;











