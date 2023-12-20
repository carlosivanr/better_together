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



* Map a library to a directory name ----------------------------;
libname out 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data/multiple_imputation';

libname mi 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data';

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

ods trace off;

* 1. Imputation phase;         
* Imputation performed in a separate step. This just loads the file into the work library;
data mi_mvn;
	set mi.mi_mvn;
run;

* 2. Analyze & pool each DV;

* 2.1 mbi_ee_sum -----------------------------------------------;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization(ref = "Waitlist") redcap_event_name degree(ref = "MDorDO");
 	model mbi_ee_sum = degree randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
 	ods output SolutionF=mixparms LSMEstimates=interactions;
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


* 2.9 fi_tot - not included in manuscript;
/*proc mixed data=mi_mvn noclprint covtest namelen=100;
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
run; */

* 2.10 sfi_tot;
proc mixed data=mi_mvn noclprint covtest namelen=100;
	by _imputation_;
	class identifier randomization redcap_event_name;
 	model sfi_tot = randomization|redcap_event_name / solution;
 	repeated / subject=identifier type=un r rcorr;
 	lsmeans randomization * redcap_event_name / pdiff cl;
 	lsmestimate randomization*redcap_event_name "Interaction Term Confidence Interval" 1 -1 -1 1 / cl;
	ods output SolutionF=mixparms;
run;

proc mianalyze parms(classvar = full) = mixparms;
	class randomization redcap_event_name;
	modeleffects intercept randomization redcap_event_name randomization*redcap_event_name;
	ods output ParameterEstimates=out.sfi_tot_mi;
run;
