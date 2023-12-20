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
libname out 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data/multiple_imputation_ttest';

libname mi 'C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/better_together/data';


* 1. Imputation phase;         
* Imputation performed in a separate step. This just loads the file into the work library;
data mi_mvn;
	set mi.mi_mvn;
run;


* For each dv
1. get the DV, identifier, redcap_event_name, and randomization columns
2. Convert to wide, constructing a difference score
3. Run the model by imputation
4. Pool results
5. Output results to disk.;

data vars;
	input dv $13.;
	datalines;
mbi_ee_sum
mbi_dp_sum
mbi_pa_sum
misshp_tot
yissum
ls3_tot
scssf_sum_tot
sfi_tot
;
run;


%macro dothis;

* Place the dependent variables into a macro variable (dvs);
proc sql;
    Select distinct dv into :dvs separated by ' ' from vars;
quit;

*%let thisstring = yissum;

* For loop, for each dv;
%do i=1 %to %sysfunc(countw(&dvs));
	
    %let thisstring=%scan(&dvs,&i,%str( ));
    %put &thisstring;
    /* Perform some action using macro variable &thisstring */
   
   	* Create a subset of the data with only the variables of interest;
	data sub_set;
		set mi_mvn;
   		keep _imputation_ identifier randomization redcap_event_name &thisstring;
   	run;
   
   * Convert sub_set from long to wide;
   proc transpose data = sub_set out = wide;
   		by _imputation_ identifier;
   		id redcap_event_name;
   		var &thisstring;
   run;
   
   * Create a data set with the randomization variables to merge;
   data randomization;
   		set bt_data;
   		where redcap_event_name = "Pre";
   		keep identifier randomization;
   run;
   
   * Sort the randomization data set;
   proc sort data = randomization;
   		by identifier;
   run;

   * Sort the wide data set;   
   proc sort data = wide;
   		by identifier;
   run;
   
   * Merge the two datasets ;
   data wide;
   		merge wide randomization;
   		by identifier;
   run;
   
   * Create the diff variable;
   data wide;
   	set wide;
   	diff = Post - Pre;
   run;
   
   * Sort the wide variable again;
   proc sort data = wide;
   	by _imputation_;
   run;
   
   *ods trace on;
   proc glm data=wide namelen=100;
   	by _imputation_;
	class randomization;
 	model diff = randomization / solution;
 	ods output ParameterEstimates=parms;
   run;

	*Remove the spaces from the parameters column;
	data parms;
		set parms;
		parameter = compress(parameter);
    run;  
         
    *Pool results and output the file;     
	proc mianalyze parms(classvar = full) = parms;
		*class randomization;
		modeleffects intercept randomizationIntervention;
		ods output ParameterEstimates=out.&thisstring._ttest_mi;
	run;
	
	
run;   
%end;
%mend;
%dothis;
