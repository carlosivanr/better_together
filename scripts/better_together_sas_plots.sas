/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Carlos Rodriguez, Ph.D. Dept. of Family Medicine. CU Anschutz
July 20, 2023
Better Together Clinician Coaching 2023
Plot of means for all DVs

Variables --------
randomization --> Waitlist Control or Intervention

redcap_event_name --> Pre or Post intervention data collection

Maslach's Burnout Inventory (MBI) - 3 subscales
1. Emotional Exhaustion (MBI_EE)
2. Depersonalization (MBI_DP)
3. Personal Accomplishment (MBI_PA)

Young's Impostor Syndrome Scale (YIS)

Moral Injury Symptom Scale for Healcare Providers (MISSHP)

UCLA Loneliness Scale (LS3)

Neff's Self-Compassion Scale Short Form (SCSSF)

Flourishing Index (FI)

Secure Flourishing Index (SFI)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

* MBI_EE_SUM  -------------------------------------------------;
title "Mean MBI_EE by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=mbi_ee_sum group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* MBI_DP_SUM  -------------------------------------------------;
title "Mean MBI_DP by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=mbi_dp_sum group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* MBI_PA_SUM  -------------------------------------------------;
title "Mean MBI_PA by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=mbi_pa_sum group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* MISSHP_TOT  -------------------------------------------------;
title "Mean MISS by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=misshp_tot group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* YISSUM ------------------------------------------------------;
title "Mean YIS by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=yissum group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* LS3_TOT  ----------------------------------------------------;
title "Mean UCLA LS by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=ls3_tot group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* SCSSF_TOT ---------------------------------------------------;
title "Mean SCSSF by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=scssf_tot group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* FI_TOT ------------------------------------------------------;
title "Mean FI by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=fi_tot group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;


* SFI_TOT -----------------------------------------------------;
title "Mean SFI by Arm";
proc sgplot data=bt_data;
   vline redcap_event_name / response=sfi_tot group=randomization stat=mean limitstat=stderr;
   yaxis label='Mean +/- SEM';
   xaxis label = 'Event' discreteorder=data;
run;
