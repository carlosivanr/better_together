

%macro plot_est_change(var=);
*concatenate var with "_lsmeans" for the data step and sgplot;
*concatenate var with "+/- CI" for plotting;
%let data = &var._lsmeans;
%let ylab = %str(%"&var. +/- CI%");
*%put &ylab;

* Create a format for plotting --------------------------------;
* discreteorder=data option in sgplot would not work;
proc format;
	value event_fmt
	0 = "Pre"
	1 = "Post";
run;

data &data;
	set &data;
	if redcap_event_name = "Pre" then event = 0;
	else event = 1;
run;

title "Estimated change in &var.";
proc sgplot data = &data;
    scatter y=Estimate x=event / yerrorlower=Lower yerrorupper=Upper
    markerattrs=(symbol=circlefilled) group=randomization groupdisplay=cluster clusterwidth=0.05;
    series y=Estimate x = event / group=randomization groupdisplay=cluster clusterwidth=0.05;
    *yaxis label='MBI_EE +/- CI';
    yaxis label="&ylab";
   	xaxis label = 'Event' values=(0,1);
   	format event event_fmt.;
run;
title


%mend;