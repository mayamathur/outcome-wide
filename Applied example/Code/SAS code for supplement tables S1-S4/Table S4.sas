
libname  yc    "C:\HSPH\manuscripts\Outcome-wide design\Imputed datasets as csvs" ; *home directory;

/*****************************************
Read in the imputed dataset (created in R)
******************************************/
proc import datafile = 'C:\HSPH\manuscripts\Outcome-wide design\Imputed datasets as csvs\imputed_dataset_1.csv' out = work.data_1 dbms = CSV; run;
proc import datafile = 'C:\HSPH\manuscripts\Outcome-wide design\Imputed datasets as csvs\imputed_dataset_2.csv' out = work.data_2 dbms = CSV; run;
proc import datafile = 'C:\HSPH\manuscripts\Outcome-wide design\Imputed datasets as csvs\imputed_dataset_3.csv' out = work.data_3 dbms = CSV; run;
proc import datafile = 'C:\HSPH\manuscripts\Outcome-wide design\Imputed datasets as csvs\imputed_dataset_4.csv' out = work.data_4 dbms = CSV; run;
proc import datafile = 'C:\HSPH\manuscripts\Outcome-wide design\Imputed datasets as csvs\imputed_dataset_5.csv' out = work.data_5 dbms = CSV; run;

data data_1; set data_1; _Imputation_=1; run;
data data_2; set data_2; _Imputation_=2; run;
data data_3; set data_3; _Imputation_=3; run;
data data_4; set data_4; _Imputation_=4; run;
data data_5; set data_5; _Imputation_=5; run;

data yc.new;
set data_1 data_2 data_3 data_4 data_5;
run;

proc sort data=yc.new; by _Imputation_;run; 
proc rank groups=3 data=yc.new out=yc.new;
var A1SEPA_z;
ranks A1SEPAt;
by _Imputation_;
run;

proc rank groups=2 data=yc.new out=yc.new;
var flourish_z emotion_z psych_z social_z B1SPOSAFz B1SQ1z B1SPWBA1z B1SPWBE1z 
    B1SPWBG1z B1SPWBR1z B1SPWBU1z B1SPWBS1z B1SSWBMSz B1SSWBSIz B1SSWBAOz B1SSWBSCz B1SSWBSAz;
ranks flourish_d emotion_d psych_d social_d B1SPOSAFd B1SQ1d B1SPWBA1d B1SPWBE1d 
      B1SPWBG1d B1SPWBR1d B1SPWBU1d B1SPWBS1d B1SSWBMSd B1SSWBSId B1SSWBAOd B1SSWBSCd B1SSWBSAd;
by _Imputation_;
run;

data yc.new; 
set yc.new;

A1SEPA_t1=(A1SEPAt=1); A1SEPA_t2=(A1SEPAt=2);
raceA_2=(raceA="b.Black"); raceA_3=(raceA="c.Others");CEDUC4cat_2=(CEDUC4cat='b.High school'); CEDUC4cat_3=(CEDUC4cat='c.Some college'); CEDUC4cat_4=(CEDUC4cat='d.College degree or more'); 
A1SE7_2=(A1SE7='b.Small town'); A1SE7_3=(A1SE7='c.Medium town');A1SE7_4=(A1SE7='d.Suburbs');A1SE7_5=(A1SE7='e.City'); A1SE7_6=(A1SE7='f.Moved around');
A1SE6_2=(A1SE6='b.Somewhat important'); A1SE6_3=(A1SE6='c.Not very important');A1SE6_4=(A1SE6='d.Not at all important');
A1PRSEX_1=(A1PRSEX="b.Female");

array orig [9] A1SE2 A1SE3 A1SE4 A1PC1 A1PC14 A1SE8c mom_smk dad_smk B1PA58;
array new  [9] A1SE2_1 A1SE3_1 A1SE4_1 A1PC1_1 A1PC14_1 A1SE8c_1 mom_smk_1 dad_smk_1 B1PA58_1;

do i=1 to 9;
   new[i]=orig[i];
   if orig[i]="a.No" then new[i]=0;
   else if orig[i]="b.Yes" then new[i]=1;
end;
run;

proc sort data=yc.new; by _Imputation_;run;

*Poisson regression*;
data poi_mi;
set yc.new;

%macro poisson(outcome);
proc genmod data=poi_mi descending; 
model &outcome.= A1SEPA_t1 A1SEPA_t2 A1PAGE_M2 A1PRSEX_1 raceA_2 raceA_3 A1SE2_1 A1SE3_1 A1SE4_1 A1PC1_1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14_1 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5 A1SE7_6
                 A1SE8c_1 mom_smk_1 dad_smk_1 B1PA58_1 A1SE6_2 A1SE6_3 A1SE6_4/dist=poisson link=log;
by _Imputation_;
title "&outcome.";
ods output parameterestimates=estimate1;
run;

proc mianalyze parms=estimate1;  
modeleffects A1SEPA_t1 A1SEPA_t2; 
ods output parameterestimates=estimate2; 
run;

data estimate2;
length outcome $50. Parm $50.;
set estimate2;
outcome="&outcome.";
RR=put(exp(Estimate),8.2);
RR_lci=put(exp(lclmean),8.2);
RR_hci=put(exp(uclmean),8.2);
label  RR="Risk Ratio"
       RR_lci="Lower 95% CL for RR"
       RR_hci="Upper 95% CL for RR"
run;

/*combine output datasets*/
data output_&outcome.;
set estimate2;
keep outcome RR RR_lci RR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
run;
proc datasets nolist; delete estimate2; run;
%mend;

%poisson(outcome=flourish_d);
%poisson(outcome=emotion_d);
%poisson(outcome=psych_d);
%poisson(outcome=social_d);
%poisson(outcome=B1SPOSAFd);
%poisson(outcome=B1SQ1d);
%poisson(outcome=B1SPWBA1d);
%poisson(outcome=B1SPWBE1d);
%poisson(outcome=B1SPWBG1d);
%poisson(outcome=B1SPWBR1d);
%poisson(outcome=B1SPWBU1d);
%poisson(outcome=B1SPWBS1d);
%poisson(outcome=B1SSWBMSd);
%poisson(outcome=B1SSWBSId);
%poisson(outcome=B1SSWBAOd);
%poisson(outcome=B1SSWBSCd);
%poisson(outcome=B1SSWBSAd);

data output_poisson;
length outcome $50. Parm $50.;
retain outcome RR RR_lci RR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
set  output_flourish_d output_emotion_d output_psych_d output_social_d output_B1SPOSAFd output_B1SQ1d output_B1SPWBA1d output_B1SPWBE1d 
     output_B1SPWBG1d output_B1SPWBR1d output_B1SPWBU1d output_B1SPWBS1d output_B1SSWBMSd output_B1SSWBSId 
     output_B1SSWBAOd output_B1SSWBSCd output_B1SSWBSAd;
keep outcome RR RR_lci RR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
run;

ods csv file='./TableS4_mi_ter_poisson.csv';
proc print data=output_poisson; run;
ods csv close;









