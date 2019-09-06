
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

proc rank groups=3 data=yc.new out=yc.new;
var A1SEPA_z;
ranks A1SEPAt;
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

array out_o [7] B1SBMIc smoke binge_c B1SA62G oth_sub B1PDEPDX B1PANXTD;
array out_n [7] B1SBMIc_n smoke_n binge_c_n B1SA62G_n oth_sub_n B1PDEPDX_n B1PANXTD_n;

do i=1 to 7;
   if out_o[i]='b.Yes' then out_n[i]=1; 
   else if out_o[i]='a.No' then out_n[i]=0;
end;
run;

proc sort data=yc.new; by _Imputation_;run;

proc freq data=yc.new;
tables A1SEPA_t1 A1SEPA_t2 A1PRSEX_1 raceA_2 raceA_3 A1SE2_1 A1SE3_1 A1SE4_1 A1PC1_1 CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14_1 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5
       A1SE8c_1 mom_smk_1 dad_smk_1 B1PA58_1 A1SE6_2 A1SE6_3 A1SE6_4 B1SBMIc smoke binge_c B1SA62G oth_sub B1PDEPDX B1PANXTD;
by _Imputation_;
run;

*parental warmth (tertile);
*Linear regression*;
data flz_mi;
set yc.new;

%macro linear(outcome);
proc reg data=flz_mi; 
model &outcome.= A1SEPA_t1 A1SEPA_t2 A1PAGE_M2 A1PRSEX_1 raceA_2 raceA_3 A1SE2_1 A1SE3_1 A1SE4_1 A1PC1_1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14_1 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5 A1SE7_6
                 A1SE8c_1 mom_smk_1 dad_smk_1 B1PA58_1 A1SE6_2 A1SE6_3 A1SE6_4/clb;
by _Imputation_;
title "&outcome.";
ods output ParameterEstimates=estimate1;
run;

proc mianalyze parms=estimate1;  
modeleffects A1SEPA_t1 A1SEPA_t2 ; 
ods output parameterestimates=estimate2; 
run;

data estimate2;
length outcome $50. Parm $50.;
set estimate2;
outcome="&outcome.";
run;

data output_&outcome.;
set estimate2;
keep outcome Parm Estimate StdErr LCLMean UCLMean Probt;
run;

proc datasets nolist; delete estimate2; run;
%mend;

%linear(outcome=flourish_z);
%linear(outcome=flourish_dz);
%linear(outcome=emotion_z);
%linear(outcome=psych_z);
%linear(outcome=social_z);
%linear(outcome=B1SPOSAFz);
%linear(outcome=B1SQ1z);
%linear(outcome=B1SPWBA1z);
%linear(outcome=B1SPWBE1z);
%linear(outcome=B1SPWBG1z);
%linear(outcome=B1SPWBR1z);
%linear(outcome=B1SPWBU1z);
%linear(outcome=B1SPWBS1z);
%linear(outcome=B1SSWBMSz);
%linear(outcome=B1SSWBSIz);
%linear(outcome=B1SSWBAOz);
%linear(outcome=B1SSWBSCz);
%linear(outcome=B1SSWBSAz);
               
data output_linear;
length outcome $50. Parm $50.;
retain outcome Parm Estimate StdErr LCLMean UCLMean Probt;
set output_flourish_z output_flourish_dz output_emotion_z output_psych_z output_social_z output_B1SPOSAFz output_B1SQ1z output_B1SPWBA1z output_B1SPWBE1z 
    output_B1SPWBG1z output_B1SPWBR1z output_B1SPWBU1z output_B1SPWBS1z output_B1SSWBMSz output_B1SSWBSIz output_B1SSWBAOz output_B1SSWBSCz output_B1SSWBSAz;
keep outcome Parm Estimate StdErr LCLMean UCLMean Probt;
run;

ods csv file='./TableS2_mi_ter_linear.csv';
proc print data=output_linear; run;
ods csv close;

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

%poisson(outcome=B1SBMIc_n);
%poisson(outcome=smoke_n);
%poisson(outcome=binge_c_n);
%poisson(outcome=B1PDEPDX_n);

data output_poisson;
length outcome $50. Parm $50.;
retain outcome RR RR_lci RR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
set  output_B1SBMIc_n output_smoke_n output_binge_c_n output_B1PDEPDX_n;
keep outcome RR RR_lci RR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
run;

ods csv file='./TableS2_mi_ter_poisson.csv';
proc print data=output_poisson; run;
ods csv close;


/***BINOMIAL REGRESSION*****/
data binomial;
set yc.new;

%macro binomial(outcome);
proc logistic data=binomial descending; 
model &outcome.= A1SEPA_t1 A1SEPA_t2 A1PAGE_M2 A1PRSEX_1 raceA_2 raceA_3 A1SE2_1 A1SE3_1 A1SE4_1 A1PC1_1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14_1 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5 A1SE7_6
                 A1SE8c_1 mom_smk_1 dad_smk_1 B1PA58_1 A1SE6_2 A1SE6_3 A1SE6_4; 
by _Imputation_;
title "&outcome.";
ods output parameterestimates=estimate1;
run;

proc mianalyze parms=estimate1;  
modeleffects A1SEPA_t1 A1SEPA_t2 ; 
ods output parameterestimates=estimate2; 
run;

data estimate2;
length outcome $50. Parm $50.;
set estimate2;
outcome="&outcome.";
OR=put(exp(Estimate),8.2);
OR_lci=put(exp(lclmean),8.2);
OR_hci=put(exp(uclmean),8.2);
label  OR="Odds Ratio"
       OR_lci="Lower 95% CL for OR"
       OR_hci="Upper 95% CL for OR";
run;

/*combine output datasets*/
data output_&outcome.;
set estimate2;
keep outcome OR OR_lci OR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
run;
proc datasets nolist; delete estimate2; run;
%mend;

%binomial(outcome=B1SA62G);
%binomial(outcome=oth_sub); 
%binomial(outcome=B1PANXTD);

data output_binomial;
retain outcome OR OR_lci OR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
set output_B1SA62G output_oth_sub output_B1PANXTD;
keep outcome OR OR_lci OR_hci Parm Estimate StdErr LCLMean UCLMean Probt;
run;

ods csv file='./TableS2_mi_ter_binomial.csv';
proc print data=output_binomial; run;
ods csv close;









