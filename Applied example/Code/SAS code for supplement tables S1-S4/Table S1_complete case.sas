libname  yc    "C:\HSPH\manuscripts\Flourishing" ; *my home directory;

*************Table 2 & 3*****************;

data association;
set yc.flourish_new_v2;
/*
proc reg data=association; 
model social_z=A1SEPA_z A1PAGE_M2 A1PRSEX raceA_2 raceA_3 A1SE2 A1SE3 A1SE4 A1PC1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5
                A1SE8c mom_smk dad_smk B1PA58 A1SE6_2 A1SE6_3 A1SE6_4/clb;
run;
*/

if A1STATUS=2 and B1STATUS=2; *restrict to those who participated in both SAQ and phone interview;
if nmiss (of A1SEPA A1PAGE_M2 sibling A1SE9 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1PC1 CEDUC4cat A1SE7 A1SE8c A1PC14 mom_smk dad_smk B1PA58 A1SE6)>0 then delete; 

raceA_2=(raceA=2); raceA_3=(raceA=3);CEDUC4cat_2=(CEDUC4cat=2); CEDUC4cat_3=(CEDUC4cat=3); CEDUC4cat_4=(CEDUC4cat=4); 
A1SE7_2=(A1SE7=2); A1SE7_3=(A1SE7=3);A1SE7_4=(A1SE7=4);A1SE7_5=(A1SE7=5);A1SE7_6=(A1SE7=6);
A1SE6_2=(A1SE6=2); A1SE6_3=(A1SE6=3);A1SE6_4=(A1SE6=4);

label A1SEPA_z='parental warmth';

proc reg data=association; 
model social_z=A1SEPA_z A1PAGE_M2 A1PRSEX raceA_2 raceA_3 A1SE2 A1SE3 A1SE4 A1PC1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5 A1SE7_6
                A1SE8c mom_smk dad_smk B1PA58 A1SE6_2 A1SE6_3 A1SE6_4/clb;
run;

data yc.mydata;
set association;
run;

/***Linear REGRESSION*****/
%macro estimate(outcome);

proc reg data=association; 
model &outcome.=A1SEPA_z A1PAGE_M2 A1PRSEX raceA_2 raceA_3 A1SE2 A1SE3 A1SE4 A1PC1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5 A1SE7_6
                A1SE8c mom_smk dad_smk B1PA58 A1SE6_2 A1SE6_3 A1SE6_4/clb;
title "&outcome.";
ods output ParameterEstimates=estimate1;
run;

data estimate2;
length outcome $50. Label $50.;
set estimate1;
outcome="&outcome.";
run;

data output_&outcome.;
set estimate2;
keep outcome Label Estimate StdErr LowerCL UpperCL Probt;
where Label="parental warmth";
run;

proc datasets nolist; delete estimate2; run;
%mend;

%estimate(outcome=flourish_z);
%estimate(outcome=flourish_dz);
%estimate(outcome=emotion_z);
%estimate(outcome=social_z);
%estimate(outcome=psych_z);
%estimate(outcome=B1SPOSAFz);
%estimate(outcome=B1SQ1z);
%estimate(outcome=B1SSWBMSz);
%estimate(outcome=B1SSWBSIz);
%estimate(outcome=B1SSWBAOz);
%estimate(outcome=B1SSWBSCz);
%estimate(outcome=B1SSWBSAz);
%estimate(outcome=B1SPWBA1z);
%estimate(outcome=B1SPWBE1z);
%estimate(outcome=B1SPWBG1z);
%estimate(outcome=B1SPWBR1z);
%estimate(outcome=B1SPWBU1z);
%estimate(outcome=B1SPWBS1z);
            
data output_linear;
retain outcome Label Estimate StdErr LowerCL UpperCL Probt;
set output_flourish_z output_emotion_z output_psych_z output_social_z 
output_B1SPOSAFz output_B1SQ1z output_B1SSWBMSz output_B1SSWBSIz output_B1SSWBAOz output_B1SSWBSCz output_B1SSWBSAz output_B1SPWBA1z output_B1SPWBE1z 
output_B1SPWBG1z output_B1SPWBR1z output_B1SPWBU1z output_B1SPWBS1z;
keep outcome Label Estimate StdErr LowerCL UpperCL Probt;
run;

ods csv file='./complete_normal_output.csv' ;
proc print data=output_linear; run;
ods csv close;


/***POISSON REGRESSION*****/
data poisson;
set association;

%macro estimate(outcome);
proc genmod data=poisson descending; 
class M2ID;
model &outcome.=A1SEPA_z A1PAGE_M2 A1PRSEX raceA_2 raceA_3 A1SE2 A1SE3 A1SE4 A1PC1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5
                A1SE8c mom_smk dad_smk B1PA58 A1SE6_2 A1SE6_3 A1SE6_4/dist=poisson link=log;
repeated subject=M2ID;
estimate 'parental warmth' A1SEPA_z 1;
title "&outcome.";
ods output estimates=estimate2;
run;

data estimate2;
length label $50.;
set estimate2;
outcome="&outcome.";
run;

/*combine output datasets*/
data output_&outcome.;
set estimate2;
keep Outcome Label MeanEstimate MeanLowerCL MeanUpperCL ProbChiSq;
where index(Label, 'Exp') ne 1;
run;
proc datasets nolist; delete estimate2; run;
%mend;

%estimate(outcome=B1SBMIc);
%estimate(outcome=smoke);
%estimate(outcome=binge_c);
%estimate(outcome=B1PDEPDX);
%estimate(outcome=flourish_c);

data output_poisson;
retain Outcome Label MeanEstimate MeanLowerCL MeanUpperCL ProbChiSq;
set output_B1SBMIc output_smoke output_binge_c output_B1PDEPDX  output_flourish_c;
keep Outcome Label MeanEstimate MeanLowerCL MeanUpperCL ProbChiSq;
run;

ods csv file='./poisson_table2_complete_case.csv';
proc print data=output_poisson; run;
ods csv close;


/***BINOMIAL REGRESSION*****/

data binomial;
set association;

%macro estimate(outcome);
proc logistic data=binomial descending; 
model &outcome.=A1SEPA_z A1PAGE_M2 A1PRSEX raceA_2 raceA_3 A1SE2 A1SE3 A1SE4 A1PC1 sibling CEDUC4cat_2 CEDUC4cat_3 CEDUC4cat_4 A1PC14 A1SE9 A1SE7_2 A1SE7_3 A1SE7_4 A1SE7_5
                A1SE8c mom_smk dad_smk B1PA58 A1SE6_2 A1SE6_3 A1SE6_4;
title "&outcome.";
where &outcome. ne .;
ods output OddsRatios=estimate1;
run;

data estimate2;
length outcome $50. ;
set estimate1;
outcome="&outcome.";
run;

proc contents data=estimate2; run;

/*combine output datasets*/
data output_&outcome.;
set estimate2;
keep outcome label oddsRatioEst LowerCL UpperCL;
where Effect="A1SEPA_z";
run;
proc datasets nolist; delete estimate2; run;
%mend;

%estimate(outcome=B1SA62G);
%estimate(outcome=oth_sub); 
%estimate(outcome=B1PANXTD);

data output_binomial;
retain outcome label oddsRatioEst LowerCL UpperCL;
set output_B1SA62G  output_oth_sub output_B1PANXTD;
keep outcome label oddsRatioEst LowerCL UpperCL;
run;

ods csv file='./complete_binomial_output.csv' ;
proc print data=output_binomial; run;
ods csv close;



