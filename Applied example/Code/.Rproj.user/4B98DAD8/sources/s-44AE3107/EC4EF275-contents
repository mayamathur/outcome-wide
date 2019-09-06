

# edit: working on this...need to include the binary variables as well

############################## READ IN SAS DATA ############################## 


# read in Ying's data as csv to avoid SAS conversion process
setwd("~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Private archive (data)")
d = read.csv("flourish_ying.csv", header = TRUE); nrow(d)
# should be 2,773 still



############################## RECODE COVARIATES PER CHEN (2017) ############################## 

library(car)

setwd("~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Linked to OSF (OWP)/Applied example/Code")
source("helper_functions.R")

# initialize new dataset with recoded variables
d2 = d

# recode some categoricals
# see the file "00. Parental warmth and flourishing_data step.sas"
# also see MIDUS codebooks or "Analysis dataset codebook" for the original source
d2$A1PRSEX = recode( d2$A1PRSEX, " 1='a.Male'; 2='b.Female' ")
table(d$A1PRSEX, d2$A1PRSEX)

d2$raceA = recode( d2$raceA, " 1='a.White'; 2='b.Black'; 3='c.Others' ")
table(d$raceA, d2$raceA)

d2$CEDUC4cat = recode( d2$CEDUC4cat, " 1='a.Less than high school'; 2='b.High school'; 3='c.Some college'; 4='d.College degree or more' ")
table(d$CEDUC4cat, d2$CEDUC4cat)

d2$A1SE7 = recode( d2$A1SE7, " 1='a.Rural'; 2='b.Small town'; 3='c.Medium town'; 4='d.Suburbs'; 5='e.City'; 6='f.Moved around'; 7='g.Unsure';")
table(d$A1SE7, d2$A1SE7)

d2$A1SE6 = recode( d2$A1SE6, " 1='a.Very important'; 2='b.Somewhat important'; 3='c.Not very important'; 4='d.Not at all important'; 7='e.Unsure' ")
table(d$A1SE6, d2$A1SE6)

# recode binaries from yes=1 / no=2 to 0/1 scheme
recode_binary( c( "A1SE2", "A1SE3", "A1SE4", "A1PC1", "B1PA58", "A1PC14" ) )
# look at one as an example
table(d$A1SE2, d2$A1SE2)


# recode other variables as in Ying's code
# "Parental warmth and flourishing data step"

# obesity
d2$B1SBMI[ d2$B1SBMI < 14.23 | d2$B1SBMI > 82.307 ] = NA
d2$B4PBMI[ d2$B4PBMI < 14.986 | d2$B4PBMI > 65.088 ] = NA
d2$B1SBMI[ is.na(d2$B1SBMI) ] = d2$B4PBMI[ is.na(d2$B1SBMI) ]
# categorical version (overweight/obese indicator)
d2$B1SBMIc = NA
d2$B1SBMIc[ d2$B1SBMI < 25 ] = 0
d2$B1SBMIc[ d2$B1SBMI >= 25 ] = 1
prop.table( table(d2$B1SBMIc) )

# smoking
# *smoking;
# if B1PA39<1 or B1PA39 in (7, 8) then smoke=.;
# else if B1PA39=1 then smoke=1; *current smoker;
# else if B1PA39=2 then smoke=1; *former smoker;
# else if B1PA39=9 then smoke=0; *never smoker;
# label smoke='smoking' ;
d2$smoke = NA
d2$smoke[ d2$B1PA39 < 1 | d2$B1PA39 %in% c(7,8) ] = NA
d2$smoke[ d2$B1PA39 %in% c(1,2) ] = 1  # current or former smoker
d2$smoke[ d2$B1PA39 == 9 ] = 0 # never smoker
prop.table( table(d2$smoke) )

# binge drinking (categorical)
# *binge drinking;
# if B1PA53=97 or B1PA53=98 then binge=.;
# else if B1PA53=99 then binge=0;
# else binge=B1PA53;
# 
# if binge=. then binge_c=.;
# else if binge=0 then binge_c=0;
# else if binge>0 then binge_c=1;
d2$binge = d2$B1PA53
d2$binge[ d2$B1PA53 %in% c(97,98) ] = NA
d2$binge[ d2$B1PA53 == 99 ] = 1
# binge drinking (binary)
d2$binge_c = d2$binge
d2$binge_c[ d2$binge > 0 ] = 1

# anxiety
# if B1PANXTD<0 or B1PANXTD>1 then B1PANXTD=.; *dummy variable;
d2$B1PANXTD[ d2$B1PANXTD < 0 | d2$B1PANXTD > 1 ] = NA

# marijuana
# *marijuana use; 
# if B1SA62G=1 then B1SA62G=1;
# else if B1SA62G=2 then B1SA62G=0;
# else B1SA62G=.;
d2$B1SA62G[ d2$B1SA62G == 2 ] = 0
d2$B1SA62G[ ! d2$B1SA62G %in% c(1,0) ] = NA

# any other drug use
# if B1SA62A=1 then B1SA62A=1; *sedatives;
# else if B1SA62A=2 then B1SA62A=0;
# else B1SA62A=.;
d2$B1SA62A[ d2$B1SA62A == 2 ] = 0
d2$B1SA62A[ ! d2$B1SA62A %in% c(1,0) ] = NA

# depression 
# if B1PDEPDX<0 or B1PDEPDX>1 then B1PDEPDX=.;*dummy variable;
d2$B1PDEPDX[ d2$B1PDEPDX < 0 | d2$B1PDEPDX > 1 ] = NA




############################## MAKE CODEBOOK ############################## 

# names of adjusted covariates
# see SAS file 9
covars = c( "A1SEPA_z",
            "A1PAGE_M2",
            "A1PRSEX",
            "raceA",
            "A1SE2",
            "A1SE3",
            "A1SE4",
            "A1PC1",
            "sibling",
            "CEDUC4cat",
            "A1PC14",
            "A1SE9",
            "A1SE7",
            "A1SE8c",
            "mom_smk",
            "dad_smk",
            "B1PA58",
            "A1SE6" )

# names of outcomes
outcomes = c("flourish_z",
             "emotion_z",
             "social_z",
             "psych_z",
             "B1SPOSAFz",
             "B1SQ1z",
             "B1SSWBMSz",
             "B1SSWBSIz",
             "B1SSWBAOz",
             "B1SSWBSCz",
             "B1SSWBSAz",
             "B1SPWBA1z",
             "B1SPWBE1z",
             "B1SPWBG1z",
             "B1SPWBR1z",
             "B1SPWBU1z",
             "B1SPWBS1z")

# variables to keep
keepers = c( covars, outcomes )
d2 = d2[ , keepers ]


# check results
library(tableone)
CreateTableOne(data=d2)


############################## SAVE PREPPED DATA ############################## 

setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Private archive components/Prepped MIDUS data")
write.csv(d2, "flourish_prepped.csv")



