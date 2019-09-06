
############################## READ IN SAS DATA ############################## 

##### This part takes ~5-10 min to run, so is commented out #####
# read in SAS data from Ying
setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Private archive components/Prepped MIDUS data")
library(sas7bdat)
d = read.sas7bdat("flourish_ying.sas7bdat")
nrow(d) # should be 2,773

# sanity check for inclusion criteria in beginning of Ying's SAS file 9
# should always be 2
table(d$A1STATUS)
table(d$B1STATUS)
# yes

# write as csv to avoid lengthy read-in process
write.csv(d, "flourish_ying.csv")

# read in Ying's data as csv to avoid SAS conversion process
setwd("Prepped data")
d = read.csv("flourish_ying.csv", header = TRUE); nrow(d)
# should be 2,773 still



############################## RECODE COVARIATES PER CHEN (2017) ############################## 

library(car)

setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/git_multiple_outcomes/Applied example")
source("helper_applied_example.R")

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

# check results
library(tableone)
CreateTableOne(data=d2)


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


############################## SAVE PREPPED DATA ############################## 

setwd("~/Dropbox/Personal computer/HARVARD/THESIS/Thesis paper #2 (MO)/Private archive components/Prepped MIDUS data")
write.csv(d2, "flourish_prepped.csv")



