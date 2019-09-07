
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                    PREP WORK                               #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 


############################## SET YOUR DIRECTORY LOCATIONS ############################## 

rm(list=ls())

# location of root directory where all the below directories are found
root.dir = "~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper"
setwd(root.dir)

# location of data
data.dir = paste(root.dir, "/Private archive (data)", sep="")

# location of code files
code.dir = paste(root.dir, "/Linked to OSF (OWP)/Applied example/Code", sep="")

# location to save results tables
results.dir = paste(root.dir, "/Linked to OSF (OWP)/Applied example/Results", sep="")

# location to save imputations and resamples
stochastic.results.dir = paste(root.dir, "/Linked to OSF (OWP)/Applied example/Results/R objects saved in analysis", sep="")

# location of data codebook
codebook.dir = paste(root.dir, "/Linked to OSF (OWP)/Applied example", sep="")

# read in data
# setwd(data.dir)
# library(readr)
# d = suppressMessages( read_csv("flourish_prepped.csv") )
# d = as.data.frame(d)

# TEMP
setwd("~/Desktop")
d = read.csv("data_prepped.csv")

# load the helper code
setwd(code.dir)
source("helper_functions.R")


############################## SET VARIABLE NAMES ############################## 

# outcomes that will be modeled with OLS
Ylin = c("flourish_z",
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

# outcomes that will be modeled with logistic regression (rare)
Ybin = c( 
          "B1SA62G",  # CHANGED FROM B1SA62A
          "B1PANXTD" )

# outcomes that will be modeled with Poisson regression (common)
# none in this example
Ycount = c("B1SBMIc",
           "smoke",
           "binge_c",
           "oth_sub",
           "B1PDEPDX")

# exposure of interest
Xname = "A1SEPA_z"

# adjusted covariates in MI analyses other than exposure of interest
Cnames.MI  = c( 
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

# adjusted covariates in CC analyses other than exposure of interest
# here it's the same as for MI, but potentially you might have different covariates
# for the two approaches
Cnames.CC = Cnames.MI

# all the analysis variables
analysis.vars = c(Cnames.CC, Cnames.MI, Xname, Ylin, Ybin, Ycount)
analysis.vars = analysis.vars[ !is.na(analysis.vars) ]

# look for name mismatches between the above lists and the actual dataset
#  i.e., see which ones aren't in dataset
# should be NA
library(testthat)
expect_equal( TRUE, length( analysis.vars[ !analysis.vars %in% names(d) ] ) == 0 )

# remove any unnecessary variables from dataset; otherwise MI breaks down
d = d[, names(d) %in% analysis.vars ]


########################### DESCRIPTIVE STATISTICS ########################### 

##### Missingness on Each Variable #####
# sorted from most to least missing
sort( apply( d[ , names(d) ], 
             2, 
             function(x) sum(is.na(x)) / length(x) ),
      decreasing = TRUE )

##### Missingness on Each Outcome #####
# sorted from most to least missing
sort( apply( d[ , names(d) %in% c(Ylin, Ybin) ], 
       2, 
       function(x) sum(is.na(x)) / length(x) ),
      decreasing = TRUE )


# ##### Look At Correlations Among Variables #####
# # this is to foreshadow problems with mice() due to collinearity
# # needs to be done in this code chunk, prior to making variables categorical
# corr = round( cor( d[ complete.cases(d), ] ), 2 )
# 
# # are the any variables with large correlations?
# corr[ corr > 0.8 & corr < 1 ]
# 
# # look at highest correlation (not 1) for each variable
# apply( corr, 2, function(x) max( abs(x[x < 1]) ) )


##### Which Binary Outcomes are Rare? #####
# this is used when computing E-values for logistic regression
# from most common to least common
# prevalence = sort( apply( d[ , names(d) %in% Ybin ], 
#              2, 
#              function(x) sum( binarize(x)[!is.na(x)]) / length(x[!is.na(x)]) ),
#               decreasing = TRUE )

prev_fn = function(x) {
  x = x[!is.na(x)]
  return( sum(x == "b.Yes") / length(x) )
}

library(dplyr)
prevalence = sort( d %>% select(Ybin, Ycount) %>%
  summarise_all(prev_fn), decreasing = TRUE )

# save list of rare binaries for use in analysis post-processing
setwd(results.dir)
write.csv( data.frame( name = names(prevalence)[ prevalence < .1 ] ),
           "list_of_rare_binaries.csv",
           row.names = FALSE )

########################### MAKE IMPUTATIONS (OR READ THEM IN) ########################### 

# recode character variables to factors to appease mice
library(dplyr)
sum(sapply(d, is.character))  # check number of character vars
d = d %>% mutate_if(sapply(d, is.character), as.factor)
sum(sapply(d, is.character))  # check again; should be 0


# imputation parameters
missingness = "MI" # missing data method ("MI" or "CC")
impute.from.scratch = FALSE

# number of imputations
# increase above 5 if some vars have >30% missingness
# see Graham "How many imputations are really needed?"
M = 5

# should we overwrite previous results files?
write.results = TRUE

# this script automatically handles case in which we're not making
#  imputations from scratch
setwd(code.dir)
source("make_imputations.R")

# look at one imputed dataset
library(tableone)
CreateTableOne(data=imps[[4]], includeNA=TRUE)


########################### RECODE VARIABLES ###########################

##### Recode the Original Dataset #####
# look at original codings
library(tableone)
CreateTableOne(data=d, includeNA = TRUE)

# make derived variables (parental warmth tertile)
d = make_derived_vars(d)

##### Recode the Imputed Datasets #####
setwd(stochastic.results.dir)
setwd("Imputed datasets as csvs")

# read in each relevant imputed dataset as csv and modify it directly
# currently this loop is not actually making any changes
for ( i in 1:M ) {
  imp = as.data.frame( suppressMessages( read_csv( paste("imputed_dataset_", i, ".csv", sep="") ) ) )

  imp = make_derived_vars(imp)

  # overwrite the old one
  write.csv( imp, paste("imputed_dataset_", i, ".csv", sep="") )
}

# look at the last imputation
CreateTableOne(data=imp,
               includeNA = TRUE)  # second argument only works for categoricals


##### Save Sanity-Check Table 1 #####
# check Table 1 on original dataset with missingness
( tab1 = CreateTableOne(data=d,
                        includeNA = TRUE) ) # second argument only works for categoricals
# save as csv
tab1mat = print( tab1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
setwd( results.dir )
write.csv(tab1mat, file = "table1_sanity.csv")


# # read them back in to have in convenient list
# # read in existing imputations
# # we're doing this even if impute.from.scratch=TRUE to have same data format
# # i.e., a list of imputed datasets instead of a mids object
setwd(stochastic.results.dir)
setwd("Imputed datasets as csvs")

imps = lapply( list.files(),
               function(x) suppressMessages(read_csv(x)) )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                          MAIN-TEXT RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# ########################### RUN #0a (TEST OLS): SET ANALYSIS PARAMETERS ###########################
# 
# # dry run: OLS with no resampling
# 
# missingness = "CC" # missing data method ("MI" or "CC")
# 
# 
# # set link ("OLS", "poisson", "logistic")
# # spelling needs to match options within function fit_model
# link = "OLS"
# 
# # TMLE or standard MLE?
# TMLE = FALSE
# 
# # resampling parameters
# # should we run from saved imputations or re-impute?
# resample = FALSE
# resample.from.scratch = FALSE
# B.resamp = 1000
# 
# # familywise alpha
# # we always set this to 0.05
# alpha = 0.05
# 
# # alpha for individual tests
# # we did both 0.05 and 0.01
# alpha.within = 0.05
# 
# setwd(code.dir)
# source("analysis.R")
# 
# 
########################### RUN #0 (TEST LOGISTIC): SET ANALYSIS PARAMETERS ###########################

# dry run: OLS with no resampling

missingness = "CC" # missing data method ("MI" or "CC")


# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE
B.resamp = 1000

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### RUN #0 (TEST POISSON): SET ANALYSIS PARAMETERS ###########################

# dry run: OLS with no resampling

missingness = "CC" # missing data method ("MI" or "CC")


# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "poisson"

# TMLE or standard MLE?
TMLE = FALSE

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE
B.resamp = 1000

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")



########################### RUN #1: OLS / resample = TRUE / TMLE = FALSE ########################### 

missingness = "MI" # missing data method ("MI" or "CC")
M = 5
# we will now run analysis.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE   # ~~~ change
resample.from.scratch = FALSE
B.resamp = 1000 # number of resamples (this is just a toy example; way too small in practice)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### RUN #2: LOGISTIC / resample = FALSE / TMLE = FALSE ########################### 

missingness = "MI"
# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### RUN #3: POISSON / resample = FALSE / TMLE = FALSE ########################### 

missingness = "MI"
# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                SUPPLEMENT RESULTS - TERTILES & TMLE                        #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

########################### SUPPLEMENTAL RUN #1 - TERTILES : OLS / resample = FALSE / TMLE = FALSE ########################### 

Xname = "A1SEPA_top"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### SUPPLEMENTAL RUN #2 - TERTILES : LOGISTIC / resample = FALSE / TMLE = FALSE ########################### 

Xname = "A1SEPA_top"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### SUPPLEMENTAL RUN #3 - TERTILES : POISSON / resample = FALSE / TMLE = FALSE ########################### 

Xname = "A1SEPA_top"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "poisson"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")



########################### SUPPLEMENTAL RUN #4 - TERTILES : OLS / resample = FALSE / TMLE = TRUE ########################### 

Xname = "A1SEPA_top"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = TRUE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### SUPPLEMENTAL RUN #5 - TERTILES : LOGISTIC / resample = FALSE / TMLE = TRUE ########################### 

Xname = "A1SEPA_top"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = TRUE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### SUPPLEMENTAL RUN #6 - TERTILES : POISSON / resample = FALSE / TMLE = TRUE ########################### 

Xname = "A1SEPA_top"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "poisson"

# TMLE or standard MLE?
TMLE = TRUE

# should we overwrite previous results files?
write.results = TRUE


# no resampling
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")



########################### POST-PROCESSING ON SUPPLEMENTAL RESULTS ########################### 





