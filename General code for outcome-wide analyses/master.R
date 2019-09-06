
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                    PREP WORK                               #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

############################## SET YOUR DIRECTORY LOCATIONS ############################## 

rm(list=ls())

# location of root directory where all the below directories are found
root.dir = "~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Linked to OSF (OWP)/General code for outcome-wide analyses"
setwd(root.dir)

# location of data
data.dir = paste(root.dir, "/Fake data", sep="")

# location of code files
code.dir = root.dir

# location to save results tables
results.dir = paste(root.dir, "/Fake results", sep="")

# location to save imputations and resamples
stochastic.results.dir = paste(root.dir, "/Fake results/R objects saved in analysis", sep="")

# location of data codebook
codebook.dir = paste(root.dir, "/Fake data", sep="")

# location of data
data.dir = paste(root.dir, "/Fake data", sep="")

# location of code files
code.dir = root.dir

# read in data
setwd(data.dir)
library(readr)
d = suppressMessages( read_csv("fake_data.csv") )
d = as.data.frame(d)

# load the helper code
setwd(code.dir)
source("helper_functions.R")


############################## SET VARIABLE NAMES ############################## 

# outcomes that will be modeled with OLS
Ylin = c( "Ylin1",
          "Ylin2",
          "Ylin3",
          "Ylin4" )

# outcomes that will be modeled with logistic regression
Ybin = c( "Ybin1",
          "Ybin2",
          "Ybin3",
          "Ybin4",
          "Ybin5" )

# outcomes that will be modeled with Poisson regression
# none in this example
Ycount = NA

# exposure of interest
Xname = "X"

# adjusted covariates in MI analyses other than exposure of interest
Cnames.MI  = c( "C1",
              "C2" )

# adjusted covariates in CC analyses other than exposure of interest
# here it's the same as for MI, but potentially you might have different covariates
# for the two approaches
Cnames.CC = Cnames.MI

# all the analysis variables
analysis.vars = c(Cnames.CC, Cnames.MI, Xname, Ylin, Ybin, Ycount)

# look for name mismatches between the above lists and the actual dataset
#  i.e., see which ones aren't in dataset
# should be NA
library(testthat)
expect_equal( TRUE, is.na( analysis.vars[ !analysis.vars %in% names(d) ] ) )

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


##### Look At Correlations Among Variables #####
# this is to foreshadow problems with mice() due to collinearity
# needs to be done in this code chunk, prior to making variables categorical
corr = round( cor( d[ complete.cases(d), ] ), 2 )

# are the any variables with large correlations?
corr[ corr > 0.8 & corr < 1 ]

# look at highest correlation (not 1) for each variable
apply( corr, 2, function(x) max( abs(x[x < 1]) ) )


##### Which Binary Outcomes are Rare? #####
# this is used when computing E-values for logistic regression
# from most common to least common
prevalence = sort( apply( d[ , names(d) %in% Ybin ], 
             2, 
             function(x) sum(x[!is.na(x)]) / length(x[!is.na(x)]) ),
              decreasing = TRUE )
# save list of rare binaries for use in analysis post-processing
setwd(results.dir)
write.csv( data.frame( name = names( prevalence[ prevalence < .1 ] ) ),
           "list_of_rare_binaries.csv",
           row.names = FALSE )

########################### MAKE IMPUTATIONS (OR READ THEM IN) ########################### 

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
CreateTableOne(data=imps[[1]], includeNA=TRUE)


########################### RECODE VARIABLES ###########################

##### Recode the Original Dataset #####
# look at original codings
library(tableone)
CreateTableOne(data=d, includeNA = TRUE)

# here you can make any needed derived variables
# (e.g., standardized continuous variables)
# d = make_derived_vars(d,
#                       var.names = Ylin)

##### Recode the Imputed Datasets #####
setwd(stochastic.results.dir)
setwd("Imputed datasets as csvs")

# read in each relevant imputed dataset as csv and modify it directly
# currently this loop is not actually making any changes
for ( i in 1:M ) {
  imp = as.data.frame( suppressMessages( read_csv( paste("imputed_dataset_", i, ".csv", sep="") ) ) )

  # here you can make any needed derived variables
  # (e.g., standardized continuous variables)
  # imp = make_derived_vars(imp,
  #                         var.names = Ylin)
  
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


# read them back in to have in convenient list
# read in existing imputations
# we're doing this even if impute.from.scratch=TRUE to have same data format
# i.e., a list of imputed datasets instead of a mids object
setwd(stochastic.results.dir)
setwd("Imputed datasets as csvs")

imps = lapply( list.files(),
               function(x) suppressMessages(read_csv(x)) )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                  MI RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

########################### RUN #0 (TEST): SET ANALYSIS PARAMETERS ###########################

# dry run: OLS with no resampling

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

# resampling parameters
# should we run from saved imputations or re-impute?
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


########################### RUN #1: OLS / resample = TRUE / TMLE = FALSE ########################### 

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

# resampling parameters
# should we run from saved imputations or re-impute?

# no resampling
resample = TRUE
resample.from.scratch = TRUE
B.resamp = 200 # number of resamples (this is just a toy example; way too small in practice)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis.R")


########################### RUN #2: OLS / resample = FALSE / TMLE = TRUE ########################### 

# we will now run analysis.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = TRUE

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
resample = FALSE
resample.from.scratch = FALSE

setwd(code.dir)
source("analysis.R")


########################### RUN #3: logistic / resample = FALSE / TMLE = FALSE ########################### 

# we will now run analysis.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE


# should we overwrite previous results files?
write.results = TRUE

setwd(code.dir)
source("analysis.R")




########################### RUN #4: logistic / resample = FALSE / TMLE = TRUE ########################### 

# we will now run analysis.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# bookmark

# TMLE or standard MLE?
TMLE = TRUE


# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?
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
#                                  CC RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

missingness = "CC"

########################### RUN #5 (TEST): OLS CC ###########################

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

setwd(code.dir)
source("analysis.R")



########################### RUN #6: LOGISTIC CC ###########################

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE

setwd(code.dir)
source("analysis.R")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                             MERGE RESULTS FILES                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

setwd(code.dir)
source("merge_results.R")






