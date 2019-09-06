
########################### MAKE MI DATASETS ########################### 

if ( missingness == "MI" ) {
  ##### Make Imputations #####
  library(mice)
  
  if ( impute.from.scratch == TRUE ) {
    
    # make smart predictor matrix
    pred = quickpred(d)

    ##### Generate Imputations #####
    library(mice)
    ini = mice(d,
               m=1,
               predictorMatrix = pred,
               method = "pmm",
               maxit = 0 )
    ini$loggedEvents
    if ( !is.null(ini$loggedEvents) ) stop("Imputation trouble: Dry run has logged events! Adjust the code in make_imputations.R.")
  
 
    # check default methods
    ini$method
    
    imps = mice( d,
                 m=M,
                 predictorMatrix = pred,
                 #ridge = 1e-02,  # this can help with collinearity; not needed here
                 method = "pmm")
    
    # any complaints?
    head(imps$loggedEvents)
    if ( !is.null(imps$loggedEvents) ) stop("Imputation trouble: Imputations have logged events! Adjust the code in make_imputations.R.")
    

    # make sure there is no missing data in the imputations
    any.missing = apply( complete(imps,1), 2, function(x) any(is.na(x)) ) # should be FALSE
    if ( any(any.missing) == TRUE ) stop("Imputed datasets have missing data! Look at logged events.")
  
    # first imputed dataset
    head( complete(imps, 1) )
    # if this line returns an error about complete() not being applicable
    #  for a mids objects (which is a lie), restart R

    if ( write.results == TRUE ) {
      
      # save imputations for reproducibility
      setwd(stochastic.results.dir)
      save( imps, file = "imputed_datasets.RData" )
      
      # also save imputed datasets as csvs for Ying
      setwd(stochastic.results.dir)
      setwd("Imputed datasets as csvs")
      
      for (i in 1:M) {
        write.csv( complete(imps,i),
                   paste("imputed_dataset_", i, ".csv", sep="") )
      }
    }
    
  } # end loop for impute.from.scratch = TRUE
  
  # read in existing imputations
  # we're doing this even if impute.from.scratch=TRUE to have same data format
  # i.e., a list of imputed datasets instead of a mids object
  setwd(stochastic.results.dir)
  setwd("Imputed datasets as csvs")
  
  library(readr)
  imps = lapply( list.files(),
                 function(x) suppressMessages(read_csv(x)) )
}

