
# Contact: Maya Mathur (mmathur@stanford.edu)


########################### FNs FOR FORMATTING RESULTS ###########################

# round while keeping trailing zeroes
my_round = function(x, digits) {
  formatC( round( x, digits ), format='f', digits=digits )
}


# format a p-value with scientific notation stars for cutoffs
# star.cutoffs: cutoffs for *, **, ***, etc., provided in any order
format_pval = function( p,
                        digits = 3,
                        star.cutoffs = NA ) {
  
  if (p >= 0.01) string = as.character( my_round( p, digits ) )
  if (p < 0.01 & p > 10^-5 ) string = formatC( p, format = "E", digits = 2 )
  if ( p < 10^-5 ) string = "< 1E-05"
  
  if ( ! is.na(star.cutoffs[1]) ) {
    
    # put in descending order
    star.cutoffs = sort(star.cutoffs, decreasing = TRUE)
    
    for ( i in 1 : length(star.cutoffs) ) {
      if ( p < star.cutoffs[i] ) string = paste( string, "*", sep="" )
    }
  }
  
  return(string)
}

# example
# p = seq( 0, .2, 0.001 )
# vapply( p, format_pval, "asdf" )
# vapply( p, function(x) format_pval( x, star.cutoffs = c( 0.01, 0.05) ), "asdf" )



########################### FN: RECODE VARIABLES DURING DATA PREP ###########################

# standardize a variable
standardize = function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# var.names: variable names to standardize (otherwise standardizes all
#  continuous vars)
make_derived_vars = function(dat,
                             var.names = NULL) {
  
  # if no var.names provided, standardize all continuous vars
  if ( is.null(var.names) ) {
    # find all continuous variables in dataset, not just continuous
    n.levels = apply( dat, 2,
                      function(x) length( unique( x[ !is.na(x) ] ) ) )
    library(dplyr)
    numerics = select_if(d, is.numeric)
    
    # truly continuous variables have >2 levels (not 0/1)
    #  and aren't factors or characters
    is.cont = (n.levels > 2) & ( names(dat) %in% names(numerics) )
    
    # standardize the continuous variables
    dat[ , is.cont ] = apply( dat[ , is.cont ],
                              2, 
                              function(x) standardize(x) )
  } else {
    # if provided variable names to standardize
    dat[ , var.names ] = apply( dat[ , var.names ],
                              2, 
                              function(x) standardize(x) )
  }
 
  # dichotomize the dietary quality score variable at its top tertile
  cutoff = as.numeric( quantile( dat$nAHEI11a, 2/3, na.rm = TRUE ) )
  temp = rep(NA, nrow(dat))
  temp[ dat$nAHEI11a > cutoff ] = 1
  temp[ dat$nAHEI11a <= cutoff ] = 0
  dat$nAHEI11a = temp
  
  return(dat)
}

# recode binary variables in several different formats into 0/1
# for undoing the above function
binarize = function(x) {
  
  # needs to be character, otherwise TRUE/FALSE and 0/1 variables
  # aren't distinguished in the conditional statements to follow
  levels.char = as.character( sort( unique(x) ) )
  
  if ( all( levels.char %in% c("a.No", "b.Yes") ) ) {
    x = as.character(x)
    xbin = rep(NA, length(x))
    xbin[ x == "b.Yes" ] = 1
    xbin[ x == "a.No" ] = 0
    return(xbin)
  } 
  
if ( all( levels.char %in% c("FALSE", "TRUE") ) ) {
    x = as.character(x)
    xbin = rep(NA, length(x))
    xbin[ x == TRUE ] = 1
    xbin[ x == FALSE ] = 0
    return(xbin)
  } 
  
  # if the variable is already 0/1, leave it alone
  # this is used in the Poisson part of analysis functions
  # so that it can flexibly handle a binary variable coded as factor
  #  or as 0/1
  if ( all( levels.char %in% c("0", "1") ) ) return(x)
  
  stop("x needs to be either already 0/1, TRUE/FALSE, or 'a.No'/'b.Yes'")
  
}

# examples
# binarize( c(1,1,1,0,1) )
# binarize( c("a.No", "b.Yes") )
# binarize(c("No", "Yes"))
# binarize( c(FALSE, TRUE, FALSE) )


########################### FN: POOL IMPUTED SEs VIA RUBIN'S RULES ###########################

# see Marshall "Combining estimates" paper, pg 3

# ests: ests from m imputations
# ses: ses from m imputations
rubin_se = function( ests, ses ){
  
  m = length(ests)
  
  # within-imputation variance
  Ubar = mean( ses^2 )
  
  # between-imputation variance
  B = (1 / (m-1)) * sum( ( ests - mean(ests) )^2 )
  
  # overall SE
  return( sqrt( Ubar + (1 + (1/m)) * B ) )
}



########################### FN: ANALYZE ONE DATASET WITHOUT MISSING DATA ###########################

# pass the CC data (or imputed dataset with no missing data)

# d = dataset
# X = quoted name of single exposure of interest
# C = quoted vector of adjusted covariate names
# Ys = quoted names of all outcomes that are accounted for in Bonferroni correction (not
#  just those using this link fn)
# alpha = alpha level
# resample = Should we draw resamples for multiplicity correction?
# B = number of resamples
# model = which model to fit ("OLS", "poisson", or "logistic")
# TMLE = should we use TMLE?

analyze_CC_dataset = function( d,
                               X,
                               C = NA,
                               Ys, 
                               alpha = 0.05,
                               resample = FALSE,
                               B = 1000,  
                               model = "OLS",
                               TMLE = FALSE ) {
  

  ##### Fit All Models #####
  samp.res = dataset_result( d = d,
                             X = X,
                             C = C,
                             Ys = Ys,  # all outcome names
                             alpha = alpha,
                             center.stats = FALSE,
                             bhat.orig = NA,
                             model = model,
                             TMLE = TMLE )

  ##### Generate Resamples #####
  
  if ( resample == TRUE ) {

    # note: if X is categorical, this will have "subscript out of bounds" error
    #  because X gets renamed in regression output
    resamps = resample_resid(  X = X,
                                            C = C,
                                            Ys = Ys,
                                            d = d,
                                            alpha = alpha,
                                            resid = samp.res$resid,
                                            bhat.orig = samp.res$b,
                                            B=B,
                                            cores = 8 )
  } else {
    resamps = NA
  }
  
  ##### Return All The Things #####
  return( list( samp.res = samp.res,
                resamps = resamps ) )
  
}



########################### FN: FIT MODEL FOR ONE DATASET ###########################

# this is a generalization of NRejections::fit_model
fit_model = function( X,
                      C = NA,
                      Y,
                      Ys,  # used for subsetting bhat.orig
                      d,
                      center.stats = FALSE,
                      bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
                      model, 
                      TMLE,
                      alpha = 0.05 ) {

  if ( length(X) > 1 ) stop("X must have length 1")
  
  # all covariates, including the one of interest
  if ( all( is.na(C) ) ) covars = X else covars = c( X, C )
  
  ################# OLS w/o TMLE ################# 
  if ( model == "OLS" & TMLE == FALSE ) {
  
    if ( all( is.na(C) ) ) mod = lm( d[[Y]] ~ d[[X]] )
    # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
    else mod = lm( d[[Y]] ~ ., data = d[ , covars] )
    
    # stats for covariate of interest
    m.stats = summary(mod)$coefficients[ 2, ]
    
    # should we center stats by the original-sample estimates?
    if( !center.stats ) {
      b = m.stats[["Estimate"]]
      tval = m.stats[["t value"]]
      SE = m.stats[["Std. Error"]]
    }
    if( center.stats ) {
      b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
      SE = m.stats[["Std. Error"]]
      tval = b / SE
    }
    
    df = nrow(d) - length(covars) - 1
    pval = 2 * ( 1 - pt( abs( b / SE ), df ) )
    resid.SD = summary(mod)$sigma
    resids = residuals(mod)
    analyzed.n = length(resids)
  }
  

  ################# Logistic Regression w/o TMLE ################# 
  if ( model == "logistic" & TMLE == FALSE ) {
    
    # if binary variable, make sure it's 0/1 instead of "b.No"/"a.Yes"
    tryCatch( {
      tempY = binarize( d[[Y]] )
    }, error = function(err) {
      browser()
    })
    
  
    if ( all( is.na(C) ) ) {
      m = glm( tempY ~ d[[X]], family = "binomial" )
    } else {
      # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
      m = glm( tempY ~ ., data = d[ , covars], family = "binomial" )
    }
    
    # stats for covariate of interest
    m.stats = summary(m)$coefficients[ 2, ]
    
    # NOTE THAT THESE ARE ACTUALLY Z-VALS, NOT TVALS
    # should we center stats by the original-sample estimates?
    if( !center.stats ) {
      b = m.stats[["Estimate"]]
      tval = m.stats[["z value"]]
      SE = m.stats[["Std. Error"]]
    }
    if( center.stats ) {
      b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
      SE = m.stats[["Std. Error"]]
      tval = b / SE
    }
    
    pval = 2 * ( 1 - pnorm( abs( b / SE ) ) )
    df = NA
    resid.SD = NA
    resids = NA
    analyzed.n = length(m$residuals)
  }
  
  
  ################# Poisson Regression w/o TMLE ################# 
  if ( model == "poisson" & TMLE == FALSE ) {
    
    # if binary variable, make sure it's 0/1 instead of "b.No"/"a.Yes"
    tempY = binarize( d[[Y]] )
    
    if ( all( is.na(C) ) ) {
      m = glm( tempY ~ d[[X]], family = "poisson" )
    } else {
      # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
      m = glm( tempY ~ ., data = d[ , covars], family = "poisson" )
    }
    
    # stats for covariate of interest
    m.stats = summary(m)$coefficients[ 2, ]
    
    # should we center stats by the original-sample estimates?
    if( !center.stats ) {
      b = m.stats[["Estimate"]]
      tval = m.stats[["z value"]]  # NOTE THAT THESE ARE ACTUALLY Z-VALS, NOT TVALS
      SE = m.stats[["Std. Error"]]
    }
    if( center.stats ) {
      b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
      SE = m.stats[["Std. Error"]]
      tval = b / SE
    }
    
    pval = 2 * ( 1 - pnorm( abs( b / SE ) ) )
    df = NA
    resid.SD = NA
    resids = NA
    analyzed.n = length(m$residuals)
  }
  

  ################# OLS w/ TMLE ################# 

  if ( model == "OLS" & TMLE == TRUE ) {

    # if there is missing data (because d is the original data and we're doing CC),
    #  remove it to avoid complaints from tmle()
    d = d[ , c(X, Y, Cnames) ]
    d = d[ complete.cases(d), ]
    
    require(tmle)
    require(SuperLearner)

    
    # for debugging
    cat( "\nAbout to attempt tmle for", Y)

    tryCatch( {
      # defaults to super-learning
      mod = tmle( Y = d[[Y]],
                  A = d[[X]],
                  W = d[ , Cnames ] )
      
    }, error = function(err) {
      # needs to be superassignment because inside fn
      browser()
    } )
    
    
    # # for debugging
    cat( "\nSurvived tmle for", Y)

    # for continuous outcome
    b = mod$estimates$ATE$psi
    SE = sqrt( mod$estimates$ATE$var.psi )
    pval = mod$estimates$ATE$pvalue
    analyzed.n = nrow(d)  # works because here we've cut down d to complete cases

    # NA because inference will be done by bootstrapping
    df = NA
    tval = NA

    # below is because because tmle package doesn't estimate SD(Y | X,C)
    # so using marginal SD(Y) instead, though this means the E-values will be conservative
    resid.SD = sd(d[[Y]])
    resids = NA
  }
  
  
  ################# Logistic w/ TMLE ################# 

  if ( model == "logistic" & TMLE == TRUE ) {
 
    # if there is missing data (because d is the original data and we're doing CC),
    #  remove it to avoid complaints from tmle()
    d = d[ , c(X, Y, Cnames) ]
    d = d[ complete.cases(d), ]
    
    
    require(tmle)
    require(SuperLearner)
    
    #browser()
    
    # for debugging
    cat( "\nAbout to attempt tmle for", Y)
    print( paste("About to attempt tmle for", Y) )
    
    # defaults to super-learning
    
    tryCatch( {
      # defaults to super-learning
      mod = tmle( Y = d[[Y]],
                  A = d[[X]],
                  W = d[ , Cnames ],
                  family = "binomial" )
      
    }, error = function(err) {
      # needs to be superassignment because inside fn
      browser()
    } )
    
    
    # # for debugging
    cat( "\nSurvived tmle for", Y)
    print( paste("Survived tmle for", Y) )
    
    # for continuous outcome
    b = log( mod$estimates$OR$psi )  # log for consistency with non-TMLE code
    SE = sqrt( mod$estimates$OR$var.log.psi )
    pval = mod$estimates$OR$pvalue
    
    # NA because inference will be done by bootstrapping
    df = NA
    tval = NA
    resid.SD = NA
    resids = NA
    analyzed.n = nrow(d)  # works because here we've cut down d to complete cases
  }
  
  
  
  if ( model == "poisson" & TMLE == TRUE ) {
  
    # if there is missing data (because d is the original data and we're doing CC),
    #  remove it to avoid complaints from tmle()
    d = d[ , c(X, Y, Cnames) ]
    d = d[ complete.cases(d), ]
    
    
    require(tmle)
    require(SuperLearner)
    
    # defaults to super-learning
    mod = tmle( Y = d[[Y]],
                A = d[[X]],
                W = d[ , Cnames ],
                family = "poisson" )
    
    # for continuous outcome
    b = mod$estimates$OR$psi
    SE = sqrt( mod$estimates$OR$var.log.psi )
    pval = mod$estimates$OR$pvalue
    
    # NA because inference will be done by bootstrapping
    df = NA
    tval = NA
    resid.SD = NA
    resids = NA
    analyzed.n = nrow(d)  # works because here we've cut down d to complete cases
  }
  

  
  stats = data.frame( outcome = Y,
                      b = b,
                      SE = SE,
                      resid.SD = resid.SD,  # used for computing E-value of beta-hat
                      df = df,
                      pval = pval,
                      tval = tval,
                      reject = pval < alpha,
                      analyzed.n = analyzed.n )
  
  return( list( stats = stats,
                resids = as.numeric(resids)
                ) )
}



########################### FN: FIT ONE TYPE OF MODEL (LINK) FOR ALL SPECIFIED Ys ###########################


# this is a generalization of NRejections::dataset_result
# see ?dataset_result in that package for more info

dataset_result = function( d,
                           X,
                           C = NA,
                           Ys,  # all outcome names
                           alpha = 0.05,
                           center.stats = TRUE,
                           bhat.orig = NA,
                           model = "OLS",
                           TMLE = FALSE ) { 
  
  
  if ( length(X) > 1 ) stop("X must have length 1")
  
  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest
  if ( any( all( is.na(C) ) ) ) covars = X
  else covars = c( X, C )
  
  # get the correct bhat for the outcome we're using
  
  # this is a list of lists:
  #  length is equal to number of outcomes
  #  each entry is another list
  # with elements "pval" (scalar) and "resid" (length matches number of subjects)
  
  lists = lapply( X = Ys,
                  FUN = function(y) fit_model( X = X,
                                               C = C,
                                               Y = y,
                                               Ys = Ys,
                                               d = d,
                                               center.stats = center.stats,
                                               bhat.orig = bhat.orig,
                                               model = model, 
                                               TMLE = TMLE,
                                               alpha = alpha ) )
  
  # "flatten" the list of lists
  u = unlist(lists)
  pvals = as.vector( u[ names(u) == "stats.pval" ] )
  tvals = as.vector( u[ names(u) == "stats.tval" ] )
  bhats = as.vector( u[ names(u) == "stats.b" ] )
  ses = as.vector( u[ names(u) == "stats.SE" ] )
  resid.sds = as.vector( u[ names(u) == "stats.resid.SD" ] )
  analyzed.n = as.vector( u[ names(u) == "stats.analyzed.n" ] )

  # save residuals
  # names of object u are resid.1, resid.2, ..., hence use of grepl 
  # need to make sure the resids are all same length in order to make matrix from them
  resid.lengths = lapply( lists, function(x) length(x$resids) )
  resid.lengths = unlist( resid.lengths ) 
  if( length( unique(resid.lengths) ) == 1 ) {
    mat = matrix( u[ grepl( "resid", names(u) ) ], byrow=FALSE, ncol=length(Ys) ) 
  } else {
    warning("Outcome models had different sample sizes (probably due to missing data), so not returning residuals.")
    mat = matrix( NA, byrow=FALSE, ncol=length(Ys) ) 
  }

  resid = as.data.frame(mat)
  names(resid) = Ys
  
  # returns vector for number of rejections at each alpha level
  # length should match length of .alpha
  n.reject = vapply( X = alpha, FUN = function(a) sum( pvals < a ), FUN.VALUE=-99 )

  return( list( rej = n.reject,
                SEs = ses,
                resid.sds = resid.sds,
                tvals = tvals,
                bhats = bhats,
                pvals = pvals,
                analyzed.n = analyzed.n,
                resid = resid ) )
}




# see eponymous function documentation in NRejections package
resample_resid = function( d,
                           X,
                           C = NA,
                           Ys,
                           alpha,
                           resid,
                           bhat.orig,
                           B=2000,
                           cores = NULL ) {
  if ( length(X) > 1 ) stop("X must have length 1")
  
  if ( all( is.na(C) ) ) covars = X
  else covars = c( X, C )
  
  ##### Check for Bad Input
  # warn about too-small N or B
  if ( nrow(d) < 100 ) warning("Sample size is too small to ensure good asymptotic behavior of resampling.")
  if ( B < 1000 ) warning("Number of resamples is too small to ensure good asymptotic behavior of resampling.")
  
  # compute Y-hat using residuals
  Yhat = d[, Ys] - resid
  
  # fix the existing covariates
  Xs = as.data.frame( d[ , covars ] )
  if( all( is.na(C) ) ) names(Xs) = X
  
  # run all bootstrap iterates
  registerDoParallel(cores=cores) 
  
  # run all resamples: takes ~10 min
  r = foreach( i = 1:B, .combine=rbind ) %dopar% {
    
    # resample residuals and add them to fitted values
    ids = sample( 1:nrow(d), replace=TRUE )
    b = as.data.frame( cbind( Xs, Yhat + resid[ids,] ) )
    
    bhats = rep( NA, length(Ys) )
    
    bt.res = dataset_result( X = X,
                             C = C,
                             Ys = Ys, 
                             d = b,
                             alpha = alpha,
                             center.stats = TRUE,
                             bhat.orig = bhat.orig )
    
    # return all the things
    list( rej = bt.res$rej,
          pvals = bt.res$pvals,
          tvals = bt.res$tvals )
    
  } ###### end r-loop (parallelized bootstrap)
  
  # close cluster to avoid Windows issues with CRAN submission
  stopImplicitCluster()
  
  # resampled p-value matrix for Westfall
  # rows = Ys
  # cols = resamples
  p.bt = do.call( cbind, r[ , "pvals" ] )
  
  # resampled test statistic matrix (uncentered) for Romano
  # rows = Ys
  # cols = resamples
  t.bt = do.call( cbind, r[ , "tvals" ] )
  
  # number of rejections
  rej.bt = do.call( cbind, r[ , "rej" ] )
  #rej.bt.0.05 = rej.bt[1,]
  #rej.bt.0.01 = rej.bt[2,]
  
  return( list( p.bt = as.matrix(p.bt),
                t.bt = as.matrix(t.bt),
                rej.bt = as.matrix(rej.bt) ) )
  
}




########################### FN: INDUCE MCAR MISSINGNESS ###########################

# this was only used for testing the code on fake data

# induce missingness
# https://stackoverflow.com/questions/18837896/degrading-data-randomly-with-pre-existing-missingness

# del.amount = number of observations in dataset to make missing

degradefunction <- function(x, del.amount){
  
  # 1) indicate which cells are NA (works with matrix or df)
  preNAs     <- is.na(x)
  # 2) how many cells are eligible to be degraded?
  OpenSpots  <- prod(dim(x)) - sum(preNAs)
  # 3) of these, select del.amount for replacement with NA
  newNas     <- sample(1:OpenSpots, size = del.amount, replace = FALSE)
  # 4) impute these NAs, ignoring the original NAs
  x[!preNAs][newNas] <- NA
  x
}





