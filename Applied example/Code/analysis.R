# Contact: Maya Mathur (mmathur@stanford.edu)

########################### PROCESS THE USER'S INPUT ########################### 

# names of outcomes with this link
if ( link == "OLS" ) Ynames = Ylin
if ( link == "poisson" ) Ynames = Ycount
if ( link == "logistic" ) Ynames = Ybin

# total number of outcomes (for Bonferroni penalization)
# not just the ones with the above-specified link
# USER NEEDS TO THINK ABOUT THIS
( n.tot.outcomes = length( c(Ylin, Ybin) ) )

if ( missingness == "CC" ) Cnames = Cnames.CC else Cnames = Cnames.MI



###########################  ANALYSES ########################### 

###### If Doing CC Analyses #####

if ( missingness == "CC" ) {
  res = analyze_CC_dataset( 
    d = d, 
    X = Xname,
    C = Cnames,
    Ys = Ynames,
    alpha = alpha.within,
    resample = resample,
    B = B.resamp,  
    model = link,
    TMLE = TMLE ) 
}



###### Analyze all imputed datasets and pool resamples #####

if ( missingness == "MI" ) {
  # initialize results
  res = list()
  
  p.bt = c()
  t.bt = c()
  rej.bt = c()
  
  bhats.unp = c()
  ses.unp = c()
  resid.sds.unp = c()
  
  
  for ( i in 1:M ) {
    
    cat("\n Analyzing imputation ", i)
    
    # "resample" is whether we're ACTUALLY going to resample, 
    #  so is also FALSE if link isn't OLS
    resample = resample.from.scratch
    if ( link != "OLS" | TMLE == TRUE ) resample = FALSE
    
    res[[i]] = analyze_CC_dataset( 
      d = imps[[i]], 
      X = Xname,
      C = Cnames,
      Ys = Ynames,
      alpha = alpha.within,
      resample = resample,
      B = B.resamp,  
      model = link,
      TMLE = TMLE )
    
    # one for each covariate
    bhats = res[[i]]$samp.res$bhats
    ( bhats.unp = rbind( bhats.unp, bhats ) )
    
    resid.sds = res[[i]]$samp.res$resid.sds
    ( resid.sds.unp = rbind( resid.sds.unp, resid.sds) )
    
    if ( TMLE == FALSE ){
      # b / se = tval
      # se = b / tval
      ses = res[[i]]$samp.res$bhats / res[[i]]$samp.res$tvals
      ( ses.unp = rbind( ses.unp, ses ) )
    } else {
      ses = res[[i]]$samp.res$SEs
      ( ses.unp = rbind( ses.unp, ses ) )
    }

    
    if (resample == TRUE) {
      ##### Concatenate the resamples #####
      ( p.bt = cbind( p.bt, res[[i]]$resamps$p.bt ) )
      ( t.bt = cbind( t.bt, res[[i]]$resamps$t.bt ) )
      ( rej.bt = cbind( rej.bt, res[[i]]$resamps$rej.bt ) )
    }
    
  }  # end imputation loop over m imputations
  
  # now p.bt, t.bt have 1 row per outcome; 1 column per resample (B=100)
  # rej.bt has 1 row per outcome; 1 column per resample (B=100)
  
  # look at results
  bhats.unp
  ses.unp
  
  # sanity check on resamples: p-values should be uniform
  if ( resample == TRUE & link == "OLS" ) {
    #hist(p.bt)
    require(testthat)
    expect_equal( mean(p.bt), 0.5, tolerance = 0.05 )
    expect_equal( mean(rej.bt), alpha.within * length(Ynames), 0.08 )
  }
  
  # save stochastic resamples for reproducibility
  if ( write.results == TRUE & link == "OLS" & resample == TRUE & resample.from.scratch == TRUE ) {
    setwd( stochastic.results.dir )
    
    # remove period from alpha-level
    if (alpha.within == 0.05) alpha.string = "alpha005"
    if (alpha.within == 0.01) alpha.string = "alpha001"
    
    write.csv( p.bt,
               paste( "resampled_OLS_pvals", "_", alpha.string, ".csv", sep="" ),
               row.names = FALSE )
    
    write.csv( t.bt,
               paste( "resampled_OLS_tvals", "_", alpha.string, ".csv", sep="" ),
               row.names = FALSE )
    
    write.csv( rej.bt,
               paste( "resampled_OLS_rej", "_", alpha.string, ".csv", sep="" ),
               row.names = FALSE )
  }
}


########################### POOL COEFFICIENTS VIA RUBIN'S RULES ########################### 


if ( missingness == "MI" ) {
  ( bhats.pool = colMeans( bhats.unp ) )
  
  ( ses.pool = vapply( 1:length(Ynames),
                       FUN = function(x) rubin_se( bhats.unp[,x], ses.unp[,x] ),
                       FUN.VALUE = 99 ) )
  
  resid.sds.pool = colMeans( resid.sds.unp )
  
  analyzed.n = NA
  
  
  ##### CI limits #####
  
  # t-based inference
  if ( link == "OLS" ) {
    # minus 2 because 1 is for intercept and 1 is for exposure of interest
    # checked against one of the fitted models :) 
    ( df = nrow(d) - 2 - length(Cnames) )  
    
    ( lo.pool = bhats.pool - qt( p = 1 - alpha/2, df = df ) * ses.pool )
    ( hi.pool = bhats.pool + qt( p = 1 - alpha/2, df = df ) * ses.pool )
    
    # p-values
    ( pvals.pool = 2 * ( 1 - pt( q = abs( bhats.pool / ses.pool ),
                                 df = df ) ) )
  } 
  
  if ( link != "OLS" ) {
    
    ( lo.pool = bhats.pool - qnorm( p = 1 - alpha/2 ) * ses.pool )
    ( hi.pool = bhats.pool + qnorm( p = 1 - alpha/2 ) * ses.pool )
    
    # p-values
    ( pvals.pool = 2 * ( 1 - pnorm( q = abs( bhats.pool / ses.pool ) ) ) )
    
  }
  
  
  # did Bonferroni reject?
  ( bonf.rej = pvals.pool < (alpha / n.tot.outcomes) )
  
}


########################### POOL RESAMPLING METHODS VIA SIMPLE CONCATENATION ########################### 


if ( link == "OLS" & resample == TRUE ) {
  
  # read in existing resamples
  if ( resample.from.scratch == FALSE ) {
    # remove period from alpha-level
    if (alpha.within == 0.05) alpha.string = "alpha005"
    if (alpha.within == 0.01) alpha.string = "alpha001"
    
    setwd( stochastic.results.dir )
    p.bt = read.csv( paste( "resampled_OLS_pvals", "_", alpha.string, ".csv", sep="" ) )
    t.bt = read.csv( paste( "resampled_OLS_tvals", "_", alpha.string, ".csv", sep="" ) )
    rej.bt = read.csv( paste( "resampled_OLS_rej", "_", alpha.string, ".csv", sep="" ) )
  }
  
  ##### Romano #####
  # test stats in t.bt are already centered
  library(StepwiseTest)
  rom = FWERkControl( bhats.pool / ses.pool,  # first argument is sample t-values, here pooled from MI
                      as.matrix( t.bt ),  # these now have resamples from all m imputations
                      k = 1,
                      alpha = alpha )
  rom.rej = as.vector(rom$Reject)
  
  ##### Ours #####
  ( theta.hat = sum( pvals.pool < alpha ) )
  
  # global test
  ( crit = quantile( rej.bt, 1 - alpha ) )
  
  # p-values for observed rejections
  ( jt.pval = sum( rej.bt >= theta.hat ) /
      length( rej.bt ) )
  
  # null interval
  ( ni.lo = quantile( rej.bt, alpha / 2 ) )
  ( ni.hi = quantile( rej.bt, 1 - alpha / 2 ) )
  
} else {
  rom.rej = NA
  theta.hat = NA
  jt.pval = NA
  ni.lo = NA
  ni.hi = NA
}



########################### E-VALUES ########################### 

require(EValue)

# this is in order to get E-values for CC results as well
if ( missingness == "CC" ) {
  bhats.pool = res$samp.res$bhats
  ses.pool = res$samp.res$SEs
  resid.sds.pool = res$samp.res$resid.sds  # to get SMD from regression coefficient
  analyzed.n = res$samp.res$analyzed.n
  
  if ( link == "OLS" ) {
    lo.pool = bhats.pool - qnorm( p = 1 - alpha/2 ) * ses.pool
   hi.pool = bhats.pool + qnorm( p = 1 - alpha/2 ) * ses.pool
  } else {
    lo.pool = bhats.pool - qt( p = 1 - alpha/2, df = df ) * ses.pool
    hi.pool = bhats.pool + qt( p = 1 - alpha/2, df = df ) * ses.pool
  }
}

##### OLS E-values #####
if ( link == "OLS" ) {
  
  # all continuous variables are already standardized by sd(Y), so no need to 
  #  standardize point estimates
  
  # if the beta-hats are standardized, this gives E-values for a 1-unit change in X 
  #  on the scale of the standardized Y
  evals.pt = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.MD( est = bhats.pool[i] / resid.sds.pool[i],
                                             se = ses.pool[i] / resid.sds.pool[i],
                                             true = 0)["E-values", "point"] ),
                     FUN.VALUE = -99 )
  
  
  evals.pt = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.MD( est = bhats.pool[i],
                                                               se = ses.pool[i],
                                                               true = 0)["E-values", "point"] ),
                     FUN.VALUE = -99 )
  
  
  
  # for each bhat, one of these will be NA (the one corresponding to the the wrong
  #  CI limit)
  evals.lo = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.MD( est = bhats.pool[i] / resid.sds.pool[i],
                                             se = ses.pool[i] / resid.sds.pool[i],
                                             true = 0)["E-values", "lower"] ),
                     FUN.VALUE = -99 )  
  
  
  evals.hi = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.MD( est = bhats.pool[i] / resid.sds.pool[i],
                                             se = ses.pool[i] / resid.sds.pool[i],
                                             true = 0)["E-values", "upper"] ),
                     FUN.VALUE = -99 ) 
}


##### Poisson Regression E-values #####
if ( link == "poisson" ) {
  
  evals.pt = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.RR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             true = 1)["E-values", "point"] ),
                     FUN.VALUE = -99 )
  
  
  # for each bhat, one of these will be NA (the one corresponding to the the wrong
  #  CI limit)
  evals.lo = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.RR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             true = 1)["E-values", "lower"] ),
                     FUN.VALUE = -99 )
  
  
  evals.hi = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.RR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             true = 1)["E-values", "upper"] ),
                     FUN.VALUE = -99 ) 
}


##### Logistic Regression E-values #####
if ( link == "logistic" ) {
  
  # figure out which variables are rare vs. not rare
  setwd(results.dir)
  # get names of rare binaries previously written at beginning of analysis
  rares = read.csv("list_of_rare_binaries.csv",
                   header = TRUE)$name
  commons = Ybin[ !Ybin %in% rares ]
  if( length(commons) > 0 ) message( paste("You fit logistic regression to the following common binary variables: ",
                                         commons, ". For these variables, E-values will use sqrt(OR) to approximate RR.",
                                         sep = "") )

  evals.pt = vapply( 1:length(bhats.pool),
                     function(i) suppressMessages( evalues.OR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             rare = ifelse( Ybin[i] %in% rares, 1, 0),
                                             true = 1)["E-values", "point"] ),
                     FUN.VALUE = -99 )
  
  
  # for each bhat, one of these will be NA (the one corresponding to the the wrong
  #  CI limit)
  evals.lo = vapply( 1:length(bhats.pool),
                     function(i) evalues.OR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             rare = FALSE,
                                             true = 1)["E-values", "lower"],
                     FUN.VALUE = -99 )
  
  
  evals.hi = vapply( 1:length(bhats.pool),
                     function(i) evalues.OR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             rare = FALSE,
                                             true = 1)["E-values", "upper"],
                     FUN.VALUE = -99 ) 
}



# combine them
evals.CI = evals.lo
evals.CI[is.na(evals.CI)] = evals.hi[is.na(evals.lo)]

# # example
# d1 = c(NA, 1, 3, NA)
# d2 = c(0, NA, NA, 5)
# d1[is.na(d1)] = d2[is.na(d1)]




# ########################### CODE SANITY CHECKS ###########################
# 
# 
# # choose one outcome at a time and look at estimates and inference
# #  on the main exposure of interest
# 
# if ( TMLE == FALSE & missingness == "MI" ) {
#   for ( i in 1:length(Ynames) ) {
# 
#     cat( "\n Manually checking", link, "outcome number ", i)
# 
#     # results for this outcome
#     bhat.man = c()
#     se.man = c()
#     pvals.man = c()
# 
#     # fit model to each imputed dataset
#     for ( j in 1:M ) {
# 
#       dat = imps[[j]]
# 
#       covars = c(Xname, Cnames)
# 
#       if ( link == "OLS" ) mod = lm( dat[[ Ynames[i] ]] ~ ., data = dat[ , covars] )
#       if ( link == "poisson" ) {
#         # if binary variable, make sure it's 0/1 instead of "b.No"/"a.Yes"
#         if ( length( unique( dat[[ Ynames[i] ]] ) ) == 2 ) tempY = binarize( dat[[ Ynames[i] ]] )
#         else tempY = dat[[ Ynames[i] ]]
# 
#         mod = glm( tempY ~ ., data = dat[ , covars], family = "poisson" )
#       }
#       if ( link == "logistic" ) mod = glm( dat[[ Ynames[i] ]] ~ ., data = dat[ , covars], family = "binomial" )
# 
#       # save within-imputation results for this imputation
#       # only saving results for exposure of interest
#       bhat.man = c( bhat.man, coef(mod)[[2]] )
#       se.man = c( se.man, summary(mod)$coefficients[2,"Std. Error"] )
#     }
# 
#     # pool results
#     bhat.pool.man = mean( bhat.man )
# 
#     # Rubin's Rules by hand
#     Ubar = mean(se.man^2)
#     B = ( 1 / (M-1) ) * sum( ( bhat.man - mean(bhat.man) )^2 )
#     total.var = Ubar + ( 1 + (1/M) ) * B
#     se.pool.man = sqrt(total.var)
# 
#     # CIs by hand
#     if ( link == "OLS" ) {
#       df = nrow(dat) - length(covars) - 1
#       t = abs( bhat.pool.man / se.pool.man )
#       pval.man = 2 * ( 1 - pt( t,
#                                df = df ) )
# 
#       lo.man = bhat.pool.man - qt(.975, df = df) * se.pool.man
#       hi.man = bhat.pool.man + qt(.975, df = df) * se.pool.man
#     }
# 
#     if ( link == "poisson" | link == "logistic" ) {
#       z = abs( bhat.pool.man / se.pool.man )
#       pval.man = 2 * ( 1 - pnorm( z ) )
# 
#       lo.man = bhat.pool.man - qnorm(.975) * se.pool.man
#       hi.man = bhat.pool.man + qnorm(.975) * se.pool.man
#     }
# 
#     # check bhats
#     require(testthat)
#     expect_equal( bhats.pool[i], bhat.pool.man, tol = 0.001 )
#     expect_equal( ses.pool[i], se.pool.man, tol = 0.001 )
#     expect_equal( lo.pool[i], lo.man, tol = 0.001 )
#     expect_equal( hi.pool[i], hi.man, tol = 0.001 )
#     expect_equal( pvals.pool[i], pval.man, tol = 0.001 )
# 
#   }
# }




########################### PRETTIFY OUTPUT ########################### 

digits = 2

ses = ses.pool


if ( link == "OLS" ) {
  
  est = round(bhats.pool, digits)
  
  est.unrounded = bhats.pool
  
  ci = paste( "[",
              my_round( lo.pool, digits ),
              ", ",
              my_round( hi.pool, digits ),
              "]",
              sep = ""
  )
} 


if ( link != "OLS" ) {
  
  est = round( exp(bhats.pool), digits)
  
  est.unrounded = bhats.pool
  
  ci = paste( "[",
              my_round( exp(lo.pool), digits),
              ", ",
              my_round( exp(hi.pool), digits),
              "]",
              sep = ""
  )
}


# set p-value star cutoffs; last is Bonferroni
stars = c(0.01, 0.05, 0.05 / length( c(Ylin, Ybin, Ycount) ) )


if ( missingness == "MI" ) {
  pvals = vapply( pvals.pool,
                  function(x) format_pval( x, digits = 3, star.cutoffs = stars ),
                  "asdf" )
} else {
  pvals = vapply( res$samp.res$pvals,
                  function(x) format_pval( x, digits = 3, star.cutoffs = stars ),
                  "asdf" )
}



# NOTE THAT LAST 4 COLUMNS ARE OUTCOME-WIDE, SO THOSE VALUES ARE REPEATED FOR
#  ALL VARIABLES IN THAT GROUP

( table2 = data.frame( Outcome = Ynames, 
                       
                       Est = est,
                       
                       Est.Unrounded = est.unrounded,
                       
                       SE = my_round( ses, digits ),
                       
                       SE.Unrounded = ses, 
                       
                       CI = ci,
                       
                       pval = pvals,
                       
                       Analyzed.n = analyzed.n,
                       
                       Reject.Romano = rom.rej,
                       
                       # ours
                       Theta.hat = theta.hat,
                       
                       Global.pval = jt.pval, 
                       
                       Null.int = paste( " [",
                                         round(ni.lo, digits),
                                         ", ",
                                         round(ni.hi, digits),
                                         "]",
                                         sep = ""
                       ),
                       
                       
                       Excess.hits = theta.hat - ni.hi
) )

# error about short row names is expected

# if doing CC, add a column saying the sample size
#browser()
#if ( missingness == "CC" ) table2$n = length(res$samp.res$resid)



digits2 = 2
( table3 = data.frame( Outcome = Ynames, 
                       
                       Evalue.point = round( evals.pt, digits2 ), 
                       
                       Evalue.CI = round( evals.CI, digits2 )
) )



###### Merge in table-friendly variable names #####

setwd(codebook.dir)
require(readxl)
cd = read_xlsx("Analysis dataset codebook.xlsx")

require(plyr)
table2$Outcome = suppressMessages( mapvalues( table2$Outcome, from = cd$Variable, to = cd$`Long name`) )
table3$Outcome = suppressMessages( mapvalues( table3$Outcome, from = cd$Variable, to = cd$`Long name`) )
# warnings are expected because codebook contains extra variables


###### Write Results #####

if ( write.results == TRUE ) {
  setwd(results.dir)
  
  flavor = ifelse( TMLE == TRUE, "tmle", "nontmle")
  
  string = paste( "table2",
                  link,
                  flavor,
                  missingness,
                  ".csv",
                  sep = "_" )
  
  write.csv(table2, string)
  
  
  string = paste( "table3",
                  link,
                  flavor,
                  missingness,
                  ".csv",
                  sep = "_" )
  
  write.csv(table3, string)
}



########################### OTHER STATS MENTIONED IN PROSE ########################### 


# # median correlation magnitude
# Ys = d[ , c(Ylin, Ycount) ]
# 
# # binaries need to be 0/1 for this
# binaries = Ycount[ !Ycount == "flourish_d" ]
# Ys[ , binaries ] = apply(Ys[ , binaries ], 2, function(x) binarize(x) )
# 
# # use only complete cases
# Ys = Ys[ complete.cases(Ys), ]
# 
# # correlations without the the diagonal elements
# corrs = as.numeric( abs( cor( Ys ) ) )
# corrs = corrs[ !corrs == 1 ]
# summary(corrs)



# ########################### CC ANALYSIS SANITY CHECK ###########################

# if user chose MI, run CC analysis with exactly the same model specifications 
# for comparison

if ( missingness == "MI") {
  # names of outcomes with this link
  if ( link == "OLS" ) Ynames = Ylin
  if ( link == "poisson" ) Ynames = Ycount
  if ( link == "logistic" ) Ynames = Ybin
  
  # for Ying's project only: adjust Cnames because we omitted the highly missing
  #  abuse_c covariate from CC analyses only
  CCres = suppressWarnings( analyze_CC_dataset( d = d,  # for sanity check on complete data
                                                X = Xname,
                                                C = Cnames.CC,
                                                Ys = Ynames,
                                                alpha = 0.05,
                                                resample = FALSE,
                                                B = B.resamp,
                                                model = link,
                                                TMLE = FALSE ) )
  
  ( cc.res = suppressMessages( data.frame( outcome = mapvalues( Ynames, cd$Variable, cd$`Long name`),
                                           est = CCres$samp.res$bhats,
                                           pval = CCres$samp.res$pvals) ) )
  
  # write results
  string = paste( "table2",
                  link,
                  "nontmle",
                  "CCsanity",
                  ".csv",
                  sep = "_" )
  
  setwd(results.dir)
  write.csv(cc.res, string)
  
}



