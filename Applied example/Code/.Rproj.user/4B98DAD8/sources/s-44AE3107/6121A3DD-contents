
########################### FN: RECODE VARIABLES DURING DATA PREP ###########################

# recodes binary variables per MIDUS' coding scheme
recode_binary = Vectorize( function( var.name ) {
  
  # warn if variable has any values besides the expected 1,2
  if( any( !d2[[var.name]] %in% c(1,2) ) ) warning( paste( var.name, " had weird values!", sep="" ) )
  
  d2[[var.name]] <<- recode( d2[[var.name]], "1=1; 2=0" )
  print( table(d[[var.name]], d2[[var.name]]) )
}, vectorize.args = "var.name" )


########################### FN: FIT ONE OUTCOME MODEL (OLS) ###########################

fit_model = function( Y.name,
                      X.name = "A1SEPA_z",
                      .d,
                      .covars = covars,
                      .center.stats = TRUE,
                      .bhat.orig = stats$b ) {

  # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
  m = lm( .d[[Y.name]] ~ ., data=.d[,.covars] )

  m.stats = summary(m)$coefficients[ X.name, ]
  CI = confint(m)[X.name,]

  # if resampling was done under Ha, center stats by the original-sample estimates
  if( !.center.stats ) {
    b = m.stats[["Estimate"]]
    tval = m.stats[["t value"]]
    SE = m.stats[["Std. Error"]]
  }
  if( .center.stats ) {
    # second term pulls out the correct beta-hat from the original vector by using the Yname that we're regressing on
    outcome.num = which( outcomes == Y.name )
    b = m.stats[["Estimate"]] - .bhat.orig[ outcome.num ]
    SE = m.stats[["Std. Error"]]
    tval = b / SE
  }

  df = nrow(.d) - length(covars) - 1
  pval = 2 * ( 1 - pt( abs( b / SE ), df ) )

  stats = data.frame( outcome = Y.name,
                      b = b,
                      SE = SE,
                      df = df,
                      tval = tval,
                      pval = pval,
                      reject.0.05 = pval < 0.05,
                      reject.0.01 = pval < 0.01,
                      lb = CI[1],
                      ub = CI[2] )

  return( list( stats = stats,
                resids = residuals(m) ) )
}

# # test for 1 outcome
# raw.res = fit_model(  Y.name = outcomes[1],
#             X.name = "A1SEPA_z",
#             .d = d,
#             .covars = covars,
#             .center.stats = FALSE )



########################### FN: GIVEN DATASET, RETURN STATS ###########################

dataset_result = function( .d,
                           .alpha = 0.05,
                           .center.stats = TRUE,
                           .bhat.orig = NA ) {  # used for centering test stats

  # # TESTING ONLY
  # .d = d
  # .alpha = 0.05
  # .center.stats = TRUE
  # .bhat.orig = stats$b

  # set names USING GLOBAL VARS
  X.names = covars
  Y.names = outcomes
  X.name = "A1SEPA_z"  # exposure of interest

  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest

  # this is a list of lists:
  #  length is equal to number of outcomes
  #  each entry is another list
  # with elements "pval" (scalar) and "resid" (length matches number of subjects)
  lists = lapply( X = Y.names,
                  FUN = function(y) fit_model(  Y.name = y,
                                                X.name = "A1SEPA_z",
                                                .d = .d,
                                                .covars = covars,
                                                .center.stats = .center.stats,
                                                .bhat.orig = stats$b ) )

  # "flatten" the list of lists
  u = unlist(lists)
  pvals = as.vector( u[ names(u) == "stats.pval" ] )

  tvals = as.vector( u[ names(u) == "stats.tval" ] )
  bhats = as.vector( u[ names(u) == "stats.b" ] )
  pvals = as.vector( u[ names(u) == "stats.pval" ] )

  # returns vector for number of rejections at each alpha level
  # length should match length of .alpha
  n.reject = vapply( X = .alpha, FUN = function(a) sum( pvals < a ), FUN.VALUE=-99 )

  return( list( rej = n.reject,
                tvals = tvals,
                bhats = bhats,
                pvals = pvals ) )
}
