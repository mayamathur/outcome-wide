

########################### SIMULATE FAKE DATA ########################### 

# location to save fake data
# location of root directory where all the below directories are found
root.dir = "~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Linked to OSF (OWP)/General code for outcome-wide analyses"
setwd(root.dir)

# location of data
data.dir = paste(root.dir, "/Fake data", sep="")


# variable names
Ylin = c( "Ylin1",
          "Ylin2",
          "Ylin3",
          "Ylin4" )

Ybin = c( "Ybin1",
          "Ybin2",
          "Ybin3",
          "Ybin4",
          "Ybin5" )

Cnames = c( "C1",
            "C2" )

Xname = "X"

covars = c(Cnames, Xname)


library(NRejections)

# generate everything as continuous and will later dichotomize the outcomes
cor = make_corr_mat( nX = length(covars),
                     nY = length(c(Ylin, Ybin) ),
                     rho.XX = 0.05,
                     rho.YY = 0.1,
                     rho.XY = 0.2,
                     prop.corr = 10/15 )
n=1000
d = sim_data( n = n, cor = cor )


names(d) = c( Xname, Cnames, Ylin, Ybin )

# binarize the outcomes and X
# don't worry about doing this with the covariates because it won't affect the 
#  models being fit

median_split = function(x) {
  x2 = rep( 0, length(x) )
  x2[ x > median(x) ] = 1
  return(x2)
}

nonmedian_split = function(x) {
  x2 = rep( 0, length(x) )
  x2[ x > quantile(x, .95) ] = 1
  return(x2)
}

# binarize all but one of the binary outcomes to make them common
d[ , names(d) %in% c(Xname, Ybin[ 1 : (length(Ybin) - 1) ] ) ] = apply( d[ , names(d) %in% c(Xname,  Ybin[ 1 : (length(Ybin) - 1) ] ) ], 2, median_split )

# binarize the last one to make it rare
d[ , Ybin[ length(Ybin) ] ] = nonmedian_split( d[ , Ybin[ length(Ybin) ] ] )

# induce MCAR missingness to all observations
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


d = degradefunction( d, 0.005 * nrow(d) * ncol(d) )
# check proportion missingness
table(is.na(d))

# look at fake data
library(tableone)
CreateTableOne(data=d)

# save it
setwd(data.dir)
write.csv(d, "fake_data.csv", row.names = FALSE)


