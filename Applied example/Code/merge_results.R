

########################### COMPARE TMLE VS. MLE FOR ALL COEFFICIENTS (MARRIAGE & DIVORCE) ###########################

results.dir = "~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Linked to OSF (OWP)/Applied example/Results/Supplement (top vs. bottom tertile)"
setwd(results.dir)

# digits for rounding
digits = 2

# cbind the OLS and logistic ones
tmle = rbind( read.csv("table2_OLS_tmle_MI_.csv"),
              read.csv("table2_logistic_tmle_MI_.csv") )
non = rbind( read.csv("table2_OLS_nontmle_MI_.csv"),
             read.csv("table2_logistic_nontmle_MI_.csv") )

##### Point Estimate #####
same.sign = sign(tmle$Est.Unrounded) == sign(non$Est.Unrounded)
perc.b.same.sign = my_round( 100 * sum( same.sign ) / length( tmle$Est.Unrounded ), digits = 2 )

# percent of times that TMLE estimate is larger
#  of the times that it's in same direction
b.ratio = abs( tmle$Est.Unrounded[same.sign == TRUE] ) / abs( non$Est.Unrounded[same.sign == TRUE] )
mean.bratio = my_round( mean(b.ratio), digits = 2 )
perc.b.stronger = my_round( 100 * sum( b.ratio > 1 ) / length( b.ratio ), digits = 2 )

# mean b ratio when normalized to be always > 1
b.ratio2 = b.ratio
b.ratio2[ b.ratio < 1 ] = 1 / b.ratio2[ b.ratio < 1 ]
mean.bratio.sign.normalized = my_round( mean(b.ratio2), digits = 2 )

# mean ratio when TMLE is larger
b.ratio.when.larger = my_round( mean(b.ratio[ b.ratio > 1 ] ), digits = 2 )

# mean ratio when TMLE is smaller
b.ratio.when.smaller = my_round( mean(b.ratio[ b.ratio < 1 ] ), digits = 2 )


##### Standard Errors #####
# SE: percent of times that TMLE SE is smaller
( se.ratio = tmle$SE / non$SE )
( perc.se.smaller = my_round( 100 * sum( se.ratio < 1 ) / length( se.ratio ), digits = 2 ) )

# SE: percent smaller when it is smaller
( smaller = se.ratio < 1 )
( se.ratio.when.smaller = my_round( mean( se.ratio[smaller] ), digits = 2 ) )

# SE: percent of times that TMLE SE is larger
( larger = tmle$SE > non$SE )
( se.ratio.when.larger = my_round( mean( se.ratio[larger] ), digits = 2 ) )



( comparison = data.frame( short.name = c("perc.b.same.sign",
                                        "perc.b.stronger",
                                        "mean.bratio",
                                        "mean.bratio.sign.normalized",
                                        "b.ratio.when.larger",
                                        "b.ratio.when.smaller",
                                        "perc.se.smaller",
                                        "se.ratio.when.smaller",
                                        "se.ratio.when.larger"),
                         stat = c(perc.b.same.sign,
                                  perc.b.stronger,
                                  mean.bratio,
                                  mean.bratio.sign.normalized,
                                  b.ratio.when.larger,
                                  b.ratio.when.smaller,
                                  perc.se.smaller,
                                  se.ratio.when.smaller,
                                  se.ratio.when.larger),
                         description = c("Percent of coefficients for which TMLE and non-TMLE estimate had same sign",
                                         "Percent of coefficients for which TMLE estimate was of larger magnitude than non-TMLE",
                                         "Of coefficients that agreed in sign, mean ratio of TMLE to non-TMLE estimate",
                                         "Of coefficients that agreed in sign, mean ratio of TMLE to non-TMLE estimate after taking inverses to have each ratio > 1",
                                         "Of coefficients that agreed in sign and had ratio > 1, mean ratio of TMLE to non-TMLE estimate",
                                         "Of coefficients that agreed in sign and had ratio < 1, mean ratio of TMLE to non-TMLE estimate",
                                         "Percent of SE estimates for which TMLE estimate was smaller than non-TMLE",
                                         "Of SE estimates for which TMLE was smaller, mean ratio of TMLE to non-TMLE estimate",
                                         "Of SE estimates for which TMLE was larger, mean ratio of TMLE to non-TMLE estimate")) )

# write results
write.csv( comparison, "tmle_mle_comparison.csv", row.names = FALSE )



##### Comparison Plot #####

# make plotting dataframe
names(tmle) = paste("tmle.", names(tmle), sep="" )
names(non) = paste("non.", names(non), sep="" )
agg = cbind(tmle, non)

agg$Model = "Linear"
agg$Model[ (length(Ylin) + 1) : nrow(agg) ] = "Logistic"
  


colors = c("black", "orange")

library(ggplot2)

##### Comparison Scatterplot of Point Estimates #####
ggplot( data = agg, aes( x = non.Est, y = tmle.Est, color = Model ) ) +
  
  xlab("Non-TMLE coefficient estimate") +
  ylab("TMLE coefficient estimate") +
  geom_abline(slope = 1, intercept = 0, color="gray") +
  geom_point(size = 3, alpha = 0.6) +
  
  theme_bw() +
  
  scale_color_manual(values = colors)

ggsave("tmle_coefficient_comparison_plot.png",
       width = 8,
       height = 8,
       units = "in")


##### Comparison Plot of SEs #####
ggplot( data = agg, aes( x = non.SE, y = tmle.SE, color = Model ) ) +
  
  xlab("Non-TMLE standard error estimate") +
  ylab("TMLE standard error estimate") +
  
  geom_abline(slope = 1, intercept = 0, color="gray") +
  geom_point(size = 3, alpha = 0.6) +
  
  theme_bw() +
  
  scale_color_manual(values = colors)

ggsave("tmle_ses_comparison_plot.png",
       width = 8,
       height = 8,
       units = "in")



########################### MERGE RESULTS INTO SHARED EXCEL FILES ###########################

library(openxlsx)

setwd(results.dir)

all.link.fns = c("OLS", "logistic", "poisson")


##### Write Table 2 Results to Different Sheets of Same File #####
for ( link in all.link.fns ) {
  
  files = list.files()
  files2 = files[ grepl( link, files ) &
                    grepl( "table2", files ) &
                    grepl( "csv", files ) ]
  files2 = files2[ !grepl("sanity", files2) ]
  
  # this happens if we didn't run that link fn at all
  if ( length(files2) == 0 ) break
  
  wb = createWorkbook()
  
  # write each Table 2 results file to a different sheet of same Excel 
  #  workbook
  for ( i in 1:length(files2) ) {
    d = read.csv( files2[i] )
    sheet.name = gsub( "table2_", "", files2[i] )
    sheet.name = gsub( ".csv", "", sheet.name )
    
    addWorksheet(wb, sheet.name)
    writeData(wb, sheet.name, d)
  }
  
  string = paste( "merged_table2_", link, ".xlsx", sep="")
  saveWorkbook(wb, string, overwrite = TRUE)
}


##### Write Table 3 Results to Different Sheets of Same File #####
for ( link in all.link.fns ) {
  
  files = list.files()
  files2 = files[ grepl( link, files ) &
                    grepl( "table3", files ) &
                    grepl( "csv", files ) ]
  files2 = files2[ !grepl("sanity", files2) ]
  
  # this happens if we didn't run that link fn at all
  if ( length(files2) == 0 ) break
  
  wb = createWorkbook()
  
  # write each Table 3 results file to a different sheet of same Excel 
  #  workbook
  for ( i in 1:length(files2) ) {
    d = read.csv( files2[i] )
    sheet.name = gsub( "table3_", "", files2[i] )
    sheet.name = gsub( ".csv", "", sheet.name )
    
    addWorksheet(wb, sheet.name)
    writeData(wb, sheet.name, d)
  }
  
  string = paste( "merged_table3_", link, ".xlsx", sep="")
  saveWorkbook(wb, string, overwrite = TRUE)
}




########################### FOREST PLOT ###########################


# codebook indicates which effect direction is "good" for each variable
setwd(codebook.dir)
require(readxl)
cd = read_xlsx("Analysis dataset codebook.xlsx")

# read in TMLE-MI results (the primary ones)
setwd(results.dir)

d.ols = read.csv("table2_OLS_tmle_MI_.csv", stringsAsFactors = FALSE )
d.logistic = read.csv("table2_logistic_tmle_MI_.csv", stringsAsFactors = FALSE)

d = rbind( d.ols, d.logistic )

d$type = c( rep( "OLS", nrow(d.ols) ), rep( "logistic", nrow(d.logistic) ) )

# put all effect sizes on (approximate) SMD scale
d$yi = d$Est.Unrounded
d$vi = d$SE.Unrounded^2

# synchronize directions
library(plyr)
d$good.direction = mapvalues( d$Outcome, from = cd$`Long name`, to = cd$`Good direction`)
d$yi = d$yi * as.numeric( d$good.direction )

# proportion of effects in predicted direction: 77%
sum( d$yi > 0 ) / length(d$yi)

# convert SMDs for continuous variables to approximate log-OR 
# other direction is more dangerous because only works well for common outcomes
d$yi[ d$type == "OLS" ] = ( d$yi[ d$type == "OLS" ] * pi ) / sqrt(3)
d$vi[ d$type == "OLS" ] = ( d$vi[ d$type == "OLS" ] * pi^2 ) / 3


# merge in E-values
evals1 = read.csv("table3_OLS_tmle_MI_.csv", stringsAsFactors = FALSE )
evals2 = read.csv("table3_logistic_tmle_MI_.csv", stringsAsFactors = FALSE)
evals = rbind( evals1, evals2 )

d = merge( d, evals, by.x = "Outcome", by.y = "Outcome" )



# sort by size
d = d[ order(d$yi, decreasing = FALSE), ]
correct.order = d$Outcome
d$Outcome = factor(d$Outcome, levels = correct.order)
levels(d$Outcome)

xlab = "Estimated OR and E-values (RR)"

# did the p-value survive Bonferroni correction?
d$pass.bonferroni = grepl("\\*\\*\\*", d$pval)

my.colors = c("black", "orange")

library(ggplot2)
ggplot( data = d, aes( x = exp(yi), y = Outcome, color = pass.bonferroni ) ) +
  theme_classic() +
  geom_vline(xintercept = 1, lty = 2, color = "red") +
  geom_point( size = 3) +
  geom_errorbarh( aes( xmin = exp( yi - qnorm(.975) * sqrt(vi) ),
                       xmax = exp( yi + qnorm(.975) * sqrt(vi) ) ),
                  height = 0) +
  
  # E-values
  geom_point( aes( x = Evalue.point, y = Outcome, shape = "shape1" ), size = 2 ) +
  geom_point( aes( x = Evalue.CI, y = Outcome, shape = "shape2" ), size = 2 ) +
  
  #scale_x_continuous( breaks = seq(.9, 1.8, .05)) +
  
  scale_shape_manual( values = c("shape1" = 4,
                                 "shape2" = 8),
                      name = "E-values",
                      guide = "legend",
                      label = c("For point estimate",
                                "For confidence interval") ) +
  
  scale_color_manual(values = my.colors,
                     label = c("Not rejected", "Rejected"),
                     name = "Bonferroni correction") +
  xlab(xlab)




setwd(results.dir)
ggsave("forest_plot.png",
       width = 10,
       height = 8,
       units = "in")




