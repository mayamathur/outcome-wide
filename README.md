
# Overview

This repository contains documented code for conducting an outcome-wide analysis, involving fitting models with different link functions, with either standard MLE or targeted maximum likelihood estimation (TMLE), and with either multiple imputation or complete-case anlaysis.

The code in the "General code" directory is intended to generalize fairly easily to run similar outcome-wide analyses on other datasets, and it includes a reproducible example with a simulated dataset. To run a new outcome-wide analysis, you will primarily modify one of the R files, `master.R`, in addition to providing your own dataset and codebook. We describe this process below.

Please address questions to Maya Mathur at: mmathur [AT] stanford [DOT] edu.

# Citing this software

If you use this code, we would appreciate if you would cite the paper in which we proposed outcome-wide analyses and those in which we developed the methodology and software for the E-value:

1. *VanderWeele TJ, Mathur MB, & Chen Y. Outcome-wide longitudinal designs for causal inference: A new template for empirical studies. Under review.*

2. *VanderWeele TJ & Ding P. (2018). Web site and R package for computing E-values. Annals of Internal Medicine, 167(4), 268-274.*

3. *Mathur MB., Ding P, Riddell CA, & VanderWeele TJ. (2018). Web site and R package for computing E-values. Epidemiology, 29(5), e45-e47.*



# Important caveats

**This code is not designed to be completely point-and-shoot, like an R package**. Too many of the analysis decisions are dataset-dependent for a point-and-shoot approach to be practical or statistically advisable. We note below (and in comments throughout the code) the steps that will particularly require human attention for each new outcome-wide analysis. Also note that **we have not thoroughly tested the code for the Poisson link in a real dataset**, so you should use extra care if using this link function. For example, `merge_results.R` does not look for or merge results for the Poisson link. 


# How to use this code

## Prior to analysis

Prior to running the main analysis script, `master.R`, **you will need to make a codebook called `Analysis dataset codebook.xlsx`** that will be used to make the results-tables human-readable and to calculate E-values properly. The codebook should contain the following columns:

- `Variable` lists the variable names exactly as they appear in the dataset.
- `Long name` lists the name of each variable as you would like them to appear in the results tables.
- `Table order` lists the order in which you would like each variable to appear in the results tables.
- `Good direction` indicates whether larger values of the outcome are considered "good" (1) or bad (-1). This variable is used when creating the forest plot with synchronized effect directions.


## Running the main analysis script

The script `master.R` conducts the analyses as follows:

1. Reads in the raw dataset and does some basic descriptive analyses, such as assessing the proportion of missing data in each variable and checking for collinearity that may cause problems for multiple imputation.

2. Allows you to provide lists of variable names (outcomes that will be modeled with each link function, adjusted covariates, the exposure of interest, etc.)

3. Allows you to specify directory names for the locations of the raw data, the future results, etc.

4. Makes imputed datasets.

5. Does some dataset-specific recoding of variables and recodes binary variables to please the `tmle` package (which expects binary variables to be coded 0/1 and throws errors about "missing data" when this is not fulfilled).

To accomplish this, `master.R` will call the helper files `make_imputations.R` and `helper_applied_example.R`. **You will need to run line-by-line and potentially modify the ~210 lines of code in the first section ("Prep Work") of `master.R`** because the needed preparatory work will inherently be somewhat dataset-specific. If you have multiple exposure variables, each of which is being subjected to its own outcome-wide analysis, you can make a separate `master.R` file for each.

You should also **pay attention during the multiple imputation process**, which `master.R` will automatically initiate by calling `make_imputations.R`. The latter script does its best to check for various concerning situations during imputation, generating error messages when these occur, in which case you should manually diagnose the problem and modify `make_imputations.R` directly. Nevertheless, even if no errors arise, you should carefully read through the contents of `make_imputations.R`, ideally even running it manually line-by-line to assess whether the modeling decisions we made for our dataset are also reasonable for yours. The imputed datasets are saved as csv files for reproducibility and so that they can be read back in for each analysis run, described next. 

After having prepped the dataset and made imputations, the remainder of `master.R` sets up a number of different analysis "runs". Each run constitutes a call to the script `analyses_applied_example.R`, informed by your choices of the following parameters:

- `link` (of `OLS`, `logistic`, or `poisson`): What type of model to fit to each outcome. Each choice will fit models to a different subset of the outcomes by name (specified earlier in `master.R` through the lists of variable names `Ylin`, `Ybin`, and `Ycount` respectively). 

- `missingness` (of `CC` or `MI`): Should missing data be handled through complete-case analysis or multiple imputation?

- `impute.from.scratch`: If imputing, should it be done from scratch, or should existing saved imputations be used instead?

- `TMLE` (of `FALSE`, `TRUE`): Should the model be fit by TMLE or by standard MLE?

- `write.results`: Should the new analysis results overwrite any previously saved ones?

- `resample`: Should residual resampling be conducted in order to estimate outcome-wide metrics of evidence strength via the number of rejections? (This only works for `link = "OLS"`).

- `resample.from.scratch`: If resampling, should it be done from scratch, or should existing saved resamples be used instead?

- `B.resamp`: The number of resamples to be run. Choices less than 1000 will generate warnings. 

- `alpha`: Familywise alpha level for multiple testing corrections

- `alpha.within`: Alpha level for each individual hypothesis test

The file `analyses_applied_example.R` can in general be left unmodified. However, **do pay attention to the parameter `n.tot.outcomes`,** which indicates the number of tests to penalize with the Bonferroni correction. This decision should be made on a case-by-case basis on scientific grounds. 

Each analysis run will save point estimates, inference, etc. in a `table2` csv file titled with the link function, missingness method, and TMLE/non-TMLE choice, as well as a `table3` file containing E-values for the former results. To make these tables human-readable, the code will use the codebook to relabel and reorder the variables.

## Analysis post-processing

Once all the analysis runs are complete, `master.R` calls another helper file, `merge_results.R` (in which you will need to set a working directory path at the beginning). **`merge_results.R` expects that you have run both OLS and logistic models with both TMLE and non-TMLE.** If this is not the case, you will need to make simple modifications to the script so that it does not run into errors when looking for files that don't exist. This script does the following:

1. Stitches all the `table2` results into a single Excel file with multiple tabs and does the same to all the `table3` E-values.

2. Writes a file called `tmle_mle_comparison.csv`, containing various comparisons between the TMLE and non-TMLE results, which are explained within the file itself. (Note that in the simulated dataset, the two sets of SEs were always the same, which is why there are NAs in the comparison file.)

3. Makes plots comparing the TMLE vs. non-TMLE point estimates and SE estimates.

4. Makes a forest plot of all the TMLE point estimates, their CIs, and their E-values after synchronizing their effect directions per the codebook and approximately converting all point estimates to the odds ratio scale, as in a meta-analysis.


## Sanity checks conducted during analysis

The code conducts some of its own sanity checks:

- It saves a quick-and-dirty `table1` summarizing all the analysis variables in the dataset. Make sure these results corroborate your expectations.

- For non-TMLE estimation, it refits each outcome model "manually" (i.e., by simply looping over the outcomes without calling the generalized functions or doing complicated formatting) and prints "Manually checking outcome number XXX" to the console when doing so. Any discrepancies (which we expect should never occur) will generate errors.

- For each MI model, the code will write an additional csv file with the suffix `CCsanity`. This file shows results from fitting the same model specification as used in the MI analysis, but using a complete-case approach. This allows you easily assess the extent to which the MI and CC results diverge. If you actually plan to report CC results in your manuscript, you will probably prefer to fit the CC model directly by setting up a separate analysis run with `missingness = "CC"`, which allows more flexibility (e.g., you can choose to have different covariates in the CC vs. MI models) and exports more extensive results in a more attractive format. If you do so, the resulting files will have the suffix `CC` rather than `CCsanity`. 



# Technical points

- By default, the code conducts multiple imputation by chained equations, using predictive mean matching for continuous variables and logistic or polytomous regression models for binary and categorical variables, respectively. For each imputation model, we pruned the variables included in the imputation model using the methods described by van Buuren et al. (1999). Inference for regression point estimates is based on Rubin's Rules (Little et. al, 2014).

- The `tmle` package does not support estimating risk ratios for the Poisson link, but rather only the average treatment effect. Therefore, as we did, you may want to choose the logistic link even for common binary outcomes. 

- When computing E-values for continuous outcomes, the code assumes that the contrast of interest is a 1-unit change in X, which is of course the only contrast when X is binary. However, **if X is continuous, you should consider modifying the calls to `evalues.MD` to use a different contrast of scientific interest.** For OLS models fit with standard MLE, the code approximates a standardized mean difference using the model-based residual standard deviation estimate. For OLS models fit with TMLE, there is not a direct analog, so the code instead uses the marginal standard deviation of Y, resulting in a potentially conservative effect size. 

- When computing E-values for logistic regression coefficients, the code will use a file written during the Prep Work section of `master.R` called `list_of_rare_binaries.csv` to use the square-root transform for odds ratios on outcomes with prevalence > 10% in the sample. The code prints messages to inform the user each time this transformation is used.

- If the E-values in the forest plot are non-monotonic with respect to the point estimates, do not panic. This is because of the conversions used to put all the point estimates on the OR scale (e.g., common (prevalence >10% in the sample) binary outcomes modeled with logistic regression use the square-root transform.)




