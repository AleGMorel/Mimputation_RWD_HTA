###### Run SUR over complete datasets ######

library(writexl)  # required to save data in .xls
library(readxl)   #required to read the imported excel ds
library(parallel) # required for the mclapply function
library(descr)    # required to describe data
library(mice)     # required to impute data
library(systemfit)  # required to run seemingly unrelated regression
library(tidyverse)  # required for data manipulation

## Import complete datasets
dataset <- paste0("C:/Users/Angela/Documents/2022-Ale-projects/project-1/data/HY_HC/HL",1:10,".xlsx")
data <- mclapply(dataset, read_excel)

## Seemingly unrelated regressions model fit in complete datasets to get true values

SUR <- function(dataset){
  
  #1 Delete unnecessary variables
  dataset$diff_Y <- NULL
  dataset$diff_cost <- NULL
  
  #2 Fit seemingly unrelated regressions model
  r1 <- cost ~ treatment + rom + depression
  r2 <- Y ~ treatment + rom + depression
  fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=dataset)
  
  #4 Extract betas and calculate ICER
  dataset$cost_diff <- fitsur[["coefficients"]][["costreg_treatment"]]
  dataset$effect_diff <- fitsur[["coefficients"]][["effectreg_treatment"]]
  dataset$ICER <- dataset$cost_diff/dataset$effect_diff
  
  #4 Extract variance
  varcov <- fitsur[["coefCov"]]
  var_cost <- fitsur[["coefCov"]][[2,2]]
  var_effect <- fitsur[["coefCov"]][[6,6]]
  cov <- fitsur[["coefCov"]][[2,6]]
  
  #5 Estimate lower and upper confidence interval limits for costs and effects 
  Za = 1.95996
  dataset$LL_cost <- dataset$cost_diff - (Za*sqrt(var_cost)) # lower-limit of the 95% CI for costs
  dataset$UL_cost <- dataset$cost_diff + (Za*sqrt(var_cost)) # upper-limit of the 95% CI for costs
  dataset$LL_effect <- dataset$effect_diff - (Za*sqrt(var_effect)) # lower-limit of the 95% CI for QALY
  dataset$UL_effect <- dataset$effect_diff + (Za*sqrt(var_effect)) # upper-limit of the 95% CI for QALY
  dataset$se_cost <- sqrt(var_cost)
  dataset$se_effect <- sqrt(var_effect)
  
  #6 Add the method used as a variable to the dataset
  dataset$method <- 'TRUE_VALUES'
  
  #7 Drop unnecessary variables that are not results
  dataset <- dataset[,-(1:8)]
  dataset <- dataset[1,]
  
  dataset <- as.data.frame(dataset)
  
}

sur_time <- Sys.time() #initial time

sur <- lapply(data,SUR)

Sys.time() - sur_time # total time

sur.bind <-bind_rows(sur, .id = "N") # append results from all simulated datasets

write_xlsx(sur.bind,"C:/Users/Angela/Documents/2022-Ale-projects/project-1/output/HY_HC_M10_TV_results.xlsx")

