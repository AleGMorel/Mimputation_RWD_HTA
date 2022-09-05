###### Run CCA over datasets with 10% of missing data######

library(writexl)  # required to save data in .xls
library(readxl)   #required to read the imported excel ds
library(parallel) # required for the mclapply function
library(descr)    # required to describe data
library(mice)     # required to impute data
library(systemfit)  # required to run seemingly unrelated regression
library(tidyverse)  # required for data manipulation

## Import datasets
dataset <- paste0("C:/Users/Angela/Documents/2022-Ale-projects/project-1/data/HY_HC/MISSING10/M10_HL",1:10,".xlsx")
data <- mclapply(dataset, read_excel)

## Complete Case analysis

CCA <- function(dataset){
  
  #1 Delete unnecessary variables
  dataset$diff_Y <- NULL
  dataset$diff_cost <- NULL
  dataset$M <- NULL
  dataset$missing_cost <- NULL
  dataset$missing_Y <- NULL
  dataset$Y <- NULL
  dataset$cost <- NULL
  
  #2 Deleting missing cases of costs and effects
  dataset <- na.omit(dataset)
  
  #3 Fit seemingly unrelated regressions model
  r1 <- cost_mw ~ treatment + rom + depression
  r2 <- Y_mw ~ treatment + rom + depression
  fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=dataset)
  
  #4 Extract betas and calculate ICER
  dataset$cost_diff <- fitsur[["coefficients"]][["costreg_treatment"]]
  dataset$effect_diff <- fitsur[["coefficients"]][["effectreg_treatment"]]
  dataset$ICER <- dataset$cost_diff/dataset$effect_diff
  
  #4 Extract variance to estimate standard errors
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
  dataset$method <- "CCA"
  
  #7 Drop unnecessary variables that are not results
  dataset <- dataset[,-(1:8)]
  dataset <- dataset[1,]
  
  dataset <- as.data.frame(dataset)
  
}

cca_time <- Sys.time() #initial time

cca <- lapply(data,CCA)

Sys.time() - cca_time # total time

cca.bind <-bind_rows(cca, .id = "N") # append results from all simulated datasets

write_xlsx(cca.bind,"C:/Users/Angela/Documents/2022-Ale-projects/project-1/output/HY_HC_M10_CCA_results.xlsx")



