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
  
  #4 Extract variance
  varcov <- fitsur[["coefCov"]]
  var_cost <- fitsur[["coefCov"]][[2,2]]
  var_effect <- fitsur[["coefCov"]][[6,6]]
  cov <- fitsur[["coefCov"]][[2,6]]
  
  #5 Estimate lower and upper confidence interval limits for costs and effects 
  Za = 1.95996
  dataset$LL_cost_pooled <- dataset$cost_diff - (Za*sqrt(var_cost)) # lower-limit of the 95% CI for costs
  dataset$UL_cost_pooled <- dataset$cost_diff + (Za*sqrt(var_cost)) # upper-limit of the 95% CI for costs
  dataset$LL_effect_pooled <- dataset$effect_diff - (Za*sqrt(var_effect)) # lower-limit of the 95% CI for QALY
  dataset$UL_effect_pooled <- dataset$effect_diff + (Za*sqrt(var_effect)) # upper-limit of the 95% CI for QALY
  
  dataset <- as.data.frame(dataset)
  
}

cca_time <- Sys.time() #initial time

cca <- lapply(data,CCA)

for (i in 1:10) {
  
  write_xlsx(cca[[i]], paste0("C:/Users/Angela/Documents/2022-Ale-projects/project-1/data/HY_HC/MISSING10/CCA/CCA_M10_HL",i,".xlsx"))
}

Sys.time() - cca_time # total time
