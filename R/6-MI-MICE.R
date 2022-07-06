###### Run MI-MICE over datasets with 10% of missing data######

library(writexl)  # required to save data in .xls
library(readxl)   #required to read the imported excel ds
library(parallel) # required for the mclapply function
library(descr)    # required to describe data
library(mice)     # required to impute data

## Import datasets
dataset <- paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/M10_HL",1:10,".xlsx")
data <- mclapply(dataset, read_excel)

## Multiple imputation: MICE procedure

MI.MICE <- function(dataset){
  
  #1 delete unnecessary variables
  dataset$diff_Y <- NULL
  dataset$diff_cost <- NULL
  dataset$M <- NULL
  dataset$missing_cost <- NULL
  dataset$missing_Y <- NULL
  dataset$Y <- NULL
  dataset$cost <- NULL
  
  #2 split dataset by treatment group 
  Tr0 <- subset(dataset, treatment==0)
  Tr1 <- subset(dataset, treatment==1)
  
  #3 create a customized predictor matrix
  #the variables in the columns are used to impute the variables in the rows. 
  #the imputation model includes the confounders and predictors of missing data
  #(i.e.., age, rom, depression) and
  #outcome variables: Y_mw, cost_mw
  predMat <- make.predictorMatrix(dataset)
  predMat[,'treatment'] <- 0
  predMat[,'leefbar'] <- 0
  predMat[,'gender'] <- 0
  predMat[,'Y_mw'] <- 1
  predMat[,'cost_mw'] <- 1
  
  #4 perform MI procedure by Tr and combine them
  imp.Tr0 <- mice(Tr0, m=5, method="pmm", predictorMatrix = predMat, seed = 1234, printFlag = FALSE)
  imp.Tr1 <- mice(Tr1, m=5, method="pmm", predictorMatrix = predMat, seed = 1234, printFlag = FALSE)
  
  #5 merge and stack imputed datasets per treatment group
  imp <- rbind(imp.Tr0, imp.Tr1)
  impdat <- complete(imp, action = "long", include = FALSE)
  
  #6 store imputed datasets in a list
  impdata <- split(impdat, f = impdat$.imp)
  
  #7 extract the number of imputations to be used in Rubin's rules
  M <- imp[["m"]]
  
  #8 fit seemingly unrelated regressions model in each imputed dataset stored in impdata (SUR)
  r1 <- cost_mw ~ treatment + age + rom + depression
  r2 <- Y_mw ~ treatment + age + rom + depression
  sur <- lapply(impdata, function(x) {systemfit(list(costreg = r1, effectreg = r2), "SUR", data=x)})
  
  #9 extract betas for costs and effects
  cost_diff <- lapply(sur, function(x) x[["coefficients"]][["costreg_treatment"]])
  effect_diff <- lapply(sur, function(x) x[["coefficients"]][["effectreg_treatment"]])
  
  #10 extract variance within imputed datasets
  varcov <- lapply(sur, function(x) x[["coefCov"]])
  var_cost <- lapply(varcov, function(x) x[2,2])
  var_effect <- lapply(varcov, function(x) x[7,7])
  cov <- lapply(varcov, function(x) x[2,7])
  
  #11 pool cost and effect differences using Rubin's rules
  imputed <- matrix(0, ncol = 2, nrow = M)
  colnames(imputed)  <- c("cost_diff","effect_diff")
  for (i in 1:M){
    imputed[i,1] <- cost_diff[[i]]
    imputed[i,2] <- effect_diff[[i]]
  }
  pooled <- apply(imputed, 2, mean)
  
  #12 pool within-imputation variance using Rubin's rules
  var <- matrix(0, ncol = 2, nrow = M)
  colnames(var)  <- c("var_cost","var_effect")
  for (i in 1:M){
    var[i,1] <- var_cost[[i]]
    var[i,2] <- var_effect[[i]]
  }
  W <- apply(var, 2, mean)
  
  #13 pool between-imputation variance using Rubin's rules
  B <- matrix(0, ncol = 2, nrow = 2)
  for (i in 1:M){
    B <- B + (matrix(imputed[i,], nrow = 2) - pooled) %*% (matrix(imputed[i,], nrow = 1) - pooled)
  }
  B <- 1/(M - 1) * B
  
  #14 pool within and between-imputation variances
  var_pooled <- (1 + 1/M) * B + W
  colnames(var_pooled)  <- c("var_cost","var_effect")
  
  #15 estimate lower and upper confidence interval limits for costs and effects using Rubin's rules
  Za = 1.95996
  dataset$cost_diff <- pooled[1]
  dataset$LL_cost_pooled <- pooled[1] - (Za*sqrt(var_pooled[1,1])) # lower-limit of the 95% CI for costs
  dataset$UL_cost_pooled <- pooled[1] + (Za*sqrt(var_pooled[1,1])) # upper-limit of the 95% CI for costs
  dataset$effect_diff <- pooled[2]
  dataset$LL_effect_pooled <- pooled[2] - (Za*sqrt(var_pooled[2,2])) # lower-limit of the 95% CI for QALY
  dataset$UL_effect_pooled <- pooled[2] + (Za*sqrt(var_pooled[2,2])) # upper-limit of the 95% CI for QALY
  
  #16 loss of efficiency
  FMI = B/(B + W)
  LE = FMI/M
  dataset$LE_cost <- LE[1,1]
  dataset$LE_effect <- LE[2,2]
  
  dataset <- as.data.frame(dataset)
  
}

mi_mice_time <- Sys.time() #initial time

mi_mice <- lapply(data,MI.MICE)

for (i in 1:10) {
  
  write_xlsx(mi_mice[[i]], paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/MI_MICE_M10_HL",i,".xlsx"))
}

Sys.time() - mi_mice_time # total time