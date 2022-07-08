###### Introduce missings at 10% in HY_HC complete datasets######

library(writexl)  # required to save data in .xls
library(readxl)   #required to read the imported excel ds
library(parallel) # required for the mclapply function
library(descr)    # required to describe data

## Import datasets
dataset <- paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/HL",1:20,".xlsx")
data <- mclapply(dataset, read_excel)

## Introduce missings
#Defining inverse log function
n <-1000 #number of observations
inv_logit <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
} # load the inv_logit function

ds<- HL1

MISSING10 <- function(ds){
  
  ###Scenario 1: MAR assumption###
  #Missing data indicator, using ROM as a confounder
  missing_p_Y <- inv_logit(-6 + 0.01*ds$rom + 0.02*ds$age + 0.02*ds$depression) # probability of missing in outcome
  missing_p_cost <- inv_logit(-5 + 0.01*ds$rom + 0.02*ds$age + 0.02*ds$depression) #probability of missing in costs
  ds$missing_Y <- rbinom(n, 1, missing_p_Y) #creating binary missing data indicator 
  ds$missing_cost <- rbinom(n, 1, missing_p_cost)
  
  #Creating outcomes and costs with missing values
  ds$Y_miss = ds$Y 
  ds$Y_miss <-ifelse(ds$missing_Y==1, ds$Y_miss==NA,ds$Y)
  ds$cost_miss = ds$cost
  ds$cost_miss <-ifelse(ds$missing_cost==1, ds$cost_miss==NA,ds$cost)
  
  #####Check adjusted vrs non-adjusted estimates##### --> to be removed
  #1) Effects
  lm1 <- lm(Y_miss ~ treatment, data = ds) #no adjustment 
  lm2 <- lm(Y_miss ~ treatment + rom + age + depression, data = ds) #adjusted for rom, age and depression
  coef1 <- lm1[["coefficients"]][["treatment"]] # extract coefficient no confounding
  coef2 <- lm2[["coefficients"]][["treatment"]] # extract coefficient ROM as a confounder
  ds$diff_Y_miss <- abs(((coef1-coef2)/coef1) *100) #Percentage of difference between raw and adjusted coefficients
  diff_Y_miss <-abs(((coef1-coef2)/coef1) *100)
  
  #2) Costs
  lm3 <- lm(cost_miss ~ treatment, data = ds) #Raw: no confounder added
  lm4 <- lm(cost_miss ~ treatment + rom + age + depression, data = ds) #Adjusted: ROM as a confounder
  coef3 <- lm3[["coefficients"]][["treatment"]] # extract coefficient no confounding
  coef4 <- lm4[["coefficients"]][["treatment"]] # extract coefficient ROM as a confounder
  ds$diff_cost_miss <- abs(((coef3-coef4)/coef3) *100) #Percentage of difference between raw and adjusted coefficients
  diff_cost_miss <- abs(((coef3-coef4)/coef3) *100)
  
  #check proportion of missing data
  ds$M <- as.integer(complete.cases(ds))
  percentage_miss =  (1-(sum(ds$M)/n)) *100
  print(percentage_miss)
  descr(ds$M) # mean represents the proportion of complete data ~90% or 10% missing  data
  
  percentage_miss
  ds <- as.data.frame(ds)
}
  
missing10 <- lapply(data,MISSING10)

for (i in 1:20) {
  
  write_xlsx(missing10[[i]], paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/M10_HL",i,".xlsx"))
}
