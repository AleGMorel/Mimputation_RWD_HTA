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

MISSING10 <- function(ds){
  
  ###Scenario 1: MAR assumption###
  #Missing data indicator, using ROM as a confounder
  missing_p_Y <- inv_logit(-4 + 0.01*ds$rom + 0.02*ds$age + 0.02*ds$gender) # probability of missing in outcome
  missing_p_cost <- inv_logit(-3 + 0.01*ds$rom + 0.02*ds$age + 0.02*ds$gender) #probability of missing in costs
  ds$missing_Y <- rbinom(n, 1, missing_p_Y) #creating binary missing data indicator 
  ds$missing_cost <- rbinom(n, 1, missing_p_cost)
  
  #Creating outcomes and costs with missing values
  ds$Y_mw = ds$Y 
  ds$Y_mw <-ifelse(ds$missing_Y==1, ds$Y_mw==NA,ds$Y)
  ds$cost_mw = ds$cost
  ds$cost_mw <-ifelse(ds$missing_cost==1, ds$cost_mw==NA,ds$cost)
  
  #check proportion of missing data
  ds$M <- as.integer(complete.cases(ds))
  descr(ds$M) # mean represents the proportion of complete data ~90% or 10% missing data
  
  ds <- as.data.frame(ds)
}
  
missing10 <- lapply(data,MISSING10)

for (i in 1:20) {
  
  write_xlsx(missing10[[i]], paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/M10_HL",i,".xlsx"))
}
