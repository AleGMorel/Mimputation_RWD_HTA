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



#####Missings in both Costs and Effects#####

#Definition:
#Percentage of missing data = (Number of observations with NA) * 100 /(Total number of observations)

MISSING10 <- function(ds){
  
  ###Scenario 1: Missingness predictors are the same as the confounders
  
  ##Missingness models: include confounders (ROM and depression)
  missing_p_Y <- inv_logit(-6.5 + 0.01*ds$rom + 0.02*ds$depression) # probability of missing in outcome
  missing_p_cost <- inv_logit(-5.5 + 0.01*ds$rom + 0.02*ds$depression) #probability of missing in costs
  ds$missing_Y <- rbinom(n, 1, missing_p_Y) #creating binary missing data indicator 
  ds$missing_cost <- rbinom(n, 1, missing_p_cost)
  
  #Creating outcomes and costs with missing values
  ds$Y_miss = ds$Y 
  ds$Y_miss <-ifelse(ds$missing_Y==1, ds$Y_miss==NA,ds$Y)
  ds$cost_miss = ds$cost
  ds$cost_miss <-ifelse(ds$missing_cost==1, ds$cost_miss==NA,ds$cost)
  
  #check proportion of missing data
  ds$M <- as.integer(complete.cases(ds))
  ds$percentage_miss =  (1-(sum(ds$M)/n)) *100
  print(percentage_miss)
  #descr(ds$M) # mean represents the proportion of complete data ~90% or 10% missing  data
  
  ds <- as.data.frame(ds)
}
  
missing10 <- lapply(data,MISSING10)

for (i in 1:20) {
  
  write_xlsx(missing10[[i]], paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/M10_HL",i,".xlsx"))
}

#########################################################################################################################
  ###Scenario 2: Missingness predictors differ from the confounders

MISSING10 <- function(ds){
  
  #Missingness model: confounders (ROM and depression) are NOT included in the missingness probability 
  missing_p_Y <- inv_logit(-6.5 + 0.02*ds$age + 0.02*ds$leefbar) # probability of missing in outcome
  missing_p_cost <- inv_logit(-5.5 + 0.02*ds$age + 0.02*ds$leefbar) #probability of missing in costs
  ds$missing_Y <- rbinom(n, 1, missing_p_Y) #creating binary missing data indicator 
  ds$missing_cost <- rbinom(n, 1, missing_p_cost)
  
  #Creating outcomes and costs with missing values
  ds$Y_miss = ds$Y 
  ds$Y_miss <-ifelse(ds$missing_Y==1, ds$Y_miss==NA,ds$Y)
  ds$cost_miss = ds$cost
  ds$cost_miss <-ifelse(ds$missing_cost==1, ds$cost_miss==NA,ds$cost)
  
  #check proportion of missing data
  ds$M <- as.integer(complete.cases(ds))
  percentage_miss =  (1-(sum(ds$M)/n)) *100
  print(percentage_miss)
  #descr(ds$M) # mean represents the proportion of complete data ~90% or 10% missing  data
  
  ds <- as.data.frame(ds)
  }
  
  missing10 <- lapply(data,MISSING10)
  
  for (i in 1:20) {
    
    write_xlsx(missing10[[i]], paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/M10_HL",i,".xlsx"))
  }