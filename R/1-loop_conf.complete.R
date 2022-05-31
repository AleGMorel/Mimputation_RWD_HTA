####################################################
####All levels of confounding in complete datasets##
####################################################
library(copula)
library(writexl)  # required to save data in .xls
library(readxl) #required to read the imported excel dataset

# All levels of confounding: 
# Low Level (LL): difference between raw and adjusted Beta coefficients in costs and effects <= 10% 
# Medium Level (ML): difference between raw and adjusted Beta coefficients in costs and effects > 10 - <=20%
# High Level (HL): difference between raw and adjusted Beta coefficients in costs and effects >20%

time <- Sys.time()

#Creating lists to allocate the different datasets
na <- c()
others <- c()
LY_LC <- c()
MY_MC <- c()
HY_HC <- c()

for (i in 1:30000) {
  
  set.seed(i)
  ###Data generation different levels of confounding###
  #1) Generate a sample of 1000 participants
  N <- 1000 #number of participants
  
  #2) Generate characteristics
  gender=(runif(N)<=.625)+0 
  age = floor(rnorm(n = N, mean = c(54, 46), sd = 9))
  depression = rnorm(n = N, mean = c(50, 60), sd = 1)
  rom = rnorm(n = N, mean = c(70, 60), sd = 5)
  leefbar <- sample(1:5, N, replace=TRUE, prob=c(0.1, 0.19, 0.42, 0/.27,0.02) ) #SES
  #response = rnorm(n = N, mean = E.response, sd = 1)
  
  #3) Assign participants to treatment groups
  psc_logit.llc<-log(0.3)+(0.001*age)+(0.02*rom)+(0.02*depression) #increasing rom by 0.01
  p <- 1/(1 + exp(-psc_logit.llc))
  treatment <- rbinom(N, size = 1, prob = p)
  table(treatment)
  
  #4) Expected values of costs and effects
  E.cost <- -15+(0.3*rom)+(0.5*treatment)+(0.02*leefbar)+(0.02*depression) 
  E.Y <- (0.3*rom)+(0.5*treatment)+(0.02*leefbar)+(0.02*depression)
  
  #5) Constructing multivariate distribution from copulas
  #creating correlation between costs and effects
  ngmvdc <- mvdc(copula=normalCopula(0.4), margins = c("norm", "gamma"),
                 paramMargins=list(list(mean = E.Y, sd =0.2),
                                   list(shape=10,rate=10/E.cost)))   
  sampled <- rMvdc(N,ngmvdc) #multivariate distribution from copula
  #  Y <- (0.3*rom)+(0.5*treatment)+(0.02*leefbar)+(0.02*depression)+sampled[,1]
  cost <- sampled[,2]*1000
  Y <- sampled[,1]
  
  #6) Creating the final complete dataset with all levels of confounding
  complete <- data.frame(age,leefbar,gender,rom,treatment,Y,cost,depression)
  
  ###Linear regressions: raw and adjusted###
  #1) Effects
  lm1 <- lm(Y ~ treatment, data = complete) #Raw: no confounder added
  lm2 <- lm(Y ~ rom + treatment, data = complete) # Ajusted: ROM as a confounder
  coef1 <- lm1[["coefficients"]][["treatment"]] # extract coefficient no confounding
  coef2 <- lm2[["coefficients"]][["treatment"]] # extract coefficient ROM as a confounder
  complete$diff_Y <- abs(((coef1-coef2)/coef1) *100) #Percentage of difference between raw and adjusted coefficients
  diff_Y <-abs(((coef1-coef2)/coef1) *100)

  #2) Costs
  lm3 <- lm(cost ~ treatment, data = complete) #Raw: no confounder added
  lm4 <- lm(cost ~ rom + treatment, data = complete) #Adjusted: ROM as a confounder
  coef3 <- lm3[["coefficients"]][["treatment"]] # extract coefficient no confounding
  coef4 <- lm4[["coefficients"]][["treatment"]] # extract coefficient ROM as a confounder
  complete$diff_cost <- abs(((coef3-coef4)/coef3) *100) #Percentage of difference between raw and adjusted coefficients
  diff_cost <- abs(((coef3-coef4)/coef3) *100)
  
  ###Classification of datasets according to level of confounding### 
  if (sum(is.na(complete$cost)) >=1){ #datasets with Na's 
    na <- append(na, list(complete)) 
  } else if((diff_Y <= 10) & 
            (diff_cost <= 10)){ #Low effects & low costs
    LY_LC <- append(LY_LC, list(complete))
  } else if((diff_Y > 10 & diff_Y <= 20) &  
            (diff_cost > 10 & diff_cost <= 20)){ #Medium effects & medium costs
    MY_MC <- append(MY_MC, list(complete))
  } else if((diff_Y > 20) &  
            (diff_cost > 20)){ #High effects & high costs
    HY_HC <- append(HY_HC, list(complete))
  } else {
    others <- append(others, list(complete)) 
  }         
}

Sys.time() - time  # calculate time to run the simulations by extracting time from the current system time  
