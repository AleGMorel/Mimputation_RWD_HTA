############################
###########Checks###########
############################

library(parallel)
library(stats)

# Individual tests to check if datasets stored in na have missings
test1 <-na[[1]]
sum(is.na(test1))

test2 <-na[[2]]
sum(is.na(test2))

test3 <-na[[3]]
sum(is.na(test3))

test4 <-na[[4]]
sum(is.na(test4))

#################################################################
#Checking mean difference between regression coefficients

#coef1-coef2 
#Coef 1: regression coefficient no confounding
#Coef 2: coefficient ROM as a confounder

CF <- function(x){
  diff <- mean(x$diff)
}

# check mean LL across simulated datasets
all.LL <-(mclapply(LL, CF))
all.LL.vector <- sapply(all.LL, mean)
summary(all.LL.vector)

# check mean ML across simulated datasets
all.ML <-(mclapply(ML, CF))
all.ML.vector <- sapply(all.ML, mean)
summary(all.ML.vector)

# check mean HL across simulated datasets
all.HL <-(mclapply(HL, CF))
all.HL.vector <- sapply(all.HL, mean)
summary(all.HL.vector)

