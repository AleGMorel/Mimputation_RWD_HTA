####################################################
###Descriptive all datasets per confounding level###
####################################################

library(parallel)
library(stats)

#########
##AGE ###

##Mean and Standard deviation##

##Mean##
overall.mean.age <- function(ds){
  m.age <- mean(ds$age)
}

##Standard deviation##
overall.sd.age <- function(ds){
  sd.age <-sd(ds$age)  
}  

#Minimum Age##
overall.min.age <- function(ds){
  min.age <- min(ds$age)
}

#Maximum Age
overall.max.age <- function(ds){
  max.age <- max(ds$age)
}

###############################################
###Low level confounding simulated datasets###

#check mean age across LL simulated datasets
LL.mean.age.list<- (mclapply(sample_LY_LC, overall.mean.age))
LL.mean.age.vector <- sapply(LL.mean.age.list, mean)
mean(LL.mean.age.vector)

#Check standard deviation across LL simulated datasets
LL.sd.age.list <-(mclapply(sample_LY_LC, overall.sd.age))
LL.sd.age.vector <-sapply(LL.sd.age.list, mean)
mean(LL.sd.age.vector)

#check min age across LL simulated datasets
LL.min.age.list <- (mclapply(sample_LY_LC,overall.min.age))
LL.min.age.vector <-sapply(LL.min.age.list, mean)
mean(LL.min.age.vector)

#check max age across LL simulated datasets
LL.max.age.list <- (mclapply(sample_LY_LC,overall.max.age))
LL.max.age.vector <-sapply(LL.max.age.list, mean)
mean(LL.max.age.vector)

#################################################
###Medium level confounding simulated datasets###

#check mean age across ML simulated datasets
ML.mean.age.list<- (mclapply(sample_MY_MC, overall.mean.age))
ML.mean.age.vector <- sapply(ML.mean.age.list, mean)
mean(ML.mean.age.vector)

#Check standard deviation across ML simulated datasets
ML.sd.age.list <-(mclapply(sample_MY_MC, overall.sd.age))
ML.sd.age.vector <-sapply(ML.sd.age.list, mean)
mean(ML.sd.age.vector)

#check min age across ML simulated datasets
ML.min.age.list <- (mclapply(sample_MY_MC,overall.min.age))
ML.min.age.vector <-sapply(ML.min.age.list, mean)
mean(ML.min.age.vector)

#check max age across ML simulated datasets
ML.max.age.list <- (mclapply(sample_MY_MC,overall.max.age))
ML.max.age.vector <-sapply(ML.max.age.list, mean)
mean(ML.max.age.vector)

################################################
###High level confounding simulated datasets###

#check mean age across HL simulated datasets
HL.mean.age.list<- (mclapply(sample_HY_HC, overall.mean.age))
HL.mean.age.vector <- sapply(HL.mean.age.list, mean)
mean(HL.mean.age.vector)

#Check standard deviation across HL simulated datasets
HL.sd.age.list <-(mclapply(sample_HY_HC, overall.sd.age))
HL.sd.age.vector <-sapply(HL.sd.age.list, mean)
mean(HL.sd.age.vector)

#check min age across HL simulated datasets
HL.min.age.list <- (mclapply(sample_HY_HC,overall.min.age))
HL.min.age.vector <-sapply(HL.min.age.list, mean)
mean(HL.min.age.vector)

#check max age across HL simulated datasets
HL.max.age.list <- (mclapply(sample_HY_HC,overall.max.age))
HL.max.age.vector <-sapply(HL.max.age.list, mean)
mean(HL.max.age.vector)


################
##ROM scores ###

##Mean and Standard deviation##

##Mean##
overall.mean.rom <- function(ds){
  m.rom <- mean(ds$rom)
}

##Standard deviation##
overall.sd.rom <- function(ds){
  sd.rom <-sd(ds$rom)  
}  

#Minimum ROM scores##
overall.min.rom <- function(ds){
  min.rom <- min(ds$rom)
}

#Maximum ROM scores##
overall.max.rom <- function(ds){
  max.rom <- max(ds$rom)
}

###############################################
###Low level confounding simulated datasets###

#check mean ROM scores  across LL simulated datasets
LL.mean.rom.list<- (mclapply(sample_LY_LC, overall.mean.rom))
LL.mean.rom.vector <- sapply(LL.mean.rom.list, mean)
mean(LL.mean.rom.vector)

#Check standard deviation across LL simulated datasets
LL.sd.rom.list <-(mclapply(sample_LY_LC, overall.sd.rom))
LL.sd.rom.vector <-sapply(LL.sd.rom.list, mean)
mean(LL.sd.rom.vector)

#check min ROM scores across LL simulated datasets
LL.min.rom.list <- (mclapply(sample_LY_LC,overall.min.rom))
LL.min.rom.vector <-sapply(LL.min.rom.list, mean)
mean(LL.min.rom.vector)

#check max ROM scores across LL simulated datasets
LL.max.rom.list <- (mclapply(sample_LY_LC,overall.max.rom))
LL.max.rom.vector <-sapply(LL.max.rom.list, mean)
mean(LL.max.rom.vector)

#################################################
###Medium level confounding simulated datasets###

#check mean ROM across ML simulated datasets
ML.mean.rom.list<- (mclapply(sample_MY_MC, overall.mean.rom))
ML.mean.rom.vector <- sapply(ML.mean.rom.list, mean)
mean(ML.mean.rom.vector)

#Check standard deviation across ML simulated datasets
ML.sd.rom.list <-(mclapply(sample_MY_MC, overall.sd.rom))
ML.sd.rom.vector <-sapply(ML.sd.rom.list, mean)
mean(ML.sd.rom.vector)

#check min ROM scores across ML simulated datasets
ML.min.rom.list <- (mclapply(sample_MY_MC,overall.min.rom))
ML.min.rom.vector <-sapply(ML.min.rom.list, mean)
mean(ML.min.rom.vector)

#check max ROM scores across ML simulated datasets
ML.max.rom.list <- (mclapply(sample_MY_MC,overall.max.rom))
ML.max.rom.vector <-sapply(ML.max.rom.list, mean)
mean(ML.max.rom.vector)

################################################
###High level confounding simulated datasets###

#check mean ROM scores across HL simulated datasets
HL.mean.rom.list<- (mclapply(sample_HY_HC, overall.mean.rom))
HL.mean.rom.vector <- sapply(HL.mean.rom.list, mean)
mean(HL.mean.rom.vector)

#Check standard deviation across HL simulated datasets
HL.sd.rom.list <-(mclapply(sample_HY_HC, overall.sd.rom))
HL.sd.rom.vector <-sapply(HL.sd.rom.list, mean)
mean(HL.sd.rom.vector)

#check min ROM scores across HL simulated datasets
HL.min.rom.list <- (mclapply(HY_HC,overall.min.rom))
HL.min.rom.vector <-sapply(HL.min.rom.list, mean)
mean(HL.min.rom.vector)

#check max ROM scores across HL simulated datasets
HL.max.rom.list <- (mclapply(sample_HY_HC,overall.max.rom))
HL.max.rom.vector <-sapply(HL.max.rom.list, mean)
mean(HL.max.rom.vector)

#######################
##Depression scores ###

##Mean and Standard deviation##

##Mean##
overall.mean.depression <- function(ds){
  m.depression <- mean(ds$depression)
}

##Standard deviation##
overall.sd.depression <- function(ds){
  sd.depression <-sd(ds$depression)  
}  

#Minimum depression scores##
overall.min.depression <- function(ds){
  min.depression <- min(ds$depression)
}

#Maximum depression scores##
overall.max.depression <- function(ds){
  max.depression <- max(ds$depression)
}

###############################################
###Low level confounding simulated datasets###

#check mean depression scores  across LL simulated datasets
LL.mean.depression.list<- (mclapply(sample_LY_LC, overall.mean.depression))
LL.mean.depression.vector <- sapply(LL.mean.depression.list, mean)
mean(LL.mean.depression.vector)

#Check standard deviation across LL simulated datasets
LL.sd.depression.list <-(mclapply(sample_LY_LC, overall.sd.depression))
LL.sd.depression.vector <-sapply(LL.sd.depression.list, mean)
mean(LL.sd.depression.vector)

#check min depression scores across LL simulated datasets
LL.min.depression.list <- (mclapply(sample_LY_LC,overall.min.depression))
LL.min.depression.vector <-sapply(LL.min.depression.list, mean)
mean(LL.min.depression.vector)

#check max depression scores across LL simulated datasets
LL.max.depression.list <- (mclapply(sample_LY_LC,overall.max.depression))
LL.max.depression.vector <-sapply(LL.max.depression.list, mean)
mean(LL.max.depression.vector)

#################################################
###Medium level confounding simulated datasets###

#check mean depression across ML simulated datasets
ML.mean.depression.list<- (mclapply(sample_MY_MC, overall.mean.depression))
ML.mean.depression.vector <- sapply(ML.mean.depression.list, mean)
mean(ML.mean.depression.vector)

#Check standard deviation across ML simulated datasets
ML.sd.depression.list <-(mclapply(sample_MY_MC, overall.sd.depression))
ML.sd.depression.vector <-sapply(ML.sd.depression.list, mean)
mean(ML.sd.depression.vector)

#check min depression scores across ML simulated datasets
ML.min.depression.list <- (mclapply(sample_MY_MC,overall.min.depression))
ML.min.depression.vector <-sapply(ML.min.depression.list, mean)
mean(ML.min.depression.vector)

#check max depression scores across ML simulated datasets
ML.max.depression.list <- (mclapply(sample_MY_MC,overall.max.depression))
ML.max.depression.vector <-sapply(ML.max.depression.list, mean)
mean(ML.max.depression.vector)

################################################
###High level confounding simulated datasets###

#check mean depression scores across HL simulated datasets
HL.mean.depression.list<- (mclapply(sample_HY_HC, overall.mean.depression))
HL.mean.depression.vector <- sapply(HL.mean.depression.list, mean)
mean(HL.mean.depression.vector)

#Check standard deviation across HL simulated datasets
HL.sd.depression.list <-(mclapply(sample_HY_HC, overall.sd.depression))
HL.sd.depression.vector <-sapply(HL.sd.depression.list, mean)
mean(HL.sd.depression.vector)

#check min depression scores across HL simulated datasets
HL.min.depression.list <- (mclapply(sample_HY_HC,overall.min.depression))
HL.min.depression.vector <-sapply(HL.min.depression.list, mean)
mean(HL.min.depression.vector)

#check max depression scores across HL simulated datasets
HL.max.depression.list <- (mclapply(sample_HY_HC,overall.max.depression))
HL.max.depression.vector <-sapply(HL.max.depression.list, mean)
mean(HL.max.depression.vector)

###########
###Costs###

##Mean and Standard deviation##

##Mean##
overall.mean.cost <- function(ds){
  m.cost <- mean(ds$cost)
}

##Standard deviation##
overall.sd.cost <- function(ds){
  sd.cost <-sd(ds$cost)  
}  

#Minimum cost##
overall.min.cost<- function(ds){
  min.cost <- min(ds$cost)
}

#Maximum cost##
overall.max.cost <- function(ds){
  max.cost <- max(ds$cost)
}

###############################################
###Low level confounding simulated datasets###

#check mean costs across LL simulated datasets
LL.mean.cost.list<- (mclapply(sample_LY_LC, overall.mean.cost))
LL.mean.cost.vector <- sapply(LL.mean.cost.list, mean)
mean(LL.mean.cost.vector)

#Check standard deviation across LL simulated datasets
LL.sd.cost.list <-(mclapply(sample_LY_LC, overall.sd.cost))
LL.sd.cost.vector <-sapply(LL.sd.cost.list, mean)
mean(LL.sd.cost.vector)

#check min cost across LL simulated datasets
LL.min.cost.list <- (mclapply(sample_LY_LC,overall.min.cost))
LL.min.cost.vector <-sapply(LL.min.cost.list, mean)
mean(LL.min.cost.vector)

#check max cost across LL simulated datasets
LL.max.cost.list <- (mclapply(sample_LY_LC,overall.max.cost))
LL.max.cost.vector <-sapply(LL.max.cost.list, mean)
mean(LL.max.cost.vector)

#################################################
###Medium level confounding simulated datasets###

#check mean cost across ML simulated datasets
ML.mean.cost.list<- (mclapply(sample_MY_MC, overall.mean.cost))
ML.mean.cost.vector <- sapply(ML.mean.cost.list, mean)
mean(ML.mean.cost.vector)

#Check standard deviation across ML simulated datasets
ML.sd.cost.list <-(mclapply(sample_MY_MC, overall.sd.cost))
ML.sd.cost.vector <-sapply(ML.sd.cost.list, mean)
mean(ML.sd.cost.vector)

#check min cost across ML simulated datasets
ML.min.cost.list <- (mclapply(sample_MY_MC,overall.min.cost))
ML.min.cost.vector <-sapply(ML.min.cost.list, mean)
mean(ML.min.cost.vector)

#check max cost across ML simulated datasets
ML.max.cost.list <- (mclapply(sample_MY_MC,overall.max.cost))
ML.max.cost.vector <-sapply(ML.max.cost.list, mean)
mean(ML.max.cost.vector)

################################################
###High level confounding simulated datasets###

#check mean cost across HL simulated datasets
HL.mean.cost.list<- (mclapply(sample_HY_HC, overall.mean.cost))
HL.mean.cost.vector <- sapply(HL.mean.cost.list, mean)
mean(HL.mean.cost.vector)

#Check standard deviation across HL simulated datasets
HL.sd.cost.list <-(mclapply(sample_HY_HC, overall.sd.cost))
HL.sd.cost.vector <-sapply(HL.sd.cost.list, mean)
mean(HL.sd.cost.vector)

#check min cost across HL simulated datasets
HL.min.cost.list <- (mclapply(sample_HY_HC,overall.min.cost))
HL.min.cost.vector <-sapply(HL.min.cost.list, mean)
mean(HL.min.cost.vector)

#check max cost across HL simulated datasets
HL.max.cost.list <- (mclapply(sample_HY_HC,overall.max.cost))
HL.max.cost.vector <-sapply(HL.max.cost.list, mean)
mean(HL.max.cost.vector)

################
###Outcome (Y)###

##Mean and Standard deviation##

##Mean##
overall.mean.Y <- function(ds){
  m.Y <- mean(ds$Y)
}

##Standard deviation##
overall.sd.Y <- function(ds){
  sd.Y <-sd(ds$Y)  
}  

#Minimum Y##
overall.min.Y<- function(ds){
  min.Y <- min(ds$Y)
}

#Maximum Y##
overall.max.Y <- function(ds){
  max.Y <- max(ds$Y)
}

###############################################
###Low level confounding simulated datasets###

#check mean Y across LL simulated datasets
LL.mean.Y.list<- (mclapply(sample_LY_LC, overall.mean.Y))
LL.mean.Y.vector <- sapply(LL.mean.Y.list, mean)
mean(LL.mean.Y.vector)

#Check standard deviation across LL simulated datasets
LL.sd.Y.list <-(mclapply(sample_LY_LC, overall.sd.Y))
LL.sd.Y.vector <-sapply(LL.sd.Y.list, mean)
mean(LL.sd.Y.vector)

#check min Y across LL simulated datasets
LL.min.Y.list <- (mclapply(sample_LY_LC,overall.min.Y))
LL.min.Y.vector <-sapply(LL.min.Y.list, mean)
mean(LL.min.Y.vector)

#check max Y across LL simulated datasets
LL.max.Y.list <- (mclapply(sample_LY_LC,overall.max.Y))
LL.max.Y.vector <-sapply(LL.max.Y.list, mean)
mean(LL.max.Y.vector)

#################################################
###Medium level confounding simulated datasets###

#check mean Y across ML simulated datasets
ML.mean.Y.list<- (mclapply(sample_MY_MC, overall.mean.Y))
ML.mean.Y.vector <- sapply(ML.mean.Y.list, mean)
mean(ML.mean.Y.vector)

#Check standard deviation across ML simulated datasets
ML.sd.Y.list <-(mclapply(sample_MY_MC, overall.sd.Y))
ML.sd.Y.vector <-sapply(ML.sd.Y.list, mean)
mean(ML.sd.Y.vector)

#check min Y across ML simulated datasets
ML.min.Y.list <- (mclapply(sample_MY_MC,overall.min.Y))
ML.min.Y.vector <-sapply(ML.min.Y.list, mean)
mean(ML.min.Y.vector)

#check max Y across ML simulated datasets
ML.max.Y.list <- (mclapply(sample_MY_MC,overall.max.Y))
ML.max.Y.vector <-sapply(ML.max.Y.list, mean)
mean(ML.max.Y.vector)

################################################
###High level confounding simulated datasets###

#check mean Y across HL simulated datasets
HL.mean.Y.list<- (mclapply(HY_HC, overall.mean.Y))
HL.mean.Y.vector <- sapply(HL.mean.Y.list, mean)
mean(HL.mean.Y.vector)

#Check standard deviation across HL simulated datasets
HL.sd.Y.list <-(mclapply(sample_HY_HC, overall.sd.Y))
HL.sd.Y.vector <-sapply(HL.sd.Y.list, mean)
mean(HL.sd.Y.vector)

#check min Y across HL simulated datasets
HL.min.Y.list <- (mclapply(sample_HY_HC,overall.min.Y))
HL.min.Y.vector <-sapply(HL.min.Y.list, mean)
mean(HL.min.Y.vector)

#check max Y across HL simulated datasets
HL.max.Y.list <- (mclapply(sample_HY_HC,overall.max.Y))
HL.max.Y.vector <-sapply(HL.max.Y.list, mean)
mean(HL.max.Y.vector)


#############
###Gender ###

library (dplyr)
library(readxl)
#For one dataset

#Code for 1 dataset
#LL1 <- read_excel("~/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/LY_LC/LL1.xlsx")
#LL1%>%
#group_by(gender)%>%
 #count(gender)

##Total observations per age category##
cat.gender <- function(ds){
  m.gender <- ds%>%
    count(gender)
}

###############################################
###Low level confounding simulated datasets###

#check gender categories across LL simulated datasets
gender.LL <- (mclapply(sample_LY_LC, cat.gender))
gender.LL.vector <- sapply(gender.LL, mean) #from here its not working
mean (gender.LL.vector) #not working

library(trend)
sample_LY_LC %>%
  group_by(gender)%>%
  summarise(Slope = list(gender(Mean)), .groups = 'drop')


sample_LY_LC %>%
  group_by(gender)%>%
  summarise(gender= list(gender(Mean)), .groups = 'drop')


gender.LL.vector <- sapply(gender.LL,) #from here its not working
mean (gender.LL.vector)

#################################################
###Medium level confounding simulated datasets###

#check gender categories across ML simulated datasets




################################################
###High level confounding simulated datasets###

#check gender categories across HL simulated datasets

