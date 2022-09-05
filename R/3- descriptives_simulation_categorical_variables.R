################
###Gender#######
#
#per dataset
library(readxl)
LL1 <- read_excel("~/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/LY_LC/LL1.xlsx")


##Gender per category##
cat.gender <- function(ds){
  c.gender <- ds%>%
    count(gender)
}

###############################################
###Low level confounding simulated datasets###

#check gender across LL simulated datasets
LL.mean.gender.list<- (mclapply(sample_LY_LC, cat.gender)) #until here its fine
LL.mean.gender.vector <- sapply(LL.mean.gender.list, mean)
mean(LL.mean.gender.vector)