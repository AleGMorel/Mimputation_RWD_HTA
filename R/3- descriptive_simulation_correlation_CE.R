###Checking Correlation between costs and effects###

library(writexl)  # required to save data in .xls
library(readxl)   #required to read the imported excel ds
library(parallel) # required for the mclapply function
library(descr)  
library(stats)

#load dataset
## Import datasets
dataset <- paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/HL",1:20,".xlsx")
data <- mclapply(dataset, read_excel)

#correlation individual datasets
cor.test(ds$cost, ds$Y, method = "spearman")

#plot
ggscatterstats(data = df, x = cost, y = Y, type = "nonparametric")

#Check correlation using Spearman's correlation coefficient (due to left-skewed costs)
#Function to check correlation between costs and effects
cor.CE <- function(x){
  S.cor <- cor (x$cost, x$Y, method = "spearman")  
}

# check correlation across HY_HC simulated datasets
cor.HH <-(mclapply(data, cor.CE))
cor.HH.vector <- sapply(cor.HH, mean)
summary (cor.HH.vector)



