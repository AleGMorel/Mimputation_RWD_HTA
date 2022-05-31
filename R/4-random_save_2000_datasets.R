####--- Select random data sets from List in R####

##Reference: https://statisticsglobe.com/select-random-element-from-list-in-r
library(writexl)  # required to save data in .xls
library(readxl)

set.seed(36958)

##Randomly sampling 2000 data sets from the list without replacement
sample_LY_LC <- sample(x= LY_LC, size = 2000)#Low level confounding
sample_MY_MC <- sample(x= MY_MC, size = 2000)#Medium level confounding
sample_HY_HC <- sample(x= HY_HC, size = 2000)#High level confounding

##Save data sets in a specific folder
#Low level confounding:
for (i in 1:length(sample_LY_LC)) {
  write_xlsx(sample_LY_LC[[i]], 
             paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/LY_LC/LL",
                    i,".xlsx"))
}

#Medium level confounding:
for (i in 1:length(sample_MY_MC)) {
  write_xlsx(sample_MY_MC[[i]], 
             paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/MY_MC/ML",
                    i,".xlsx"))
}

#High level confounding
for (i in 1:length(sample_HY_HC)) {
  write_xlsx(sample_HY_HC[[i]], 
             paste0("C:/Users/aegue/Documents/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/HL",
                    i,".xlsx"))
}

