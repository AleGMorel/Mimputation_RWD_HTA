#########Checking MAR assumption########

#########High level of confounding, 10 % missing data######################
library(readxl)
#Load dataset, with complete data and missing data columns
M10_HL1 <- read_excel("~/HTA PhD/Missing Data Simulation/R codes/Time to Get Real/Data/HY_HC/MISSING10/M10_HL1.xlsx")

#Defining data
cost <- M10_HL1$cost
cost_miss <- M10_HL1$cost_miss
Y <- M10_HL1$Y
Y_miss <- M10_HL1$Y_miss

####COST DATA####

###Histogram of complete datasets vrs datasets with missings data in costs
#plot two histograms in same graph
hist(cost , col='light yellow',
     xlab='Values', ylab='Frequency', main='Comparing complete and incomplete cost data')
hist(cost_miss, col='light blue', add=TRUE)

#add legend
legend('topright',c('costs', 'costs with MD'),
       fill=c(col='light yellow', col='light blue'))

###Boxplot of complete datasets vrs datasets with missings data in outcome
boxplot(cost, cost_miss,
        data=M10_HL10,
        main="Comparing complete and incomplete cost data",
        xlab = 'Complete vrs missing costs',
        ylab='Costs (in Euros)', 
        col="light blue",
        border="black",
        horizontal = FALSE
)


####OUTCOME DATA####

###Histogram of complete datasets vrs datasets with missings data in Outcome
#plot two histograms in same graph
hist(Y , col='light yellow',
     xlab='Values', ylab='Frequency', main='Comparing complete and incomplete cost data')
hist(Y_miss, col='light blue', add=TRUE)

#add legend
legend('topright',c('Y', 'Y with MD'),
       fill=c(col='light yellow', col='light blue'))

###Boxplot of complete datasets vrs datasets with missings data in outcome
boxplot(Y, Y_miss,
        data=M10_HL10,
        main="Comparing complete and incomplete outcome data",
        xlab = 'Complete vrs missing outcomes',
        ylab='Outcome', 
        col="light blue",
        border="black",
        horizontal = FALSE
)