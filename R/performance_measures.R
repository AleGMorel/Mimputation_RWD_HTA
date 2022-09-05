###### Performance measures ######

library(writexl)  # required to save data in .xls
library(readxl)   #required to read the imported excel ds


## Import results
HY_HC_M10_TV_results <- read_excel("~/2022-Ale-projects/project-1/output/HY_HC_M10_TV_results.xlsx")
HY_HC_M10_CCA_results <- read_excel("~/2022-Ale-projects/project-1/output/HY_HC_M10_CCA_results.xlsx")
HY_HC_M10_MI_MICE_results <- read_excel("~/2022-Ale-projects/project-1/output/HY_HC_M10_MI_MICE_results.xlsx")

## Delete unecessary variables
HY_HC_M10_MI_MICE_results$LE_cost <- NULL
HY_HC_M10_MI_MICE_results$LE_effect <- NULL

## Bind results from all methods together to get performance measures
HY_HC_M10_results.1  <- rbind(HY_HC_M10_TV_results, HY_HC_M10_CCA_results)
HY_HC_M10_results <- rbind(HY_HC_M10_results.1, HY_HC_M10_MI_MICE_results ) 


## Coverage rate

# * Confidence interval coverage rate *
# ** Cost **
# gen coverage_rate_count_costs = 1 if 250 >= LL_costs & 250 <= UL_costs
# count if coverage_rate_count_costs == 1
# mat countmat = r(N)
# gen nncount_costs = countmat[1,1]
# gen i = 2000 /* fill in number of datasets*/
# gen coverage_rate_costs = nncount_costs/i
# gen coverage_perc_costs = 100*coverage_rate_costs
# gen coverage_SE_costs = sqrt((coverage_perc_costs*(100-coverage_perc_costs))/i)
# 
# ** EffecT **
# gen coverage_rate_count_effecT = 1 if 0.04 >= LL_QALY & 0.04 <= UL_QALY
# count if coverage_rate_count_effecT == 1
# mat countmat = r(N)
# gen nncount_effecT = countmat[1,1]
# gen coverage_rate_effecT = nncount_effecT/i
# gen coverage_perc_effecT = 100*coverage_rate_effecT
# gen coverage_SE_effecT = sqrt((coverage_perc_effecT*(100-coverage_perc_effecT))/i)
# 

## Empirical bias *
# ** Cost **
# gen beta_difference_costs = cost_diff - 250
# egen empirical_bias_costs = mean(beta_difference_costs)
# gen squared_costs_betadiff = beta_difference_costs^2
# egen sum_bias_squared_costs = total(squared_costs_betadiff)
# gen bias_SE_costs = sqrt(1/(i*(i-1))*(sum_bias_squared_costs))
# 
# ** EffecT **
# gen beta_difference_effecT = QALY_diff - 0.04
# egen empirical_bias_effecT = mean(beta_difference_effecT)
# gen squared_effecT_betadiff = beta_difference_effecT^2
# egen sum_bias_squared_effecT = total(squared_effecT_betadiff)
# gen bias_SE_effecT = sqrt(1/(i*(i-1))*(sum_bias_squared_effecT))

## RMSE
# * Root-mean-square error costs*
# gen beta_diff_squared_costs = beta_difference_costs^2
# egen mean_MSE_costs = mean(beta_diff_squared_costs)
# gen mean_RMSE_costs =sqrt(mean_MSE_costs)
# 
# * Root-mean-square error standard error costs *
# gen beta_mse_costs = (beta_diff_squared_costs - mean_MSE_costs)^2
# egen sum_beta_mse_costs = total(beta_mse_costs)
# gen mse_MCse_costs = sqrt((sum_beta_mse_costs)/ (i*(i -1)))
# gen rmse_MCse_costs = sqrt(mse_MCse_costs)
#  
# * Root-mean-square error effecT*
# gen beta_diff_squared_effecT = beta_difference_effecT^2
# egen mean_MSE_effecT = mean(beta_diff_squared_effecT)
# gen mean_RMSE_effecT =sqrt(mean_MSE_effecT)
#  
# * Root-mean-square error standard error effecT *
# gen beta_mse_effecT = (beta_diff_squared_effecT - mean_MSE_effecT)^2
# egen sum_beta_mse_effecT = total(beta_mse_effecT)
# gen mse_MCse_effecT = sqrt((sum_beta_mse_effecT)/ (i*(i -1)))
# gen rmse_MCse_effecT = sqrt(mse_MCse_effecT)
#  


