################################################
# Compute interval forecasts for OECD countries
################################################

## Load the main functions before computation 
## source("1_Main_functions.r") Uncomment this line to source the R script.
## Note that the "_test" in object names in this script actually refers to the validation set. The logic of naming is to initially "test" the estimated model on the validation set. Applogise for any confusions caused.

# Load required R packages

require(StMoMo)
library(demography)

# Compute prediction intervals

OECD_PI_test = paste(OECD_countries, "_PI_test", sep = "")

for(ij in 1:length(OECD_countries))
{
  # This loop may take significant time to complete.
  # Use a "save & load" strategy in case of R crashes.
  PI_temp = err_fun_interval_testing(index = ij,  state_select = OECD_all_data, state_select_smooth = OECD_all_smooth, nboot = 1000)
  assign(OECD_PI_test[ij], PI_temp)
  
  save(PI_temp, file = paste(OECD_countries[ij], "_PI_test", ".RData", sep = ""))
  
  rm(PI_temp)
  gc()
  print(ij)
}

# Compute averaged prediction interval and interval score

OECD_interval_test_average = paste0(OECD_countries, "_interval_test_average")
OECD_interval_test_interior_trimming = paste0(OECD_countries, "_interval_test_interior_trimming")
OECD_interval_test_mse = paste0(OECD_countries, "_interval_test_mse")
OECD_interval_test_AIC = paste0(OECD_countries, "_interval_test_AIC")
OECD_interval_test_SHAP_no_truncate = paste0(OECD_countries, "_interval_test_SHAP_no_truncate")
OECD_interval_test_SHAP_5 = paste0(OECD_countries, "_interval_test_SHAP_5")
OECD_interval_test_SHAP_10 = paste0(OECD_countries, "_interval_test_SHAP_10")
OECD_interval_test_SHAP_15 = paste0(OECD_countries, "_interval_test_SHAP_15")
OECD_interval_test_SHAP_20 = paste0(OECD_countries, "_interval_test_SHAP_20")
OECD_interval_test_SHAP_50 = paste0(OECD_countries, "_interval_test_SHAP_50")


for(ij in 1:length(OECD_countries))
{
  # Simple Averaging
  temp_interval = combine_interval(country_index = ij, method = "Average")
  assign(OECD_interval_test_average[ij], temp_interval)
  rm(temp_interval)

  # Interior Trimming
  temp_interval = combine_interval(country_index = ij, method = "Interior_trimming")
  assign(OECD_interval_test_interior_trimming[ij], temp_interval)
  rm(temp_interval)
  
  # AIC
  temp_interval = combine_interval(country_index = ij, method = "AIC")
  assign(OECD_interval_test_AIC[ij], temp_interval)
  rm(temp_interval)

  # MSE
  temp_interval = combine_interval(country_index = ij, method = "MSE")
  assign(OECD_interval_test_mse[ij], temp_interval)
  rm(temp_interval)
  
  # SHAP without truncations
  temp_interval = combine_interval(country_index = ij, method = "SHAP", alpha = 0)
  assign(OECD_interval_test_SHAP_no_truncate[ij], temp_interval)
  rm(temp_interval)
  
  # SHAP with alpha = 0.05
  temp_interval = combine_interval(country_index = ij, method = "SHAP", alpha = 0.05)
  assign(OECD_interval_test_SHAP_5[ij], temp_interval)
  rm(temp_interval)
  
  
  # SHAP with alpha = 0.1
  temp_interval = combine_interval(country_index = ij, method = "SHAP", alpha = 0.1)
  assign(OECD_interval_test_SHAP_10[ij], temp_interval)
  rm(temp_interval)
  
  # SHAP with alpha = 0.15
  temp_interval = combine_interval(country_index = ij, method = "SHAP", alpha = 0.15)
  assign(OECD_interval_test_SHAP_15[ij], temp_interval)
  rm(temp_interval)
  
  # SHAP with alpha = 0.2
  temp_interval = combine_interval(country_index = ij, method = "SHAP", alpha = 0.2)
  assign(OECD_interval_test_SHAP_20[ij], temp_interval)
  rm(temp_interval)
  
  # SHAP with alpha = 0.5
  temp_interval = combine_interval(country_index = ij, method = "SHAP", alpha = 0.5)
  assign(OECD_interval_test_SHAP_50[ij], temp_interval)
  rm(temp_interval)
  
  print(ij)
}

#####################
# summary of results
#####################

# Combination methods in the literature

for(ih in 1:10)
{
  interval_test_table_basic = list()
  
  for(country_index in 1:length(OECD_countries))
  {
    ## Simple Average
    SA_F = get(OECD_interval_test_average[country_index])$mean_interval_score_female[ih]
    SA_M = get(OECD_interval_test_average[country_index])$mean_interval_score_male[ih]
    
    ## Iterior Trimming
    IT_F = get(OECD_interval_test_interior_trimming[country_index])$mean_interval_score_female[ih]
    IT_M = get(OECD_interval_test_interior_trimming[country_index])$mean_interval_score_male[ih]
    
    ## AIC
    AIC_F = get(OECD_interval_test_AIC[country_index])$mean_interval_score_female[ih]
    AIC_M = get(OECD_interval_test_AIC[country_index])$mean_interval_score_male[ih]
    
    ## MSE
    MSE_F = get(OECD_interval_test_mse[country_index])$mean_interval_score_female[ih]
    MSE_M = get(OECD_interval_test_mse[country_index])$mean_interval_score_male[ih]
    
    interval_test_table_basic[[country_index]] = cbind(SA_F, SA_M, IT_F, IT_M,
                                                          AIC_F, AIC_M, MSE_F, MSE_M)
  }
  
  output_latex_temp = do.call(rbind.data.frame, interval_test_table_basic)
  rownames(output_latex_temp) = OECD_full_name
  
  output_table_name = paste("output_interval_test_table_basic_h", ih, sep = "")
  assign(output_table_name, output_latex_temp)
  
  rm(output_latex_temp, output_table_name)
}

# Shapley-based combination methods

for(ih in 1:10)
{
  interval_test_table_SHAP = list()
  
  for(country_index in 1:length(OECD_countries))
  {
    ## SHAP no truncation
    SHAP_no_truncate_F = get(OECD_interval_test_SHAP_no_truncate[country_index])$mean_interval_score_female[ih]
    SHAP_no_truncate_M = get(OECD_interval_test_SHAP_no_truncate[country_index])$mean_interval_score_male[ih]
    
    ## SHAP_5
    SHAP_5_F = get(OECD_interval_test_SHAP_5[country_index])$mean_interval_score_female[ih]
    SHAP_5_M = get(OECD_interval_test_SHAP_5[country_index])$mean_interval_score_male[ih]
    
    ## SHAP_10
    SHAP_10_F = get(OECD_interval_test_SHAP_10[country_index])$mean_interval_score_female[ih]
    SHAP_10_M = get(OECD_interval_test_SHAP_10[country_index])$mean_interval_score_male[ih]
    
    ## SHAP_20
    SHAP_20_F = get(OECD_interval_test_SHAP_20[country_index])$mean_interval_score_female[ih]
    SHAP_20_M = get(OECD_interval_test_SHAP_20[country_index])$mean_interval_score_male[ih]
    
    interval_test_table_SHAP[[country_index]] = cbind(SHAP_no_truncate_F,  SHAP_no_truncate_M,
                                                      SHAP_5_F, SHAP_5_M,
                                                      SHAP_10_F, SHAP_10_M,
                                                      SHAP_20_F, SHAP_20_M)
  }
  
  output_latex_temp = do.call(rbind.data.frame, interval_test_table_SHAP)
  rownames(output_latex_temp) = OECD_full_name
  
  output_table_name = paste("output_interval_test_table_SHAP_h", ih, sep = "")
  assign(output_table_name, output_latex_temp)
  
  rm(output_latex_temp, output_table_name)
}

# Replicate the tables in our paper

output_interval_vertical_t1 = cbind(output_interval_test_table_basic_h1[,c(1,2)],
                                    output_interval_test_table_SHAP_h1[,c(1,2,3,4)])
output_interval_vertical_t1 = rbind(output_interval_vertical_t1, apply(output_interval_vertical_t1,2,mean))
rownames(output_interval_vertical_t1)[25] = "Mean"

output_interval_vertical_t2 = output_interval_test_table_basic_h1[,3:8]
output_interval_vertical_t2 = rbind(output_interval_vertical_t2, apply(output_interval_vertical_t2,2,mean))
rownames(output_interval_vertical_t2)[25] = "Mean"



