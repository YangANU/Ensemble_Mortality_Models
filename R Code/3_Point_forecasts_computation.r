#############################################
# Compute point forecasts for OECD countries
#############################################

## Load the main functions before computation 
## source("1_Main_functions.r") Uncomment this line to source the R script.
## Note that the "_test" in object names in this script actually refers to the validation set. The logic of naming is to initially "test" the estimated model on the validation set. Applogise for any confusions caused.

# Load required R packages

require(StMoMo)
library(h2o)
library(shapley)
library(demography)
library(openxlsx)

# Compute forecasts for each country

OECD_all_data = c("AUT_all", "BEL_all", "CZE_all", "DNK_all", "EST_all", 
                  "FIN_all", "FRA_all", "HUN_all", "ISL_all", "IRL_all", 
                  "ITA_all", "JPN_all", "LVA_all", "LTU_all", "LUX_all", 
                  "NLD_all", "NZL_all", "NOR_all", "POL_all", "ESP_all", 
                  "SWE_all", "CHE_all", "GBR_all", "USA_all")

OECD_all_smooth = c("AUT_smooth", "BEL_smooth", "CZE_smooth", "DNK_smooth", "EST_smooth", 
                  "FIN_smooth", "FRA_smooth", "HUN_smooth", "ISL_smooth", "IRL_smooth", 
                  "ITA_smooth", "JPN_smooth", "LVA_smooth", "LTU_smooth", "LUX_smooth", 
                  "NLD_smooth", "NZL_smooth", "NOR_smooth", "POL_smooth", "ESP_smooth", 
                  "SWE_smooth", "CHE_smooth", "GBR_smooth", "USA_smooth")
				  
for(ij in 1:length(OECD_selected))
{
  all_data_temp = lapply(get(OECD_selected[ij]), extract.years, 1960:2019)
  
  # export death rates into csv
  all_mx_temp = data.frame(Year = rep(1960:2019, each = 101), Age = rep(0:100, 60), 
                             Female = na_replace(as.vector(all_data_temp$Female$rate$Female)), 
                             Male = na_replace(as.vector(all_data_temp$Male$rate$Male)), 
                             Total = na_replace(as.vector(all_data_temp$Total$rate$Total)))
  write.table(all_mx_temp, file = "all_mx_temp.txt", row.names = FALSE)
  
  # export exposure into csv
  all_expo_temp = data.frame(Year = rep(1960:2019, each = 101), Age = rep(0:100, 60), 
                               Female = na_replace(as.vector(all_data_temp$Female$pop$Female)), 
                               Male = na_replace(as.vector(all_data_temp$Male$pop$Male)), 
                               Total = na_replace(as.vector(all_data_temp$Total$pop$Total)))
  write.table(all_expo_temp, file = "all_expo_temp.txt", row.names = FALSE)
  
  # assign demogdata
  demogdata_temp = read.demogdata("all_mx_temp.txt", "all_expo_temp.txt", "mortality", OECD_countries[ij], skip = 0, popskip = 0)
  assign(OECD_all_data[ij], demogdata_temp)
  
  # smoothing
  demogdata_temp_smooth = smooth.demogdata(demogdata_temp)
  assign(OECD_all_smooth[ij], demogdata_temp_smooth)
  
  rm(all_data_temp, all_mx_temp, all_expo_temp, demogdata_temp, demogdata_temp_smooth)
  
}				  

# Define training set (1960-1999) and a validation set (2000-2009).
# Note that the "_test" in object names in the remaining script actually refers to the validation set (i.e., "testing" the estimated model on the validation set)

OECD_train = c("AUT_train", "BEL_train", "CZE_train", "DNK_train", "EST_train", 
               "FIN_train", "FRA_train", "HUN_train", "ISL_train", "IRL_train", 
               "ITA_train", "JPN_train", "LVA_train", "LTU_train", "LUX_train", 
               "NLD_train", "NZL_train", "NOR_train", "POL_train", "ESP_train", 
               "SWE_train", "CHE_train", "GBR_train", "USA_train")

OECD_train_smooth = c("AUT_train_smooth", "BEL_train_smooth", "CZE_train_smooth", "DNK_train_smooth", "EST_train_smooth", 
                      "FIN_train_smooth", "FRA_train_smooth", "HUN_train_smooth", "ISL_train_smooth", "IRL_train_smooth", 
                      "ITA_train_smooth", "JPN_train_smooth", "LVA_train_smooth", "LTU_train_smooth", "LUX_train_smooth", 
                      "NLD_train_smooth", "NZL_train_smooth", "NOR_train_smooth", "POL_train_smooth", "ESP_train_smooth", 
                      "SWE_train_smooth", "CHE_train_smooth", "GBR_train_smooth", "USA_train_smooth")

OECD_test = c("AUT_test", "BEL_test", "CZE_test", "DNK_test", "EST_test", 
              "FIN_test", "FRA_test", "HUN_test", "ISL_test", "IRL_test", 
              "ITA_test", "JPN_test", "LVA_test", "LTU_test", "LUX_test", 
              "NLD_test", "NZL_test", "NOR_test", "POL_test", "ESP_test", 
              "SWE_test", "CHE_test", "GBR_test", "USA_test")

OECD_test_smooth = c("AUT_test_smooth", "BEL_test_smooth", "CZE_test_smooth", "DNK_test_smooth", "EST_test_smooth", 
                     "FIN_test_smooth", "FRA_test_smooth", "HUN_test_smooth", "ISL_test_smooth", "IRL_test_smooth", 
                     "ITA_test_smooth", "JPN_test_smooth", "LVA_test_smooth", "LTU_test_smooth", "LUX_test_smooth", 
                     "NLD_test_smooth", "NZL_test_smooth", "NOR_test_smooth", "POL_test_smooth", "ESP_test_smooth", 
                     "SWE_test_smooth", "CHE_test_smooth", "GBR_test_smooth", "USA_test_smooth")

for(ij in 1:length(OECD_selected))
{
  ################
  # Training Data
  ################
  
  train_data_temp = lapply(get(OECD_selected[ij]), extract.years, 1960:1999)
  
  # export death rates into csv
  train_mx_temp = data.frame(Year = rep(1960:1999, each = 101), Age = rep(0:100, 40), 
                             Female = na_replace(as.vector(train_data_temp$Female$rate$Female)), 
                             Male = na_replace(as.vector(train_data_temp$Male$rate$Male)), 
                             Total = na_replace(as.vector(train_data_temp$Total$rate$Total)))
  write.table(train_mx_temp, file = "train_mx_temp.txt", row.names = FALSE)
  
  # export exposure into csv
  train_expo_temp = data.frame(Year = rep(1960:1999, each = 101), Age = rep(0:100, 40), 
                               Female = na_replace(as.vector(train_data_temp$Female$pop$Female)), 
                               Male = na_replace(as.vector(train_data_temp$Male$pop$Male)), 
                               Total = na_replace(as.vector(train_data_temp$Total$pop$Total)))
  write.table(train_expo_temp, file = "train_expo_temp.txt", row.names = FALSE)
  
  # assign demogdata
  demogdata_temp = read.demogdata("train_mx_temp.txt", "train_expo_temp.txt", "mortality", OECD_countries[ij], skip = 0, popskip = 0)
  assign(OECD_train[ij], demogdata_temp)
  
  # smoothing
  demogdata_temp_smooth = smooth.demogdata(demogdata_temp)
  assign(OECD_train_smooth[ij], demogdata_temp_smooth)
  
  rm(train_data_temp, train_mx_temp, train_expo_temp, demogdata_temp, demogdata_temp_smooth)
  
  #############
  # Validation 
  #############
  
  test_data_temp = lapply(get(OECD_selected[ij]), extract.years, 1960:2009)
  
  # export death rates into csv
  test_mx_temp = data.frame(Year = rep(1960:2009, each = 101), Age = rep(0:100, 50), 
                            Female = na_replace(as.vector(test_data_temp$Female$rate$Female)), 
                            Male = na_replace(as.vector(test_data_temp$Male$rate$Male)), 
                            Total = na_replace(as.vector(test_data_temp$Total$rate$Total)))
  write.table(test_mx_temp, file = "test_mx_temp.txt", row.names = FALSE)
  
  # export exposure into csv
  test_expo_temp = data.frame(Year = rep(1960:2009, each = 101), Age = rep(0:100, 50), 
                              Female = na_replace(as.vector(test_data_temp$Female$pop$Female)), 
                              Male = na_replace(as.vector(test_data_temp$Male$pop$Male)), 
                              Total = na_replace(as.vector(test_data_temp$Total$pop$Total)))
  write.table(test_expo_temp, file = "test_expo_temp.txt", row.names = FALSE)
  
  # assign demogdata
  demogdata_temp = read.demogdata("test_mx_temp.txt", "test_expo_temp.txt", "mortality", OECD_countries[ij], skip = 0, popskip = 0)
  assign(OECD_test[ij], demogdata_temp)
  
  # smoothing
  demogdata_temp_smooth = smooth.demogdata(demogdata_temp)
  assign(OECD_test_smooth[ij], demogdata_temp_smooth)
  
  rm(test_data_temp, test_mx_temp, test_expo_temp, demogdata_temp, demogdata_temp_smooth)
  
  print(ij)
}

### check

par(mfrow = c(2,2))
for(ij in 1:24)
{
  plot(get(OECD_train_smooth[ij]))
}

par(mfrow = c(2,2))
for(ij in 1:24)
{
  plot(get(OECD_test_smooth[ij]))
}

for(ij in 1:length(OECD_selected))
{
  # This loop takes significant time to complete.
  # Use a "save & load" strategy in case of R crashes.
  train_fore_temp = err_fun_forecast_modified(index = ij, state_select = OECD_train, state_select_smooth = OECD_train_smooth)
  assign(OECD_fore_train[ij], train_fore_temp) 
  
  save(train_fore_temp, file = paste(OECD_countries[ij], "_fore_train", ".RData", sep = ""))
  rm(train_fore_temp)
  gc()
  
  print(ij)
}

for(ij in 1:length(OECD_selected))
{
  print(nrow(get(OECD_fore_train[ij])$train_female_AIC))
}

for(ij in 1:length(OECD_selected))
{
  print(nrow(get(OECD_fore_train[ij])$train_male_AIC))
}

for(ij in 1:length(OECD_selected))
{
  # This loop takes significant time to complete.
  # Use a "save & load" strategy in case of R crashes.
  
  test_fore_temp = err_fun_forecast_modified(index = ij, state_select = OECD_test, state_select_smooth = OECD_test_smooth)
  assign(OECD_fore_test[ij], test_fore_temp) 
  
  save(test_fore_temp, file = paste(OECD_countries[ij], "_fore_test", ".RData", sep = ""))
  rm(test_fore_temp)
  gc()
  
  print(ij)
}

for(ij in 1:length(OECD_selected))
{
  print(nrow(get(OECD_fore_test[ij])$train_female_AIC))
}

for(ij in 1:length(OECD_selected))
{
  print(nrow(get(OECD_fore_test[ij])$train_male_AIC))
}

# Combine the computed forecasts into dataframes for future computations

OECD_fore_train_df = c("AUT_fore_train_df", "BEL_fore_train_df", "CZE_fore_train_df", "DNK_fore_train_df", "EST_fore_train_df", 
                    "FIN_fore_train_df", "FRA_fore_train_df", "HUN_fore_train_df", "ISL_fore_train_df", "IRL_fore_train_df", 
                    "ITA_fore_train_df", "JPN_fore_train_df", "LVA_fore_train_df", "LTU_fore_train_df", "LUX_fore_train_df", 
                    "NLD_fore_train_df", "NZL_fore_train_df", "NOR_fore_train_df", "POL_fore_train_df", "ESP_fore_train_df", 
                    "SWE_fore_train_df", "CHE_fore_train_df", "GBR_fore_train_df", "USA_fore_train_df")

OECD_fore_test_df = c("AUT_fore_test_df", "BEL_fore_test_df", "CZE_fore_test_df", "DNK_fore_test_df", "EST_fore_test_df", 
                   "FIN_fore_test_df", "FRA_fore_test_df", "HUN_fore_test_df", "ISL_fore_test_df", "IRL_fore_test_df", 
                   "ITA_fore_test_df", "JPN_fore_test_df", "LVA_fore_test_df", "LTU_fore_test_df", "LUX_fore_test_df", 
                   "NLD_fore_test_df", "NZL_fore_test_df", "NOR_fore_test_df", "POL_fore_test_df", "ESP_fore_test_df", 
                   "SWE_fore_test_df", "CHE_fore_test_df", "GBR_fore_test_df", "USA_fore_test_df")


for(ij in 1:length(OECD_selected))
{
  temp_train_df = temp_test_df = list()
  
  for(ik in 0:100)
  {
    # training
    temp_train_df[[ik+1]] =  combine_df_train_OECD(age = ik, index = ij)
    
    # testing
    temp_test_df[[ik+1]] =  combine_df_test_OECD(age = ik, index = ij)
  }
  
  # Assign the results
  assign(OECD_fore_train_df[ij], temp_train_df)
  assign(OECD_fore_test_df[ij], temp_test_df)
  
  rm(temp_train_df, temp_test_df)
  print(ij)
}

# Compute SHAP values for each OECD country

for(ik in 1:length(OECD_countries))
{
  temp_SHAP = age_specific_SHAP(country_index = ik)
  assign(OECD_SHAP[ik], temp_SHAP)
  
  save(temp_SHAP, file = paste0(OECD_SHAP[ik], ".RData"))
  rm(temp_SHAP)
  
  
  print(paste(OECD_countries[ik], " SHAP Computation Complete", sep = ""))
}

# Compute weighted point forecasts for all OECD countries

OECD_fore_result_base = paste0(OECD_countries, "_fore_result_base")
OECD_fore_result_average = paste0(OECD_countries, "_fore_result_average")
OECD_fore_result_mse_combine = paste0(OECD_countries, "_fore_result_mse_combine")
OECD_fore_result_AIC = paste0(OECD_countries, "_fore_result_AIC")
OECD_fore_result_SHAP_no_truncate = paste0(OECD_countries, "_fore_result_SHAP_no_truncate")
OECD_fore_result_SHAP_5 = paste0(OECD_countries, "_fore_result_SHAP_5")
OECD_fore_result_SHAP_10 = paste0(OECD_countries, "_fore_result_SHAP_10")
OECD_fore_result_SHAP_15 = paste0(OECD_countries, "_fore_result_SHAP_15")
OECD_fore_result_SHAP_20 = paste0(OECD_countries, "_fore_result_SHAP_20")
OECD_fore_result_SHAP_50 = paste0(OECD_countries, "_fore_result_SHAP_50")

for(ij in 1:length(OECD_fore_result_average))
{
  # Average weighting
  temp_fore_result_average = combine_forecast(country_index = ij, method = "Average")
  assign(OECD_fore_result_average[ij], temp_fore_result_average)

  rm(temp_fore_result_average)

  # MSE weighting
  temp_fore_result_mse_combine = combine_forecast(country_index = ij, method = "MSE")
  assign(OECD_fore_result_mse_combine[ij], temp_fore_result_mse_combine)

  rm(temp_fore_result_mse_combine)

  # AIC Weighting
  temp_fore_result_AIC = combine_forecast(country_index = ij, method = "AIC")
  assign(OECD_fore_result_AIC[ij], temp_fore_result_AIC)

  rm(temp_fore_result_AIC)

  # SHAP with alpha = 0.05
  temp_fore_result_SHAP_5 = combine_forecast(country_index = ij, method = "SHAP", alpha = 0.05)
  assign(OECD_fore_result_SHAP_5[ij], temp_fore_result_SHAP_5)

  rm(temp_fore_result_SHAP_5)

  # SHAP with alpha = 0.1
  temp_fore_result_SHAP_10 = combine_forecast(country_index = ij, method = "SHAP", alpha = 0.1)
  assign(OECD_fore_result_SHAP_10[ij], temp_fore_result_SHAP_10)

  rm(temp_fore_result_SHAP_10)

  # SHAP with alpha = 0.15
  temp_fore_result_SHAP_15 = combine_forecast(country_index = ij, method = "SHAP", alpha = 0.15)
  assign(OECD_fore_result_SHAP_15[ij], temp_fore_result_SHAP_15)

  rm(temp_fore_result_SHAP_15)

  # SHAP with alpha = 0.2
  temp_fore_result_SHAP_20 = combine_forecast(country_index = ij, method = "SHAP", alpha = 0.20)
  assign(OECD_fore_result_SHAP_20[ij], temp_fore_result_SHAP_20)

  rm(temp_fore_result_SHAP_20)
  
  # SHAP with alpha = 0.5
  temp_fore_result_SHAP_50 = combine_forecast(country_index = ij, method = "SHAP", alpha = 0.50)
  assign(OECD_fore_result_SHAP_50[ij], temp_fore_result_SHAP_50)

  rm(temp_fore_result_SHAP_50)
  
  print(ij)
}

# SHAP method without truncations

for(ij in 1:length(OECD_countries))
{
  # SHAP without truncations 
  temp_fore_result_SHAP_no_truncate = combine_forecast(country_index = ij, method = "SHAP", alpha = 0)
  assign(OECD_fore_result_SHAP_no_truncate[ij], temp_fore_result_SHAP_no_truncate)
  
  rm(temp_fore_result_SHAP_no_truncate)
  
  print(ij)
}

# Evaluating benchmark models' point forecasts

for(ij in 1:length(OECD_fore_result_base))
{
  # SHAP without truncations 
  temp_fore_result_base = individual_eval(country_index = ij)
  assign(OECD_fore_result_base[ij], temp_fore_result_base)
  
  rm(temp_fore_result_base)
  
  print(ij)
}

# Collect and rearrange all MSE and MAE results 

base_mse_all_female = base_mse_all_male = base_mae_all_female = base_mae_all_male = list()
base_mse_combine = base_mae_combine = list()

for(ij in 1:length(OECD_fore_result_base))
{
  # MSE
  base_mse_all_female[[ij]] = colMeans(get(OECD_fore_result_base[ij])$MSE_female_all, na.rm = TRUE)
  base_mse_all_male[[ij]] = colMeans(get(OECD_fore_result_base[ij])$MSE_male_all, na.rm = TRUE)
  base_mse_all_female[[ij]] = rbind(base_mse_all_female[[ij]], colMeans(base_mse_all_female[[ij]], na.rm = TRUE))
  base_mse_all_male[[ij]] = rbind(base_mse_all_male[[ij]], colMeans(base_mse_all_male[[ij]], na.rm = TRUE))
  colnames(base_mse_all_female[[ij]]) = paste(c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"), "_F", sep = "")
  colnames(base_mse_all_male[[ij]]) = paste(c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"), "_M", sep = "")
  rownames(base_mse_all_female[[ij]])[11] = rownames(base_mse_all_male[[ij]])[11] = "Average"
  
  base_mse_combine[[ij]] = (base_mse_all_female[[ij]] + base_mse_all_male[[ij]])/2
  colnames(base_mse_combine[[ij]]) = paste(c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"), "_combined", sep = "")
  # MAE
  
  base_mae_all_female[[ij]] = colMeans(get(OECD_fore_result_base[ij])$MAE_female_all, na.rm = TRUE)
  base_mae_all_male[[ij]] = colMeans(get(OECD_fore_result_base[ij])$MAE_male_all, na.rm = TRUE)
  base_mae_all_female[[ij]] = rbind(base_mae_all_female[[ij]], colMeans(base_mae_all_female[[ij]], na.rm = TRUE))
  base_mae_all_male[[ij]] = rbind(base_mae_all_male[[ij]], colMeans(base_mae_all_male[[ij]], na.rm = TRUE))
  colnames(base_mae_all_female[[ij]]) = paste(c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"), "_F", sep = "")
  colnames(base_mae_all_male[[ij]]) = paste(c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"), "_M", sep = "")
  rownames(base_mae_all_female[[ij]])[11] = rownames(base_mae_all_male[[ij]])[11] = "Average"
  
  base_mae_combine[[ij]] = (base_mae_all_female[[ij]] + base_mae_all_male[[ij]])/2
  colnames(base_mae_combine[[ij]]) = paste(c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"), "_combined", sep = "")
}

base_mse_rearranged = base_mae_rearranged = list()

for(ij in 1:length(OECD_fore_result_base))
{
  mse_temp = cbind(base_mse_all_female[[ij]], base_mse_all_male[[ij]],  base_mse_combine[[ij]])
  mse_temp = mse_temp[, c(matrix(1:ncol(mse_temp), nrow = 3, byrow = TRUE))]
  base_mse_rearranged[[ij]] = mse_temp
  rm(mse_temp)
  
  mae_temp = cbind(base_mae_all_female[[ij]], base_mae_all_male[[ij]],  base_mae_combine[[ij]])
  mae_temp = mae_temp[, c(matrix(1:ncol(mae_temp), nrow = 3, byrow = TRUE))]
  base_mae_rearranged[[ij]] = mae_temp
  rm(mae_temp)
}

# Export mean MSE and MAE into excel spreadsheets


## MSE 
for(ih in 1:11)
{
  output_by_country = list()
  
  for(country_index in 1:length(OECD_fore_result_base))
  {
    
    output_by_country[[country_index]] = base_mse_rearranged[[country_index]][ih,]
  }
  
  output_dataframe = do.call(rbind, output_by_country)
  rownames(output_dataframe) = OECD_full_name
  
  ## save results into Excel spreadsheet
  require(xlsx)
  
  if(ih < 11)
  {
    sheetname_temp = paste("h = ", ih, sep = "")
  } else 
  {
    sheetname_temp = "Average of all forecasting horizons"
  }
  
  if(ih == 1)
  {
    write.xlsx(output_dataframe, file = "Base_fore_MSE_ALL.xlsx", sheetName = sheetname_temp, row.names = TRUE)
  }
  
  if(ih > 1)
  {
    write.xlsx(output_dataframe, file = "Base_fore_MSE_ALL.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
  }
  
  rm(sheetname_temp)
}


## MAE
for(ih in 1:11)
{
  output_by_country = list()
  
  for(country_index in 1:length(OECD_fore_result_base))
  {
    
    output_by_country[[country_index]] = base_mae_rearranged[[country_index]][ih,]
  }
  
  output_dataframe = do.call(rbind, output_by_country)
  rownames(output_dataframe) = OECD_full_name
  
  ## save results into Excel spreadsheet
  require(xlsx)
  
  if(ih < 11)
  {
    sheetname_temp = paste("h = ", ih, sep = "")
  } else 
  {
    sheetname_temp = "Average of all forecasting horizons"
  }
  
  if(ih == 1)
  {
    write.xlsx(output_dataframe, file = "Base_fore_MAE_ALL.xlsx", sheetName = sheetname_temp, row.names = TRUE)
  }
  
  if(ih > 1)
  {
    write.xlsx(output_dataframe, file = "Base_fore_MAE_ALL.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
  }
  
  rm(sheetname_temp)
}

# Find the best-performing benchmark models

base_fore_top3 = paste0(OECD_countries, "_fore_top3")
base_fore_top5 = paste0(OECD_countries, "_fore_top5")

for(country_index in 1:length(OECD_fore_result_base))
{
  base_mean_mse_female = colMeans(get(OECD_fore_result_base[country_index])$MSE_female_all, na.rm = TRUE)
  base_mean_mse_male = colMeans(get(OECD_fore_result_base[country_index])$MSE_male_all, na.rm = TRUE)
  
  colnames(base_mean_mse_female) = colnames(base_mean_mse_male) = c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr")
  
  OBS_female = OBS_male =
    weighted_fore_female_top3 = weighted_fore_male_top3 =
    weighted_fore_female_top5 = weighted_fore_male_top5 = list()

  MSE_top3_female = MSE_top3_male = MAE_top3_female = MAE_top3_male = 
    MSE_top5_female = MSE_top5_male = MAE_top5_female = MAE_top5_male = list()
  
  method_top3_female = method_top3_male = method_top5_female = method_top5_male = list()

  for(ih in 1:10)
  {
    select_index = find_h_index(ih)
    
    #########
    # female
    #########
    
    # Top 3 best models
    temp_top3_mse_fore_female = sort(base_mean_mse_female[ih,], decreasing = FALSE)[1:3]
    method_top3_female[[ih]] = names(temp_top3_mse_fore_female)
    
    for(ij in 1:101)
    {
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE]
      OBS_female[[ij]] = fore_selected[, "OBS"]
      fore_female[[ij]] = rowMeans(fore_selected[,method_top3_female[[ih]], drop = FALSE])
    }
  
    OBS_female[[ih]] = as.matrix(do.call(rbind, OBS_female))
    weighted_fore_female_top3[[ih]] = as.matrix(do.call(rbind, fore_female))
    
    MSE_top3_female[[ih]] = mse(OBS_female[[ih]], weighted_fore_female_top3[[ih]])
    MAE_top3_female[[ih]] = mae(OBS_female[[ih]], weighted_fore_female_top3[[ih]])

    
    # Top 5 best models
    temp_top5_mse_fore_female = sort(base_mean_mse_female[ih,], decreasing = FALSE)[1:5]
    method_top5_female[[ih]] = names(temp_top5_mse_fore_female)
    
    for(ij in 1:101)
    {
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE]
      fore_female[[ij]] = rowMeans(fore_selected[,method_top5_female[[ih]], drop = FALSE])
    }
    
    weighted_fore_male_top5[[ih]] = as.matrix(do.call(rbind, fore_female))
    
    MSE_top5_female[[ih]] = mse(OBS_female[[ih]], weighted_fore_male_top5[[ih]])
    MAE_top5_female[[ih]] = mae(OBS_female[[ih]], weighted_fore_male_top5[[ih]])
    
    #########
    # male
    #########
    
    # Top 3 best models
    temp_top3_mse_fore_male = sort(base_mean_mse_male[ih,], decreasing = FALSE)[1:3]
    method_top3_male[[ih]] = names(temp_top3_mse_fore_male)
    
    for(ij in 1:101)
    {
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE]
      OBS_male[[ij]] = fore_selected[, "OBS"]
      fore_male[[ij]] = rowMeans(fore_selected[,method_top3_male[[ih]], drop = FALSE])
    }
    
    OBS_male[[ih]] = as.matrix(do.call(rbind, OBS_male))
    weighted_fore_male_top3[[ih]] = as.matrix(do.call(rbind, fore_male))
    
    MSE_top3_male[[ih]] = mse(OBS_male[[ih]], weighted_fore_male_top3[[ih]])
    MAE_top3_male[[ih]] = mae(OBS_male[[ih]], weighted_fore_male_top3[[ih]])
    
    
    # Top 5 best models
    temp_top5_mse_fore_male = sort(base_mean_mse_female[ih,], decreasing = FALSE)[1:5]
    method_top5_male[[ih]] = names(temp_top5_mse_fore_male)
    
    for(ij in 1:101)
    {
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE]
      fore_male[[ij]] = rowMeans(fore_selected[,method_top5_male[[ih]], drop = FALSE])
    }
    
    weighted_fore_male_top5[[ih]] = as.matrix(do.call(rbind, fore_male))
    
    MSE_top5_male[[ih]] = mse(OBS_male[[ih]], weighted_fore_male_top5[[ih]])
    MAE_top5_male[[ih]] = mae(OBS_male[[ih]], weighted_fore_male_top5[[ih]])
  }
  
  list_to_return_top3 = list(OBS_female = OBS_female, OBS_male = OBS_male,
                        weighted_fore_female_top3 = weighted_fore_female_top3, 
                        weighted_fore_male_top3 = weighted_fore_male_top3,
                        MSE_top3_female = MSE_top3_female, 
                        MSE_top3_male = MSE_top3_male,
                        MAE_top3_female = MAE_top3_female,
                        MAE_top3_male = MAE_top3_male,
                        method_top3_female = method_top3_female,
                        method_top3_male = method_top3_male)
  
  list_to_return_top5 = list(OBS_female = OBS_female, OBS_male = OBS_male,
                             weighted_fore_female_top5 = weighted_fore_female_top5,
                             weighted_fore_male_top5 = weighted_fore_male_top5, 
                             MSE_top5_female = MSE_top5_female,
                             MSE_top5_male = MSE_top5_male,
                             MAE_top5_female = MAE_top5_female,
                             MAE_top5_male = MAE_top5_male,
                             method_top5_female = method_top5_female,
                             method_top5_male = method_top5_male)
  
  assign(base_fore_top3[[country_index]], list_to_return_top3)
  assign(base_fore_top5[[country_index]], list_to_return_top5)
  
  print(country_index)
}

###########################
# Summarisation of results
###########################

######
# MSE
######

OECD_fore_MSE_all = paste0(OECD_countries, "_fore_MSE_all")

for(ij in 1:length(OECD_fore_MSE_all))
{
  ## Averaging over 101 ages
  
  # Average weighting
  average_mean_female = colMeans(get(OECD_fore_result_average[ij])$MSE_female_all, na.rm = TRUE)
  average_mean_male = colMeans(get(OECD_fore_result_average[ij])$MSE_male_all, na.rm = TRUE)
  
  # MSE weighting
  mse_mean_female = colMeans(get(OECD_fore_result_mse_combine[ij])$MSE_female_all, na.rm = TRUE)
  mse_mean_male = colMeans(get(OECD_fore_result_mse_combine[ij])$MSE_male_all, na.rm = TRUE)
  
  # AIC Weighting
  AIC_mean_female = colMeans(get(OECD_fore_result_AIC[ij])$MSE_female_all, na.rm = TRUE)
  AIC_mean_male = colMeans(get(OECD_fore_result_AIC[ij])$MSE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0
  SHAP_no_truncate_mean_female = colMeans(get(OECD_fore_result_SHAP_no_truncate[ij])$MSE_female_all, na.rm = TRUE)
  SHAP_no_truncate_mean_male = colMeans(get(OECD_fore_result_SHAP_no_truncate[ij])$MSE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.05
  SHAP_5_mean_female = colMeans(get(OECD_fore_result_SHAP_5[ij])$MSE_female_all, na.rm = TRUE)
  SHAP_5_mean_male = colMeans(get(OECD_fore_result_SHAP_5[ij])$MSE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.1
  SHAP_10_mean_female = colMeans(get(OECD_fore_result_SHAP_10[ij])$MSE_female_all, na.rm = TRUE)
  SHAP_10_mean_male = colMeans(get(OECD_fore_result_SHAP_10[ij])$MSE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.15
  SHAP_15_mean_female = colMeans(get(OECD_fore_result_SHAP_15[ij])$MSE_female_all, na.rm = TRUE)
  SHAP_15_mean_male = colMeans(get(OECD_fore_result_SHAP_15[ij])$MSE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.2
  SHAP_20_mean_female = colMeans(get(OECD_fore_result_SHAP_20[ij])$MSE_female_all, na.rm = TRUE)
  SHAP_20_mean_male = colMeans(get(OECD_fore_result_SHAP_20[ij])$MSE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.5
  SHAP_50_mean_female = colMeans(get(OECD_fore_result_SHAP_50[ij])$MSE_female_all, na.rm = TRUE)
  SHAP_50_mean_male = colMeans(get(OECD_fore_result_SHAP_50[ij])$MSE_male_all, na.rm = TRUE)
  
  ## Combine all results into dataframes
  fore_mse_all_female = data.frame(Average = average_mean_female, 
                                   Average_top3 = unlist(get(base_fore_top3[[ij]])$MSE_top3_female),
                                   Average_top5 = unlist(get(base_fore_top5[[ij]])$MSE_top5_female),
                                   MSE = mse_mean_female, AIC = AIC_mean_female,
                                   SHAP_no_truncate = SHAP_no_truncate_mean_female,
                                   SHAP_5 = SHAP_5_mean_female, SHAP_10 = SHAP_10_mean_female, 
                                   SHAP_15 = SHAP_15_mean_female, SHAP_20 = SHAP_20_mean_female,
                                   SHAP_50 = SHAP_50_mean_female)
  fore_mse_all_female = rbind(fore_mse_all_female, apply(fore_mse_all_female, 2, mean))
  rownames(fore_mse_all_female)[11] = "Mean"
  
  fore_mse_all_male = data.frame(Average = average_mean_male, 
                                 Average_top3 = unlist(get(base_fore_top3[[ij]])$MSE_top3_male),
                                 Average_top5 = unlist(get(base_fore_top5[[ij]])$MSE_top5_male),
                                 MSE = mse_mean_male, AIC = AIC_mean_male,
                                 SHAP_no_truncate = SHAP_no_truncate_mean_male,
                                 SHAP_5 = SHAP_5_mean_male, SHAP_10 = SHAP_10_mean_male, 
                                 SHAP_15 = SHAP_15_mean_male, SHAP_20 = SHAP_20_mean_male,
                                 SHAP_50 = SHAP_50_mean_male)
  fore_mse_all_male = rbind(fore_mse_all_male, apply(fore_mse_all_male, 2, mean))
  rownames(fore_mse_all_male)[11] = "Mean"
  
  fore_mse_all = list(female = fore_mse_all_female, male = fore_mse_all_male)
  
  assign(OECD_fore_MSE_all[ij], fore_mse_all)
  
  rm(fore_mse_all, fore_mse_all_female, fore_mse_all_male)
  
  print(ij)
}



######
# MAE
######

OECD_fore_MAE_all = paste0(OECD_countries, "_fore_MAE_all")

for(ij in 1:length(OECD_fore_MAE_all))
{
  ## Averaging over 101 ages
  
  # Average weighting
  average_mean_female = colMeans(get(OECD_fore_result_average[ij])$MAE_female_all, na.rm = TRUE)
  average_mean_male = colMeans(get(OECD_fore_result_average[ij])$MAE_male_all, na.rm = TRUE)
  
  # MAE weighting
  mse_mean_female = colMeans(get(OECD_fore_result_mse_combine[ij])$MAE_female_all, na.rm = TRUE)
  mse_mean_male = colMeans(get(OECD_fore_result_mse_combine[ij])$MAE_male_all, na.rm = TRUE)
  
  # AIC Weighting
  AIC_mean_female = colMeans(get(OECD_fore_result_AIC[ij])$MAE_female_all, na.rm = TRUE)
  AIC_mean_male = colMeans(get(OECD_fore_result_AIC[ij])$MAE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0
  SHAP_no_truncate_mean_female = colMeans(get(OECD_fore_result_SHAP_no_truncate[ij])$MAE_female_all, na.rm = TRUE)
  SHAP_no_truncate_mean_male = colMeans(get(OECD_fore_result_SHAP_no_truncate[ij])$MAE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.05
  SHAP_5_mean_female = colMeans(get(OECD_fore_result_SHAP_5[ij])$MAE_female_all, na.rm = TRUE)
  SHAP_5_mean_male = colMeans(get(OECD_fore_result_SHAP_5[ij])$MAE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.1
  SHAP_10_mean_female = colMeans(get(OECD_fore_result_SHAP_10[ij])$MAE_female_all, na.rm = TRUE)
  SHAP_10_mean_male = colMeans(get(OECD_fore_result_SHAP_10[ij])$MAE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.15
  SHAP_15_mean_female = colMeans(get(OECD_fore_result_SHAP_15[ij])$MAE_female_all, na.rm = TRUE)
  SHAP_15_mean_male = colMeans(get(OECD_fore_result_SHAP_15[ij])$MAE_male_all, na.rm = TRUE)
  
  # SHAP with alpha = 0.2
  SHAP_20_mean_female = colMeans(get(OECD_fore_result_SHAP_20[ij])$MAE_female_all, na.rm = TRUE)
  SHAP_20_mean_male = colMeans(get(OECD_fore_result_SHAP_20[ij])$MAE_male_all, na.rm = TRUE)
  
  #SHAP with alpha = 0.5
  SHAP_50_mean_female = colMeans(get(OECD_fore_result_SHAP_50[ij])$MAE_female_all, na.rm = TRUE)
  SHAP_50_mean_male = colMeans(get(OECD_fore_result_SHAP_50[ij])$MAE_male_all, na.rm = TRUE)
  
  ## Combine all results into dataframes
  fore_mae_all_female = data.frame(Average = average_mean_female, 
                                   Average_top3 = unlist(get(base_fore_top3[[ij]])$MAE_top3_female),
                                   Average_top5 = unlist(get(base_fore_top5[[ij]])$MAE_top5_female),
                                   MSE = mse_mean_female, AIC = AIC_mean_female,
                                   SHAP_no_truncate = SHAP_no_truncate_mean_female,
                                   SHAP_5 = SHAP_5_mean_female, SHAP_10 = SHAP_10_mean_female, 
                                   SHAP_15 = SHAP_15_mean_female, SHAP_20 = SHAP_20_mean_female,
                                   SHAP_50 = SHAP_50_mean_female)
  fore_mae_all_female = rbind(fore_mae_all_female, apply(fore_mae_all_female, 2, mean))
  rownames(fore_mae_all_female)[11] = "Mean"
  
  fore_mae_all_male = data.frame(Average = average_mean_male, 
                                 Average_top3 = unlist(get(base_fore_top3[[ij]])$MAE_top3_male),
                                 Average_top5 = unlist(get(base_fore_top5[[ij]])$MAE_top5_male),
                                 MSE = mse_mean_male, AIC = AIC_mean_male,
                                 SHAP_no_truncate = SHAP_no_truncate_mean_male,
                                 SHAP_5 = SHAP_5_mean_male, SHAP_10 = SHAP_10_mean_male, 
                                 SHAP_15 = SHAP_15_mean_male, SHAP_20 = SHAP_20_mean_male,
                                 SHAP_50 = SHAP_50_mean_male)
  fore_mae_all_male = rbind(fore_mae_all_male, apply(fore_mae_all_male, 2, mean))
  rownames(fore_mae_all_male)[11] = "Mean"
  
  fore_mae_all = list(female = fore_mae_all_female, male = fore_mae_all_male)
  
  assign(OECD_fore_MAE_all[ij], fore_mae_all)
  
  rm(fore_mae_all, fore_mae_all_female, fore_mae_all_male)
  
  print(ij)
}



#################
# Export results
#################

# Formate the results into separate tables by forecasting horizon h

## MSE ##
for(ih in 1:11)
{
  output_by_country = list()
  
  for(country_index in 1:length(OECD_fore_MSE_all))
  {
    # Average
    Average_F = get(OECD_fore_MSE_all[country_index])$female[ih,"Average"]
    Average_M = get(OECD_fore_MSE_all[country_index])$male[ih,"Average"]
    Average_combined = (Average_F + Average_M)/2
    
    # Average Top 3
    Average_top3_F = get(OECD_fore_MSE_all[country_index])$female[ih,"Average_top3"]
    Average_top3_M = get(OECD_fore_MSE_all[country_index])$male[ih,"Average_top3"]
    Average_top3_combined = (Average_top3_F + Average_top3_M)/2
    
    # Average Top 5
    Average_top5_F = get(OECD_fore_MSE_all[country_index])$female[ih,"Average_top5"]
    Average_top5_M = get(OECD_fore_MSE_all[country_index])$male[ih,"Average_top5"]
    Average_top5_combined = (Average_top5_F + Average_top5_M)/2
    
    # MSE
    MSE_F = get(OECD_fore_MSE_all[country_index])$female[ih,"MSE"]
    MSE_M = get(OECD_fore_MSE_all[country_index])$male[ih,"MSE"]
    MSE_combined = (MSE_F + MSE_M)/2
    
    # AIC
    AIC_F = get(OECD_fore_MSE_all[country_index])$female[ih,"AIC"]
    AIC_M = get(OECD_fore_MSE_all[country_index])$male[ih,"AIC"]
    AIC_combined = (AIC_F + AIC_M)/2

    # SHAP no truncation
    SHAP_no_truncate_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_no_truncate"]
    SHAP_no_truncate_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_no_truncate"]
    SHAP_no_truncate_combined = (SHAP_no_truncate_F + SHAP_no_truncate_M)/2
    
    # SHAP_5
    SHAP_5_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_5"]
    SHAP_5_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_5"]
    SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
    
    # SHAP_10
    SHAP_10_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_10"]
    SHAP_10_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_10"]
    SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
    
    # SHAP_15
    SHAP_15_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_15"]
    SHAP_15_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_15"]
    SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
    
    # SHAP_20
    SHAP_20_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_20"]
    SHAP_20_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_20"]
    SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
    
    # SHAP_500
    SHAP_50_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_50"]
    SHAP_50_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_50"]
    SHAP_50_combined = (SHAP_50_F + SHAP_50_M)/2
    
    output_by_country[[country_index]] = cbind(Average_F, Average_M, Average_combined,
                                               Average_top3_F, Average_top3_M, Average_top3_combined,
                                               Average_top5_F, Average_top5_M, Average_top5_combined,
                                               MSE_F, MSE_M, MSE_combined,
                                               AIC_F, AIC_M, AIC_combined,
                                               SHAP_no_truncate_F, SHAP_no_truncate_M, SHAP_no_truncate_combined,
                                               SHAP_5_F, SHAP_5_M, SHAP_5_combined,
                                               SHAP_10_F, SHAP_10_M, SHAP_10_combined,
                                               SHAP_15_F, SHAP_15_M, SHAP_15_combined,
                                               SHAP_20_F, SHAP_20_M, SHAP_20_combined,
                                               SHAP_50_F, SHAP_50_M, SHAP_50_combined)
  }
  
  
  output_dataframe = do.call(rbind.data.frame, output_by_country)
  rownames(output_dataframe) = OECD_full_name
  
  ## save results into Excel spreadsheet
  require(xlsx)
  
  if(ih < 11)
  {
    sheetname_temp = paste("h = ", ih, sep = "")
  } else 
    {
      sheetname_temp = "Average of all forecasting horizons"
    }
  
  if(ih == 1)
  {
    write.xlsx(output_dataframe, file = "OECD_MSE_All.xlsx", sheetName = sheetname_temp, row.names = TRUE)
  }
  
  if(ih > 1)
  {
    write.xlsx(output_dataframe, file = "OECD_MSE_All.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
  }
  
  rm(sheetname_temp)
}


## MAE ##
for(ih in 1:11)
{
  output_by_country = list()
  
  for(country_index in 1:length(OECD_fore_MAE_all))
  {
    # Average
    Average_F = get(OECD_fore_MAE_all[country_index])$female[ih,"Average"]
    Average_M = get(OECD_fore_MAE_all[country_index])$male[ih,"Average"]
    Average_combined = (Average_F + Average_M)/2
    
    # Average Top 3
    Average_top3_F = get(OECD_fore_MAE_all[country_index])$female[ih,"Average_top3"]
    Average_top3_M = get(OECD_fore_MAE_all[country_index])$male[ih,"Average_top3"]
    Average_top3_combined = (Average_top3_F + Average_top3_M)/2
    
    # Average Top 5
    Average_top5_F = get(OECD_fore_MAE_all[country_index])$female[ih,"Average_top5"]
    Average_top5_M = get(OECD_fore_MAE_all[country_index])$male[ih,"Average_top5"]
    Average_top5_combined = (Average_top5_F + Average_top5_M)/2
    
    # MSE
    MSE_F = get(OECD_fore_MAE_all[country_index])$female[ih,"MSE"]
    MSE_M = get(OECD_fore_MAE_all[country_index])$male[ih,"MSE"]
    MSE_combined = (MSE_F + MSE_M)/2
    
    # AIC
    AIC_F = get(OECD_fore_MAE_all[country_index])$female[ih,"AIC"]
    AIC_M = get(OECD_fore_MAE_all[country_index])$male[ih,"AIC"]
    AIC_combined = (AIC_F + AIC_M)/2
    
    # SHAP no truncation
    SHAP_no_truncate_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_no_truncate"]
    SHAP_no_truncate_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_no_truncate"]
    SHAP_no_truncate_combined = (SHAP_no_truncate_F + SHAP_no_truncate_M)/2
    
    # SHAP_5
    SHAP_5_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_5"]
    SHAP_5_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_5"]
    SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
    
    # SHAP_10
    SHAP_10_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_10"]
    SHAP_10_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_10"]
    SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
    
    # SHAP_15
    SHAP_15_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_15"]
    SHAP_15_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_15"]
    SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
    
    # SHAP_20
    SHAP_20_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_20"]
    SHAP_20_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_20"]
    SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
    
    # SHAP_500
    SHAP_50_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_50"]
    SHAP_50_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_50"]
    SHAP_50_combined = (SHAP_50_F + SHAP_50_M)/2
    
    output_by_country[[country_index]] = cbind(Average_F, Average_M, Average_combined,
                                               Average_top3_F, Average_top3_M, Average_top3_combined,
                                               Average_top5_F, Average_top5_M, Average_top5_combined,
                                               MSE_F, MSE_M, MSE_combined,
                                               AIC_F, AIC_M, AIC_combined,
                                               SHAP_no_truncate_F, SHAP_no_truncate_M, SHAP_no_truncate_combined,
                                               SHAP_5_F, SHAP_5_M, SHAP_5_combined,
                                               SHAP_10_F, SHAP_10_M, SHAP_10_combined,
                                               SHAP_15_F, SHAP_15_M, SHAP_15_combined,
                                               SHAP_20_F, SHAP_20_M, SHAP_20_combined,
                                               SHAP_50_F, SHAP_50_M, SHAP_50_combined)
  }
  
  
  output_dataframe = do.call(rbind.data.frame, output_by_country)
  rownames(output_dataframe) = OECD_full_name
  
  ## save results into Excel spreadsheet
  require(xlsx)
  
  if(ih < 11)
  {
    sheetname_temp = paste("h = ", ih, sep = "")
  } else 
  {
    sheetname_temp = "Average of all forecasting horizons"
  }
  
  if(ih == 1)
  {
    write.xlsx(output_dataframe, file = "OECD_MAE_All.xlsx", sheetName = sheetname_temp, row.names = TRUE)
  }
  
  if(ih > 1)
  {
    write.xlsx(output_dataframe, file = "OECD_MAE_All.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
  }
  
  rm(sheetname_temp)
}

# TOP 3 and 5 best-performing methods


OECD_top3_methods_female = paste0(OECD_countries, "_top3_methods_female")
OECD_top3_methods_male = paste0(OECD_countries, "_top3_methods_male")
OECD_top5_methods_female = paste0(OECD_countries, "_top5_methods_female")
OECD_top5_methods_male = paste0(OECD_countries, "_top5_methods_male")

for(ih in 1:10)
{
  output_by_country_top_3_female = output_by_country_top_3_male = 
    output_by_country_top_5_female = output_by_country_top_5_male = list()
  
  for(country_index in 1:length(OECD_countries))
  {
    output_by_country_top_3_female[[country_index]] = get(base_fore_top3[country_index])$method_top3_female[[ih]]
    output_by_country_top_3_male[[country_index]] = get(base_fore_top3[country_index])$method_top3_male[[ih]]
    
    output_by_country_top_5_female[[country_index]] = get(base_fore_top5[country_index])$method_top5_female[[ih]]
    output_by_country_top_5_male[[country_index]] = get(base_fore_top5[country_index])$method_top5_male[[ih]]
  }
  
  output_by_country_top_3_female_df = do.call(rbind, output_by_country_top_3_female)
  output_by_country_top_3_male_df = do.call(rbind, output_by_country_top_3_male)
  rownames(output_by_country_top_3_female_df) = rownames(output_by_country_top_3_male_df) = OECD_full_name
  colnames(output_by_country_top_3_female_df) = colnames(output_by_country_top_3_male_df) = c("1st_Model", "2nd_Model", "3rd_Model")
  
  output_by_country_top_5_female_df = do.call(rbind, output_by_country_top_5_female)
  output_by_country_top_5_male_df = do.call(rbind, output_by_country_top_5_male)
  rownames(output_by_country_top_5_female_df) = rownames(output_by_country_top_5_male_df) = OECD_full_name
  colnames(output_by_country_top_5_female_df) = colnames(output_by_country_top_5_male_df) = c("1st_Model", "2nd_Model", "3rd_Model", "4th_Model", "5th_Model")
  
  
  # ## save results into Excel spreadsheet
  require(xlsx)
  
  sheetname_temp = paste("h = ", ih, sep = "")
  
  if(ih == 1)
  {
    write.xlsx(output_by_country_top_3_female_df, file = "top_3_female.xlsx", sheetName = sheetname_temp, row.names = TRUE)
    write.xlsx(output_by_country_top_3_male_df, file = "top_3_male.xlsx", sheetName = sheetname_temp, row.names = TRUE)
    write.xlsx(output_by_country_top_5_female_df, file = "top_5_female.xlsx", sheetName = sheetname_temp, row.names = TRUE)
    write.xlsx(output_by_country_top_5_male_df, file = "top_5_male.xlsx", sheetName = sheetname_temp, row.names = TRUE)
  }

  if(ih > 1)
  {
    write.xlsx(output_by_country_top_3_female_df, file = "top_3_female.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
    write.xlsx(output_by_country_top_3_male_df, file = "top_3_male.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
    write.xlsx(output_by_country_top_5_female_df, file = "top_5_female.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
    write.xlsx(output_by_country_top_5_male_df, file = "top_5_male.xlsx", sheetName = sheetname_temp, row.names = TRUE, append = TRUE )
  }

  rm(sheetname_temp)
}

# Export benchmark forecasts

age_all = 0:100


for(country_index in 1:length(OECD_fore_result_base))
{
  file_name_temp = paste(OECD_countries[country_index], "_base_forecasts.xlsx", sep = "")
  
  for(ih in 1:10)
  {
    select_index = find_h_index(ih)
    
    fore_selected_female = fore_selected_male = data.frame(rep(character(), 17))
    for(ij in 1:101)
    {
      # female
      fore_selected_female = rbind(fore_selected_female, data.frame(cbind(Age = age_all[ij], get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE])))

      # male
      fore_selected_male = rbind(fore_selected_male, data.frame(cbind(Age = age_all[ij], get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE])))
    }
    
    ## export results
    
    sheetname_temp_female = paste("h=", ih, "_F", sep = "")
    sheetname_temp_male = paste("h=", ih, "_M", sep = "")
    
    if(ih == 1)
    {
      wb = createWorkbook()
      
      # female
      addWorksheet(wb, sheetname_temp_female)
      writeData(wb, sheetname_temp_female, fore_selected_female)
      
      # male
      addWorksheet(wb, sheetname_temp_male)
      writeData(wb, sheetname_temp_male, fore_selected_male)
    }
    
    if(ih > 1)
    {
      # female
      addWorksheet(wb, sheetname_temp_female)
      writeData(wb, sheetname_temp_female, fore_selected_female)
      
      # male
      addWorksheet(wb, sheetname_temp_male)
      writeData(wb, sheetname_temp_male, fore_selected_male)
    }
    
    rm(sheetname_temp_female, sheetname_temp_male)
  }
  
  
  saveWorkbook(wb, file_name_temp, overwrite = TRUE)
  
  rm(file_name_temp)
  
  print(country_index)
}

## MSE

base_fore_mse_female = paste0(OECD_countries, "_base_mse_female")
base_fore_mse_male = paste0(OECD_countries, "_base_mse_male")

for(country_index in 1:length(OECD_countries))
{
  base_fore_temp_female = base_fore_temp_male = list()
  
  for(ih in 1:10)
  {
    select_index = find_h_index(ih)
    
    base_fore_temp_female[[ih]] = base_fore_temp_male[[ih]] = data.frame(rep(character(), 17))
    for(ij in 1:101)
    {
      # female
      base_fore_temp_female[[ih]] = rbind(base_fore_temp_female[[ih]], data.frame(cbind(Age = age_all[ij], get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE])))
      
      # male
      base_fore_temp_male[[ih]] = rbind(base_fore_temp_male[[ih]], data.frame(cbind(Age = age_all[ij], get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE])))
    }
  }
  
  assign(base_fore_mse_female[country_index], base_fore_temp_female)
  assign(base_fore_mse_male[country_index], base_fore_temp_male)
  
  print(country_index)
}

## MAE

base_fore_mae_female = paste0(OECD_countries, "_base_mae_female")
base_fore_mae_male = paste0(OECD_countries, "_base_mae_male")

for(country_index in 1:length(OECD_countries))
{
  base_fore_temp_female = base_fore_temp_male = list()
  
  for(ih in 1:10)
  {
    select_index = find_h_index(ih)
    
    base_fore_temp_female[[ih]] = base_fore_temp_male[[ih]] = data.frame(rep(character(), 17))
    for(ij in 1:101)
    {
      # female
      base_fore_temp_female[[ih]] = rbind(base_fore_temp_female[[ih]], data.frame(cbind(Age = age_all[ij], get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE])))
      
      # male
      base_fore_temp_male[[ih]] = rbind(base_fore_temp_male[[ih]], data.frame(cbind(Age = age_all[ij], get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE])))
    }
  }
  
  assign(base_fore_mse_female[country_index], base_fore_temp_female)
  assign(base_fore_mse_male[country_index], base_fore_temp_male)
  
  print(country_index)
}

####################################
# Replicate the tables in our paper
####################################

# MSE for ih = 1, 6, 10

or(ih in c(1,6,10))
{
  output_by_country = list()
  for(country_index in 1:length(OECD_fore_MSE_all))
  {
    # Average
    Average_F = get(OECD_fore_MSE_all[country_index])$female[ih,"Average"]
    Average_M = get(OECD_fore_MSE_all[country_index])$male[ih,"Average"]
    # Average_combined = (Average_F + Average_M)/2
    
    # AIC
    AIC_F = get(OECD_fore_MSE_all[country_index])$female[ih,"AIC"]
    AIC_M = get(OECD_fore_MSE_all[country_index])$male[ih,"AIC"]
    # AIC_combined = (AIC_F + AIC_M)/2
    
    # SHAP no truncation
    SHAP_no_truncate_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_no_truncate"]
    SHAP_no_truncate_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_no_truncate"]
    # SHAP_no_truncate_combined = (SHAP_no_truncate_F + SHAP_no_truncate_M)/2
    
    # # SHAP_5
    # SHAP_5_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_5"]
    # SHAP_5_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_5"]
    # SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
    # 
    # # SHAP_10
    # SHAP_10_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_10"]
    # SHAP_10_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_10"]
    # SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
    # 
    # # SHAP_15
    # SHAP_15_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_15"]
    # SHAP_15_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_15"]
    # SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
    # 
    # # SHAP_20
    # SHAP_20_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_20"]
    # SHAP_20_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_20"]
    # SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
    
    # SHAP_50
    SHAP_50_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_50"]
    SHAP_50_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_50"]
    # SHAP_50_combined = (SHAP_50_F + SHAP_50_M)/2
    
    output_by_country[[country_index]] = cbind(Average_F, Average_M,
                                               AIC_F, AIC_M,
                                               SHAP_no_truncate_F, SHAP_no_truncate_M,
                                               SHAP_50_F, SHAP_50_M)
  }
  
  output_latex_temp = do.call(rbind.data.frame, output_by_country)
  output_latex_temp = rbind(output_latex_temp, apply(output_latex_temp, 2, mean))
  rownames(output_latex_temp) = c(OECD_full_name, "Mean")
  
  output_table_name = paste("output_latex_mse_h", ih, sep = "") 
  assign(output_table_name, output_latex_temp)
  
  rm(output_latex_temp, output_table_name)
}

#  MAE for ih = 1, 6, 10

for(ih in 1:10)
{
  output_by_country = list()
  for(country_index in 1:length(OECD_fore_MAE_all))
  {
    # Average
    Average_F = get(OECD_fore_MAE_all[country_index])$female[ih,"Average"]
    Average_M = get(OECD_fore_MAE_all[country_index])$male[ih,"Average"]
    # Average_combined = (Average_F + Average_M)/2
    
    # AIC
    AIC_F = get(OECD_fore_MAE_all[country_index])$female[ih,"AIC"]
    AIC_M = get(OECD_fore_MAE_all[country_index])$male[ih,"AIC"]
    # AIC_combined = (AIC_F + AIC_M)/2
    
    # SHAP no truncation
    SHAP_no_truncate_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_no_truncate"]
    SHAP_no_truncate_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_no_truncate"]
    # SHAP_no_truncate_combined = (SHAP_no_truncate_F + SHAP_no_truncate_M)/2
    
    # SHAP_5
    # SHAP_5_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_5"]
    # SHAP_5_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_5"]
    # SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
    # 
    # # SHAP_10
    # SHAP_10_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_10"]
    # SHAP_10_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_10"]
    # SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
    # 
    # # SHAP_15
    # SHAP_15_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_15"]
    # SHAP_15_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_15"]
    # SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
    # 
    # # SHAP_20
    # SHAP_20_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_20"]
    # SHAP_20_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_20"]
    # SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
    
    # SHAP_50
    SHAP_50_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_50"]
    SHAP_50_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_50"]
    # SHAP_50_combined = (SHAP_50_F + SHAP_50_M)/2
    
    output_by_country[[country_index]] = cbind(Average_F, Average_M,
                                               AIC_F, AIC_M,
                                               SHAP_no_truncate_F, SHAP_no_truncate_M,
                                               SHAP_50_F, SHAP_50_M)
  }
  
  output_latex_temp = do.call(rbind.data.frame, output_by_country)
  output_latex_temp = rbind(output_latex_temp, apply(output_latex_temp, 2, mean))
  rownames(output_latex_temp) = c(OECD_full_name, "Mean")
  
  output_table_name = paste("output_latex_mae_h", ih, sep = "") 
  assign(output_table_name, output_latex_temp)
  
  rm(output_latex_temp, output_table_name)
}

# Use grid search to find the alpha that minimises the point forecasting error

alpha_search_range = seq(0.01, 0.99, by = 0.01)

OECD_grid_search_all = paste0(OECD_countries, "_fore_result_SHAP_grid_search")

for(ij in 1:length(OECD_countries))
{
  temp_grid_search_all = paste0(OECD_countries[ij], "_fore_grid_search_alpha_", alpha_search_range)
  
  for(ik in 1:99)
  {
    temp_fore_result_SHAP = combine_forecast(country_index = ij, method = "SHAP", alpha = alpha_search_range[ik])
    assign(temp_grid_search_all[ik], temp_fore_result_SHAP)
    rm(temp_fore_result_SHAP)
  }
  
  assign(OECD_grid_search_all[ij], temp_grid_search_all)
  rm(temp_grid_search_all)
  
  print(ij)
}

# Summary of results into a matrix

positive_colmeans = function(mat)
{
  mat[mat<=0] <- NA
  
  return(colMeans(mat, na.rm = TRUE))     
}


OECD_grid_search_MSE_female = paste0(OECD_countries, "_fore_result_SHAP_grid_search_MSE_female") 
OECD_grid_search_MSE_male   = paste0(OECD_countries, "_fore_result_SHAP_grid_search_MSE_male") 
OECD_grid_search_MAE_female = paste0(OECD_countries, "_fore_result_SHAP_grid_search_MAE_female") 
OECD_grid_search_MAE_male   = paste0(OECD_countries, "_fore_result_SHAP_grid_search_MAE_male") 

for(ij in 1:length(OECD_countries))
{
  temp_mse_female = temp_mse_male = temp_mae_female = temp_mae_male = matrix(NA, nrow = 99, ncol = 10)
  
  colnames(temp_mse_female) = colnames(temp_mse_male) = 
    colnames(temp_mae_female) = colnames(temp_mae_male) = paste0("h = ", 1:10)
  
  for(ik in 1:99)
  {
    temp_result = get(get(OECD_grid_search_all[ij])[ik])
    
    temp_mse_female[ik,] = colMeans(temp_result$MSE_female_all, na.rm = TRUE)
    temp_mse_male[ik,]   = colMeans(temp_result$MSE_male_all, na.rm = TRUE)
    temp_mae_female[ik,] = colMeans(temp_result$MAE_female_all, na.rm = TRUE)
    temp_mae_male[ik,]   = colMeans(temp_result$MAE_male_all, na.rm = TRUE)
    
    rm(temp_result)
  }
    
  assign(OECD_grid_search_MSE_female[ij], temp_mse_female)
  assign(OECD_grid_search_MSE_male[ij], temp_mse_male)
  assign(OECD_grid_search_MAE_female[ij], temp_mae_female)
  assign(OECD_grid_search_MAE_male[ij], temp_mae_male)
  
  rm(temp_mse_female)
  rm(temp_mse_male)
  rm(temp_mae_female)
  rm(temp_mae_male)
  
  print(ij)
}


## Selecting country-specific and h-specific alpha parameters

OECD_country_and_h_alpha_MSE_female = paste0(OECD_countries, "_country_and_h_alpha_MSE_female") 
OECD_country_and_h_alpha_MSE_male   = paste0(OECD_countries, "_country_and_h_alpha_MSE_male") 
OECD_country_and_h_alpha_MAE_female = paste0(OECD_countries, "_country_and_h_alpha_MAE_female") 
OECD_country_and_h_alpha_MAE_male   = paste0(OECD_countries, "_country_and_h_alpha_MAE_male") 


for(ij in 1:length(OECD_countries))
{
  assign(OECD_country_and_h_alpha_MSE_female[ij], apply(get(OECD_grid_search_MSE_female[ij]), 2, which.min))
  assign(OECD_country_and_h_alpha_MSE_male[ij], apply(get(OECD_grid_search_MSE_male[ij]), 2, which.min))
  assign(OECD_country_and_h_alpha_MAE_female[ij], apply(get(OECD_grid_search_MAE_female[ij]), 2, which.min))
  assign(OECD_country_and_h_alpha_MAE_male[ij], apply(get(OECD_grid_search_MAE_male[ij]), 2, which.min))
}


## Selecting country-specific alpha parameters

OECD_country_alpha_MSE_female = paste0(OECD_countries, "_country_alpha_MSE_female") 
OECD_country_alpha_MSE_male   = paste0(OECD_countries, "_country_alpha_MSE_male") 
OECD_country_alpha_MAE_female = paste0(OECD_countries, "_country_alpha_MAE_female") 
OECD_country_alpha_MAE_male   = paste0(OECD_countries, "_country_alpha_MAE_male") 


for(ij in 1:length(OECD_countries))
{
  assign(OECD_country_alpha_MSE_female[ij], which.min(apply(get(OECD_grid_search_MSE_female[ij]), 1, mean)))
  assign(OECD_country_alpha_MSE_male[ij], which.min(apply(get(OECD_grid_search_MSE_male[ij]), 1, mean)))
  assign(OECD_country_alpha_MAE_female[ij], which.min(apply(get(OECD_grid_search_MAE_female[ij]), 1, mean)))
  assign(OECD_country_alpha_MAE_male[ij], which.min(apply(get(OECD_grid_search_MAE_male[ij]), 1, mean)))
}

# Create a table to compare different alpha values


compare_alpha_female_h1 = compare_alpha_male_h1 = matrix(NA, nrow = 24, ncol = 6)
rownames(compare_alpha_female_h1) = rownames(compare_alpha_male_h1) = OECD_full_name
colnames(compare_alpha_female_h1) = colnames(compare_alpha_male_h1) = c("Optimal", "0.05", "0.1", "0.15", "0.2", "0.5")

for(ij in 1:24)
{
  # female
  compare_alpha_female_h1[ij, 1] = get(OECD_grid_search_MSE_female[ij])[get(OECD_country_and_h_alpha_MSE_female[ij])[1],1]
  compare_alpha_female_h1[ij, 2] = get(OECD_grid_search_MSE_female[ij])[5,1]
  compare_alpha_female_h1[ij, 3] = get(OECD_grid_search_MSE_female[ij])[10,1]
  compare_alpha_female_h1[ij, 4] = get(OECD_grid_search_MSE_female[ij])[15,1]
  compare_alpha_female_h1[ij, 5] = get(OECD_grid_search_MSE_female[ij])[20,1]
  compare_alpha_female_h1[ij, 6] = get(OECD_grid_search_MSE_female[ij])[50,1]
  
  # male
  compare_alpha_male_h1[ij, 1] = get(OECD_grid_search_MSE_male[ij])[get(OECD_country_and_h_alpha_MSE_male[ij])[1],1]
  compare_alpha_male_h1[ij, 2] = get(OECD_grid_search_MSE_male[ij])[5,1]
  compare_alpha_male_h1[ij, 3] = get(OECD_grid_search_MSE_male[ij])[10,1]
  compare_alpha_male_h1[ij, 4] = get(OECD_grid_search_MSE_male[ij])[15,1]
  compare_alpha_male_h1[ij, 5] = get(OECD_grid_search_MSE_male[ij])[20,1]
  compare_alpha_male_h1[ij, 6] = get(OECD_grid_search_MSE_male[ij])[50,1]
}


# For h = 1, alpha = 0.05, 0.1, 0.15, 0.2

ih = 1
output_by_country = list()

for(country_index in 1:length(OECD_fore_MSE_all))
{

  # SHAP_5
  SHAP_5_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_5"]
  SHAP_5_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_5"]
  # SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
  # 
  # # SHAP_10
  SHAP_10_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_10"]
  SHAP_10_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_10"]
  # SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
  # 
  # # SHAP_15
  SHAP_15_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_15"]
  SHAP_15_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_15"]
  # SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
  # 
  # # SHAP_20
  SHAP_20_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_20"]
  SHAP_20_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_20"]
  # SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
  
  output_by_country[[country_index]] = cbind(SHAP_5_F,  SHAP_5_M,
                                             SHAP_10_F, SHAP_10_M,
                                             SHAP_15_F, SHAP_15_M,
                                             SHAP_20_F, SHAP_20_M)
}

MSE_alpha_h_1 = do.call(rbind.data.frame, output_by_country)
rownames(MSE_alpha_h_1) = OECD_full_name