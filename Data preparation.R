#################################
# Data preparation for Shiny App
#################################

library(ftsa)

load("Shiny_data.RData")

country_names = c("Austria", "Belgium", "Czech", "Denmark", "Estonia",
                  "Finland", "France", "Hungary", "Iceland", "Ireland",
                  "Italy", "Japan", "Latvia", "Lithuania", "Luxemburg",
                  "Netherlands", "New_Zealand", "Norway", "Poland",
                  "Spain", "Sweden", "Switzerland", "UK", "USA") 

OECD_countries = c("AUT", "BEL", "CZE", "DNK", "EST", "FIN",
                   "FRATNP", "HUN", "ISL", "IRL", "ITA", "JPN",
                   "LVA", "LTU", "LUX", "NLD", "NZL_NP", "NOR",
                   "POL", "ESP", "SWE", "CHE", "GBR_NP", "USA")

OECD_fore_result_average = paste0(OECD_countries, "_fore_result_average")
OECD_fore_result_AIC = paste0(OECD_countries, "_fore_result_AIC")
OECD_fore_result_SHAP_no_truncate = paste0(OECD_countries, "_fore_result_SHAP_no_truncate")
OECD_fore_result_SHAP_5 = paste0(OECD_countries, "_fore_result_SHAP_5")
OECD_fore_result_SHAP_10 = paste0(OECD_countries, "_fore_result_SHAP_10")
OECD_fore_result_SHAP_15 = paste0(OECD_countries, "_fore_result_SHAP_15")
OECD_fore_result_SHAP_20 = paste0(OECD_countries, "_fore_result_SHAP_20")
OECD_fore_result_SHAP_50 = paste0(OECD_countries, "_fore_result_SHAP_50")

OECD_obs_female = paste0(OECD_countries, "_obs_female")
OECD_obs_male = paste0(OECD_countries, "_obs_male")

OECD_average_fore_female = paste0(OECD_countries, "_average_fore_female")
OECD_average_fore_male   = paste0(OECD_countries, "_average_fore_male")
OECD_AIC_fore_female     = paste0(OECD_countries, "_AIC_fore_female")
OECD_AIC_fore_male       = paste0(OECD_countries, "_AIC_fore_male")
OECD_SHAP_fore_female    = paste0(OECD_countries, "_SHAP_fore_female")
OECD_SHAP_fore_male      = paste0(OECD_countries, "_SHAP_fore_male")
OECD_SHAP_5_fore_female  = paste0(OECD_countries, "_SHAP_5_fore_female")
OECD_SHAP_5_fore_male    = paste0(OECD_countries, "_SHAP_5_fore_male")
OECD_SHAP_10_fore_female = paste0(OECD_countries, "_SHAP_10_fore_female")
OECD_SHAP_10_fore_male   = paste0(OECD_countries, "_SHAP_10_fore_male")
OECD_SHAP_15_fore_female = paste0(OECD_countries, "_SHAP_15_fore_female")
OECD_SHAP_15_fore_male   = paste0(OECD_countries, "_SHAP_15_fore_male")
OECD_SHAP_20_fore_female = paste0(OECD_countries, "_SHAP_20_fore_female")
OECD_SHAP_20_fore_male   = paste0(OECD_countries, "_SHAP_20_fore_male")
OECD_SHAP_50_fore_female = paste0(OECD_countries, "_SHAP_50_fore_female")
OECD_SHAP_50_fore_male   = paste0(OECD_countries, "_SHAP_50_fore_male")

## Separate forecasts into list

for(ik in 1:24)
{
  obs_all_female = obs_all_male = list()
  fore_average_female = fore_average_male = list()
  fore_AIC_female = fore_AIC_male = list()
  fore_SHAP_female = fore_SHAP_male = list()
  fore_SHAP_5_female = fore_SHAP_5_male = list()
  fore_SHAP_10_female = fore_SHAP_10_male = list()
  fore_SHAP_15_female = fore_SHAP_15_male = list()
  fore_SHAP_20_female = fore_SHAP_20_male = list()
  fore_SHAP_50_female = fore_SHAP_50_male = list()
  
  for(ih in 1:10)
  {
    # Observation
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_average[ik])$OBS_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_average[ik])$OBS_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    obs_all_female[[ih]] = fts_female_temp
    obs_all_male[[ih]] = fts_male_temp
    
    # Average
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_average[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_average[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_average_female[[ih]] = fts_female_temp
    fore_average_male[[ih]] = fts_male_temp
    
    # AIC
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_AIC[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_AIC[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_AIC_female[[ih]] = fts_female_temp
    fore_AIC_male[[ih]] = fts_male_temp
    
    # Shapley
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_SHAP_no_truncate[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_SHAP_no_truncate[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_SHAP_female[[ih]] = fts_female_temp
    fore_SHAP_male[[ih]] = fts_male_temp
    
    # Shapley with alpha 5
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_SHAP_5[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_SHAP_5[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_SHAP_5_female[[ih]] = fts_female_temp
    fore_SHAP_5_male[[ih]] = fts_male_temp
    
    
    # Shapley with alpha 10
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_SHAP_10[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_SHAP_10[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_SHAP_10_female[[ih]] = fts_female_temp
    fore_SHAP_10_male[[ih]] = fts_male_temp
    
    
    # Shapley with alpha 15
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_SHAP_15[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_SHAP_15[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_SHAP_15_female[[ih]] = fts_female_temp
    fore_SHAP_15_male[[ih]] = fts_male_temp
    
    # Shapley with alpha 20
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_SHAP_20[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_SHAP_20[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_SHAP_20_female[[ih]] = fts_female_temp
    fore_SHAP_20_male[[ih]] = fts_male_temp
    
    # Shapley with alpha 50
    obs_female_temp = obs_male_temp = matrix(NA, nrow = 101, ncol = (11-ih))
    for(ij in 1:101)
    {
      obs_female_temp[ij,] = get(OECD_fore_result_SHAP_50[ik])$weighted_fore_female_all[[ij]][[ih]]
      obs_male_temp[ij,] = get(OECD_fore_result_SHAP_50[ik])$weighted_fore_male_all[[ij]][[ih]]
    }
    colnames(obs_female_temp) = colnames(obs_male_temp) = ((2009+ih):2019)
    
    fts_female_temp =  fts(x = 0:100, y = log(obs_female_temp), xname = "Age", yname = "Mortality Rate")
    fts_male_temp =  fts(x = 0:100, y = log(obs_male_temp), xname = "Age", yname = "Mortality Rate")
    
    fore_SHAP_50_female[[ih]] = fts_female_temp
    fore_SHAP_50_male[[ih]] = fts_male_temp
  }
  
  
  assign(OECD_obs_female[ik], obs_all_female)
  assign(OECD_obs_male[ik], obs_all_male)
  assign(OECD_average_fore_female[ik], fore_average_female)
  assign(OECD_average_fore_male[ik], fore_average_male)
  assign(OECD_AIC_fore_female[ik], fore_AIC_female)
  assign(OECD_AIC_fore_male[ik], fore_AIC_male)
  assign(OECD_SHAP_fore_female[ik], fore_SHAP_female)
  assign(OECD_SHAP_fore_male[ik], fore_SHAP_male)
  assign(OECD_SHAP_5_fore_female[ik], fore_SHAP_5_female)
  assign(OECD_SHAP_5_fore_male[ik], fore_SHAP_5_male)
  assign(OECD_SHAP_10_fore_female[ik], fore_SHAP_10_female)
  assign(OECD_SHAP_10_fore_male[ik], fore_SHAP_10_male)
  assign(OECD_SHAP_15_fore_female[ik], fore_SHAP_15_female)
  assign(OECD_SHAP_15_fore_male[ik], fore_SHAP_15_male)
  assign(OECD_SHAP_20_fore_female[ik], fore_SHAP_20_female)
  assign(OECD_SHAP_20_fore_male[ik], fore_SHAP_20_male)
  assign(OECD_SHAP_50_fore_female[ik], fore_SHAP_50_female)
  assign(OECD_SHAP_50_fore_male[ik], fore_SHAP_50_male)
}


## organise mse & mae

OECD_fore_MSE_all = paste0(OECD_countries, "_fore_MSE_all")
OECD_fore_MAE_all = paste0(OECD_countries, "_fore_MAE_all")

all_country_MSE = paste0("all_country_MSE_h_", 1:10)
all_country_MAE = paste0("all_country_MAE_h_", 1:10)

SHAP_5_MSE = paste0("SHAP_5_MSE_h_", 1:10)
SHAP_10_MSE = paste0("SHAP_10_MSE_h_", 1:10)
SHAP_15_MSE = paste0("SHAP_15_MSE_h_", 1:10)
SHAP_20_MSE = paste0("SHAP_20_MSE_h_", 1:10)
SHAP_50_MSE = paste0("SHAP_50_MSE_h_", 1:10)

SHAP_5_MAE = paste0("SHAP_5_MAE_h_", 1:10)
SHAP_10_MAE = paste0("SHAP_10_MAE_h_", 1:10)
SHAP_15_MAE = paste0("SHAP_15_MAE_h_", 1:10)
SHAP_20_MAE = paste0("SHAP_20_MAE_h_", 1:10)
SHAP_50_MAE = paste0("SHAP_50_MAE_h_", 1:10)

for(ih in 1:10)
{
  ## MSE
  MSE_temp = SHAP_5_MSE_temp = SHAP_10_MSE_temp = SHAP_15_MSE_temp = 
    SHAP_20_MSE_temp = SHAP_50_MSE_temp = list()
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
    
    # SHAP_5
    SHAP_5_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_5"]
    SHAP_5_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_5"]
    # SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
    
    # SHAP_10
    SHAP_10_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_10"]
    SHAP_10_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_10"]
    # SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
    
    # SHAP_15
    SHAP_15_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_15"]
    SHAP_15_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_15"]
    # SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
    
    # SHAP_20
    SHAP_20_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_20"]
    SHAP_20_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_20"]
    # SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
    
    # SHAP_500
    SHAP_50_F = get(OECD_fore_MSE_all[country_index])$female[ih,"SHAP_50"]
    SHAP_50_M = get(OECD_fore_MSE_all[country_index])$male[ih,"SHAP_50"]
    # SHAP_50_combined = (SHAP_50_F + SHAP_50_M)/2
    
    # MSE_temp[[country_index]] = cbind(Average_F, Average_M, Average_combined,
    #                                            AIC_F, AIC_M, AIC_combined,
    #                                            SHAP_no_truncate_F, SHAP_no_truncate_M, SHAP_no_truncate_combined)
    # 
    # SHAP_5_MSE_temp[[country_index]] = cbind(SHAP_5_F, SHAP_5_M, SHAP_5_combined)
    # SHAP_10_MSE_temp[[country_index]] = cbind(SHAP_10_F, SHAP_10_M, SHAP_10_combined)
    # SHAP_15_MSE_temp[[country_index]] = cbind(SHAP_15_F, SHAP_15_M, SHAP_15_combined)
    # SHAP_20_MSE_temp[[country_index]] = cbind(SHAP_20_F, SHAP_20_M, SHAP_20_combined)
    # SHAP_50_MSE_temp[[country_index]] = cbind(SHAP_50_F, SHAP_50_M, SHAP_50_combined)
    
    MSE_temp[[country_index]] = cbind(Average_F, Average_M,
                                      AIC_F, AIC_M,
                                      SHAP_no_truncate_F, SHAP_no_truncate_M)
    
    SHAP_5_MSE_temp[[country_index]] = cbind(SHAP_5_F, SHAP_5_M)
    SHAP_10_MSE_temp[[country_index]] = cbind(SHAP_10_F, SHAP_10_M)
    SHAP_15_MSE_temp[[country_index]] = cbind(SHAP_15_F, SHAP_15_M)
    SHAP_20_MSE_temp[[country_index]] = cbind(SHAP_20_F, SHAP_20_M)
    SHAP_50_MSE_temp[[country_index]] = cbind(SHAP_50_F, SHAP_50_M)
  }
  
  # first 3 methods
  output_latex_temp = do.call(rbind.data.frame, MSE_temp)
  rownames(output_latex_temp) = country_names
  colnames(output_latex_temp) = c("Average F", "Average M", "AIC F", "AIC M", "Shapley F", "Shapley M")
  assign(all_country_MSE[ih], output_latex_temp)
  
  # SHAP with truncations
  SHAP_5_latex_temp = do.call(rbind.data.frame, SHAP_5_MSE_temp)
  rownames(SHAP_5_latex_temp) = country_names
  colnames(SHAP_5_latex_temp) = c("Shapley (5%) F", "Shapley (5%) M")
  assign(SHAP_5_MSE[ih], SHAP_5_latex_temp)
  
  SHAP_10_latex_temp = do.call(rbind.data.frame, SHAP_10_MSE_temp)
  rownames(SHAP_10_latex_temp) = country_names
  colnames(SHAP_10_latex_temp) = c("Shapley (10%) F", "Shapley (10%) M")
  assign(SHAP_10_MSE[ih], SHAP_10_latex_temp)
  
  SHAP_15_latex_temp = do.call(rbind.data.frame, SHAP_15_MSE_temp)
  rownames(SHAP_15_latex_temp) = country_names
  colnames(SHAP_15_latex_temp) = c("Shapley (15%) F", "Shapley (15%) M")
  assign(SHAP_15_MSE[ih], SHAP_15_latex_temp)
  
  SHAP_20_latex_temp = do.call(rbind.data.frame, SHAP_20_MSE_temp)
  rownames(SHAP_20_latex_temp) = country_names
  colnames(SHAP_20_latex_temp) = c("Shapley (20%) F", "Shapley (20%) M")
  assign(SHAP_20_MSE[ih], SHAP_20_latex_temp)

  SHAP_50_latex_temp = do.call(rbind.data.frame, SHAP_50_MSE_temp)
  rownames(SHAP_50_latex_temp) = country_names
  colnames(SHAP_50_latex_temp) = c("Shapley (50%) F", "Shapley (50%) M")
  assign(SHAP_50_MSE[ih], SHAP_50_latex_temp)
  
  rm(output_latex_temp, SHAP_5_latex_temp, SHAP_15_latex_temp, SHAP_20_latex_temp, SHAP_50_latex_temp)
  
  ## MAE
  MAE_temp = SHAP_5_MAE_temp = SHAP_10_MAE_temp = SHAP_15_MAE_temp = 
    SHAP_20_MAE_temp = SHAP_50_MAE_temp = list()
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
    SHAP_5_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_5"]
    SHAP_5_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_5"]
    # SHAP_5_combined = (SHAP_5_F + SHAP_5_M)/2
    
    # SHAP_10
    SHAP_10_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_10"]
    SHAP_10_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_10"]
    # SHAP_10_combined = (SHAP_10_F + SHAP_10_M)/2
    
    # SHAP_15
    SHAP_15_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_15"]
    SHAP_15_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_15"]
    # SHAP_15_combined = (SHAP_15_F + SHAP_15_M)/2
    
    # SHAP_20
    SHAP_20_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_20"]
    SHAP_20_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_20"]
    # SHAP_20_combined = (SHAP_20_F + SHAP_20_M)/2
    
    # SHAP_500
    SHAP_50_F = get(OECD_fore_MAE_all[country_index])$female[ih,"SHAP_50"]
    SHAP_50_M = get(OECD_fore_MAE_all[country_index])$male[ih,"SHAP_50"]
    # SHAP_50_combined = (SHAP_50_F + SHAP_50_M)/2
    
    # MAE_temp[[country_index]] = cbind(Average_F, Average_M, Average_combined,
    #                                   AIC_F, AIC_M, AIC_combined,
    #                                   SHAP_no_truncate_F, SHAP_no_truncate_M, SHAP_no_truncate_combined)
    # 
    # SHAP_5_MAE_temp[[country_index]] = cbind(SHAP_5_F, SHAP_5_M, SHAP_5_combined)
    # SHAP_10_MAE_temp[[country_index]] = cbind(SHAP_10_F, SHAP_10_M, SHAP_10_combined)
    # SHAP_15_MAE_temp[[country_index]] = cbind(SHAP_15_F, SHAP_15_M, SHAP_15_combined)
    # SHAP_20_MAE_temp[[country_index]] = cbind(SHAP_20_F, SHAP_20_M, SHAP_20_combined)
    # SHAP_50_MAE_temp[[country_index]] = cbind(SHAP_50_F, SHAP_50_M, SHAP_50_combined)
    
    MAE_temp[[country_index]] = cbind(Average_F, Average_M,
                                      AIC_F, AIC_M,
                                      SHAP_no_truncate_F, SHAP_no_truncate_M)
    
    SHAP_5_MAE_temp[[country_index]] = cbind(SHAP_5_F, SHAP_5_M)
    SHAP_10_MAE_temp[[country_index]] = cbind(SHAP_10_F, SHAP_10_M)
    SHAP_15_MAE_temp[[country_index]] = cbind(SHAP_15_F, SHAP_15_M)
    SHAP_20_MAE_temp[[country_index]] = cbind(SHAP_20_F, SHAP_20_M)
    SHAP_50_MAE_temp[[country_index]] = cbind(SHAP_50_F, SHAP_50_M)
  }
  
  # first 3 methods
  output_latex_temp = do.call(rbind.data.frame, MAE_temp)
  rownames(output_latex_temp) = country_names
  colnames(output_latex_temp) = c("Average F", "Average M", "AIC F", "AIC M", "Shapley F", "Shapley M")
  assign(all_country_MAE[ih], output_latex_temp)
  
  # SHAP with truncations
  SHAP_5_latex_temp = do.call(rbind.data.frame, SHAP_5_MAE_temp)
  rownames(SHAP_5_latex_temp) = country_names
  colnames(SHAP_5_latex_temp) = c("Shapley (5%) F", "Shapley (5%) M")
  assign(SHAP_5_MAE[ih], SHAP_5_latex_temp)
  
  SHAP_10_latex_temp = do.call(rbind.data.frame, SHAP_10_MAE_temp)
  rownames(SHAP_10_latex_temp) = country_names
  colnames(SHAP_10_latex_temp) = c("Shapley (10%) F", "Shapley (10%) M")
  assign(SHAP_10_MAE[ih], SHAP_10_latex_temp)
  
  SHAP_15_latex_temp = do.call(rbind.data.frame, SHAP_15_MAE_temp)
  rownames(SHAP_15_latex_temp) = country_names
  colnames(SHAP_15_latex_temp) = c("Shapley (15%) F", "Shapley (15%) M")
  assign(SHAP_15_MAE[ih], SHAP_15_latex_temp)
  
  SHAP_20_latex_temp = do.call(rbind.data.frame, SHAP_20_MAE_temp)
  rownames(SHAP_20_latex_temp) = country_names
  colnames(SHAP_20_latex_temp) = c("Shapley (20%) F", "Shapley (20%) M")
  assign(SHAP_20_MAE[ih], SHAP_20_latex_temp)
  
  SHAP_50_latex_temp = do.call(rbind.data.frame, SHAP_50_MAE_temp)
  rownames(SHAP_50_latex_temp) = country_names
  colnames(SHAP_50_latex_temp) = c("Shapley (50%) F", "Shapley (50%) M")
  assign(SHAP_50_MAE[ih], SHAP_50_latex_temp)
  
  rm(output_latex_temp, SHAP_5_latex_temp, SHAP_15_latex_temp, SHAP_20_latex_temp, SHAP_50_latex_temp)
}


