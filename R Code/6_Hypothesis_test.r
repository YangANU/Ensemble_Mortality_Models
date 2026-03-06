#######################
# Diebold-Mariano Test
#######################

## This R script conducts the Diebold-Mariano test for evaluating point forecasts.

# Load required R packages

library(forecast)
library(demography)
library(ggplot2)
library(tidyverse)

fore_error_test <- function(country_index)
{
  # find errors of forecasts
  
  fore_female_average = fore_male_average = 
    fore_female_AIC = fore_male_AIC = 
    fore_female_shapley = fore_male_shapley = list()
  
  error_female_average = error_male_average = 
    error_female_AIC = error_male_AIC = 
    error_female_shapley = error_male_shapley = list()
  
  for(h_horizon in 1:10)
  {
    fore_female_average[[h_horizon]] = 
      fore_female_AIC[[h_horizon]] = 
      fore_female_shapley[[h_horizon]] = matrix(NA, nrow = 101, ncol = (11-h_horizon))
    
    fore_male_average[[h_horizon]] = 
      fore_male_AIC[[h_horizon]] = 
      fore_male_shapley[[h_horizon]] = matrix(NA, nrow = 101, ncol = (11-h_horizon))
    
    error_female_average[[h_horizon]] = 
      error_female_AIC[[h_horizon]] = 
      error_female_shapley[[h_horizon]] = matrix(NA, nrow = 101, ncol = (11-h_horizon))
    
    error_male_average[[h_horizon]] = 
      error_male_AIC[[h_horizon]] = 
      error_male_shapley[[h_horizon]] = matrix(NA, nrow = 101, ncol = (11-h_horizon))
    

    for(ik in 1:101)
    {
      ### Extract forecasts
      ## female
      fore_female_average[[h_horizon]][ik,] = log(get(OECD_fore_result_average[country_index])$weighted_fore_female_all[[ik]][[h_horizon]])
      fore_female_AIC[[h_horizon]][ik,] = log(get(OECD_fore_result_AIC[country_index])$weighted_fore_female_all[[ik]][[h_horizon]])
      fore_female_shapley[[h_horizon]][ik,] = log(get(OECD_fore_result_SHAP_no_truncate[country_index])$weighted_fore_female_all[[ik]][[h_horizon]])
      
      ## male
      fore_male_average[[h_horizon]][ik,] = log(get(OECD_fore_result_average[country_index])$weighted_fore_male_all[[ik]][[h_horizon]])
      fore_male_AIC[[h_horizon]][ik,] = log(get(OECD_fore_result_AIC[country_index])$weighted_fore_male_all[[ik]][[h_horizon]])
      fore_male_shapley[[h_horizon]][ik,] = log(get(OECD_fore_result_SHAP_no_truncate[country_index])$weighted_fore_male_all[[ik]][[h_horizon]])
      
      
      ### Compute point forecast errors
      ## female
      obs_female = log(extract.ages(extract.years(get(OECD_all_data[country_index]), (2009+h_horizon):2019), ages = 0:100, combine.upper = FALSE)$rate$female[ik,])
      error_female_average[[h_horizon]][ik,] = obs_female - fore_female_average[[h_horizon]][ik,]
      error_female_AIC[[h_horizon]][ik,] = obs_female - fore_female_AIC[[h_horizon]][ik,]
      error_female_shapley[[h_horizon]][ik,] = obs_female - fore_female_shapley[[h_horizon]][ik,]
      
      ## male
      obs_male = log(extract.ages(extract.years(get(OECD_all_data[country_index]), (2009+h_horizon):2019), ages = 0:100, combine.upper = FALSE)$rate$male[ik,])
      error_male_average[[h_horizon]][ik,] = obs_male - fore_male_average[[h_horizon]][ik,]
      error_male_AIC[[h_horizon]][ik,] = obs_male - fore_male_AIC[[h_horizon]][ik,]
      error_male_shapley[[h_horizon]][ik,] = obs_male - fore_male_shapley[[h_horizon]][ik,]
      
    }
  }
  
  # conduct Diebold-Mariano test
  DM_shapley_average_female = DM_shapley_AIC_female = 
    DM_shapley_average_male = DM_shapley_AIC_male = rep(0, 101)
  
  for(ik in 1:101)
  {
    ## female
    temp_error_female_average = unlist(lapply(error_female_average, `[`,ik,))
    temp_error_female_AIC = unlist(lapply(error_female_AIC, `[`,ik,))
    temp_error_female_shapley = unlist(lapply(error_female_shapley, `[`,ik,))
    
    index_average_female = is.finite(temp_error_female_average)
    index_AIC_female = is.finite(temp_error_female_AIC)
    index_shapley_female = is.finite(temp_error_female_shapley)
    
    index_shapley_average_female = ifelse(index_average_female*index_shapley_female == 0, FALSE, TRUE)
    if(sum(index_shapley_average_female) > 1)
    {
      DM_shapley_average_female[ik] = dm.test(e1 = temp_error_female_average[index_shapley_average_female], 
                                              e2 = temp_error_female_shapley[index_shapley_average_female], alternative = "greater")$p.value
    } else 
    {
      DM_shapley_average_female[ik] = 1
    }

    index_shapley_AIC_female = ifelse(index_AIC_female*index_shapley_female == 0, FALSE, TRUE)
    if(sum(index_shapley_AIC_female) > 1)
    {
      DM_shapley_AIC_female[ik] = dm.test(e1 = temp_error_female_AIC[index_shapley_AIC_female], 
                                          e2 = temp_error_female_shapley[index_shapley_AIC_female], alternative = "greater")$p.value
    }
    
    ## male
    temp_error_male_average = unlist(lapply(error_male_average, `[`,ik,))
    temp_error_male_AIC = unlist(lapply(error_male_AIC, `[`,ik,))
    temp_error_male_shapley = unlist(lapply(error_male_shapley, `[`,ik,))
    
    index_average_male = is.finite(temp_error_male_average)
    index_AIC_male = is.finite(temp_error_male_AIC)
    index_shapley_male = is.finite(temp_error_male_shapley)
    
    index_shapley_average_male = ifelse(index_average_male*index_shapley_male == 0, FALSE, TRUE)
    if(sum(index_shapley_average_male) > 1)
    {
      DM_shapley_average_male[ik] = dm.test(e1 = temp_error_male_average[index_shapley_average_male], 
                                              e2 = temp_error_male_shapley[index_shapley_average_male], alternative = "greater")$p.value
    } else 
    {
      DM_shapley_average_male[ik] = 1
    }
    
    index_shapley_AIC_male = ifelse(index_AIC_male*index_shapley_male == 0, FALSE, TRUE)
    if(sum(index_shapley_AIC_male) > 1)
    {
      DM_shapley_AIC_male[ik] = dm.test(e1 = temp_error_male_AIC[index_shapley_AIC_male], 
                                          e2 = temp_error_male_shapley[index_shapley_AIC_male], alternative = "greater")$p.value
    }
  }
  

  return(list(DM_shapley_average_female = DM_shapley_average_female,
              DM_shapley_AIC_female = DM_shapley_AIC_female,
              DM_shapley_average_male = DM_shapley_average_male,
              DM_shapley_AIC_male = DM_shapley_AIC_male,
              fore_female_average = fore_female_average,
              fore_male_average = fore_male_average,
              fore_female_AIC = fore_female_AIC,
              fore_male_AIC = fore_male_AIC,
              fore_female_shapley = fore_female_shapley,
              fore_male_shapley = fore_male_shapley,
              error_female_average = error_female_average,
              error_male_average = error_male_average,
              error_female_AIC = error_female_AIC,
              error_male_AIC = error_male_AIC,
              error_female_shapley = error_female_shapley,
              error_male_shapley = error_male_shapley))
}


OECD_DM_shapley_average_female = paste0(OECD_countries, "_DM_shapley_average_female")
OECD_DM_shapley_AIC_female = paste0(OECD_countries, "_DM_shapley_AIC_female")
OECD_DM_shapley_average_male = paste0(OECD_countries, "_DM_shapley_average_male")
OECD_DM_shapley_AIC_male = paste0(OECD_countries, "_DM_shapley_AIC_male")


for(ij in 1:length(OECD_countries))
{
  temp_DM = fore_error_test(ij)
  
  assign(OECD_DM_shapley_average_female[ij], temp_DM$DM_shapley_average_female)
  assign(OECD_DM_shapley_AIC_female[ij], temp_DM$DM_shapley_AIC_female)

  assign(OECD_DM_shapley_average_male[ij], temp_DM$DM_shapley_average_male)
  assign(OECD_DM_shapley_AIC_male[ij], temp_DM$DM_shapley_AIC_male)
  
  rm(temp_DM)
  print(ij)
}

# Create heatmap to visualise the results

DM_p_matrix = matrix(NA, nrow = 24, ncol = 4)
for(ij in 1:length(OECD_countries))
{
  DM_p_matrix[ij,1] = sum(get(OECD_DM_shapley_average_female[ij]) < 0.05)/101
  DM_p_matrix[ij,2] = sum(get(OECD_DM_shapley_average_male[ij]) < 0.05)/101
  DM_p_matrix[ij,3] = sum(get(OECD_DM_shapley_AIC_female[ij]) < 0.05)/101
  DM_p_matrix[ij,4] = sum(get(OECD_DM_shapley_AIC_male[ij]) < 0.05)/101
}

colnames(DM_p_matrix) = c("SHAP_SMA_Female", "SHAP_SMA_Male",
                          "SHAP_AIC_Female", "SHAP_AIC_Male")
DM_df = data.frame(Country = OECD_full_name, DM_p_matrix)


dat_dm =
  DM_df %>%
  as_tibble() %>%
  pivot_longer(!Country, names_to = "DM", values_to = "ratio") %>%
  mutate(
    Country = factor(Country, ordered=TRUE, levels = OECD_full_name),
    DM = factor(DM, ordered = TRUE, levels = c("SHAP_SMA_Female", "SHAP_SMA_Male",
                                               "SHAP_AIC_Female", "SHAP_AIC_Male"))
  )

heatmap_dm = ggplot(dat_dm, aes(DM, Country)) +
  geom_tile(aes(fill = ratio)) +
  geom_text(aes(label = round(ratio, 4))) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#0C7BDC", high = "#FFC20A") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
heatmap_dm

