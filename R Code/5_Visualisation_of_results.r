###########################
# Visualisation of results
###########################

## This script presents R commands for creating visulisations used in our paper.
## Run the "3_Point_forecasts_computation.r" and "4_Interval_forecasts_computation.r" before generating the plots.

# Load required R packages

library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)
library(RColorBrewer)

# A heatmap of point forecast errors for a selection of models

all_base_method = c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr")

OECD_SHAP_50_Selected_models_mat_female = OECD_SHAP_50_Selected_models_mat_male = matrix(NA, nrow = length(OECD_countries), ncol = length(all_base_method))
rownames(OECD_SHAP_50_Selected_models_mat_female) = rownames(OECD_SHAP_50_Selected_models_mat_male) = OECD_full_name
colnames(OECD_SHAP_50_Selected_models_mat_female) = colnames(OECD_SHAP_50_Selected_models_mat_male) = all_base_method

for(ij in 1:length(OECD_countries))
{
  # female
  method_select_female = table(unlist(get(OECD_fore_result_SHAP_50[ij])$SHAP_selected_method_female))
  OECD_SHAP_50_Selected_models_mat_female[ij,] = method_select_female[all_base_method]
  
  # male
  method_select_male = table(unlist(get(OECD_fore_result_SHAP_50[ij])$SHAP_selected_method_male))
  OECD_SHAP_50_Selected_models_mat_male[ij,] = method_select_male[all_base_method]
  
  rm(method_select_female, method_select_male)
  print(ij)
}

OECD_SHAP_50_Selected_models_mat_female[is.na(OECD_SHAP_50_Selected_models_mat_female)] = 0
OECD_SHAP_50_Selected_models_mat_male[is.na(OECD_SHAP_50_Selected_models_mat_male)] = 0

# simplify method names
colnames(OECD_SHAP_50_Selected_models_mat_female) = colnames(OECD_SHAP_50_Selected_models_mat_male)= paste("M", 1:15, sep = "")

# add a column of country names
OECD_SHAP_50_Selected_models_mat_female = data.frame(Country = OECD_full_name, OECD_SHAP_50_Selected_models_mat_female)
OECD_SHAP_50_Selected_models_mat_male = data.frame(Country = OECD_full_name, OECD_SHAP_50_Selected_models_mat_male)

## Female
dat_female =
  OECD_SHAP_50_Selected_models_mat_female %>%
  as_tibble() %>%
  pivot_longer(!Country, names_to = "Model", values_to = "count") %>%
  mutate(
    Country = factor(Country, ordered=TRUE, levels = OECD_full_name),
    Model = factor(Model, ordered = TRUE, levels = paste("M", 1:15, sep = ""))
  )

heatmap_female = ggplot(dat_female, aes(Model, Country)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#0C7BDC", high = "#FFC20A") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())	
heatmap_female

## Male
dat_male =
  OECD_SHAP_50_Selected_models_mat_male %>%
  as_tibble() %>%
  pivot_longer(!Country, names_to = "Model", values_to = "count") %>%
  mutate(
    Country = factor(Country, ordered=TRUE, levels = OECD_full_name),
    Model = factor(Model, ordered = TRUE, levels = paste("M", 1:15, sep = ""))
  )

heatmap_male = ggplot(dat_male, aes(Model, Country)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count)) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#0C7BDC", high = "#FFC20A") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
heatmap_male		

# A heatmap of country-specific alpha 

country_h_specific_alpha_mat_female = country_h_specific_alpha_mat_male = data.frame(cbind(Country = OECD_full_name, matrix(NA, nrow = 24, ncol = 10)))

for(ij in 1:24)
{
  country_h_specific_alpha_mat_female[ij,2:11] = alpha_search_range[get(OECD_country_and_h_alpha_MSE_female[ij])]
  country_h_specific_alpha_mat_male[ij,2:11] = alpha_search_range[get(OECD_country_and_h_alpha_MSE_male[ij])]
}

colnames(country_h_specific_alpha_mat_female)[2:11] = colnames(country_h_specific_alpha_mat_male)[2:11] = paste("h=", 1:10, sep = "")


## female

dat_female_alpha =
  country_h_specific_alpha_mat_female %>%
  as_tibble() %>%
  pivot_longer(!Country, names_to = "Alpha", values_to = "para_val") %>%
  mutate(
    Country = factor(Country, ordered = TRUE, levels = OECD_full_name),
    Forecast = factor(Alpha, ordered = TRUE, levels = paste("h=", 1:10, sep = ""))
  )

heatmap_female_alpha = ggplot(dat_female_alpha, aes(Forecast, Country)) +
  geom_tile(aes(fill = as.numeric(para_val))) +
  geom_text(aes(label = para_val)) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#0C7BDC", high = "#FFC20A") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

heatmap_female_alpha

## male

dat_male_alpha =
  country_h_specific_alpha_mat_male %>%
  as_tibble() %>%
  pivot_longer(!Country, names_to = "Alpha", values_to = "para_val") %>%
  mutate(
    Country = factor(Country, ordered = TRUE, levels = OECD_full_name),
    Forecast = factor(Alpha, ordered = TRUE, levels = paste("h=", 1:10, sep = ""))
  )

heatmap_male_alpha = ggplot(dat_male_alpha, aes(Forecast, Country)) +
  geom_tile(aes(fill = as.numeric(para_val))) +
  geom_text(aes(label = para_val)) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#0C7BDC", high = "#FFC20A") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

heatmap_male_alpha

# Visualisation of Country-Age MSE decomposition

mat_fore_mse_h1_female = paste0("mat_fore_mse_h1_female_", OECD_countries, sep = "")
mat_fore_mse_h1_male = paste0("mat_fore_mse_h1_male_", OECD_countries, sep = "")

for(ij in 1:length(OECD_countries))
{
  mat_fore_mse_h1_female_temp = mat_fore_mse_h1_male_temp = matrix(NA, nrow = 8, ncol = 101)
  rownames(mat_fore_mse_h1_female_temp) = rownames(mat_fore_mse_h1_male_temp) = c("SMA", "AIC", "SHAP", "SHAP5",
                                                                                  "SHAP10", "SHAP15", "SHAP20", "SHAP50")
  colnames(mat_fore_mse_h1_female_temp) = colnames(mat_fore_mse_h1_male_temp) = 0:100
  
  ## female
  mat_fore_mse_h1_female_temp[1,] = get(OECD_fore_result_average[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[2,] = get(OECD_fore_result_AIC[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[3,] = get(OECD_fore_result_SHAP_no_truncate[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[4,] = get(OECD_fore_result_SHAP_5[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[5,] = get(OECD_fore_result_SHAP_10[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[6,] = get(OECD_fore_result_SHAP_15[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[7,] = get(OECD_fore_result_SHAP_20[ij])$MSE_female_all[,1]
  mat_fore_mse_h1_female_temp[8,] = get(OECD_fore_result_SHAP_50[ij])$MSE_female_all[,1]
  
  ## male
  mat_fore_mse_h1_male_temp[1,] = get(OECD_fore_result_average[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[2,] = get(OECD_fore_result_AIC[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[3,] = get(OECD_fore_result_SHAP_no_truncate[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[4,] = get(OECD_fore_result_SHAP_5[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[5,] = get(OECD_fore_result_SHAP_10[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[6,] = get(OECD_fore_result_SHAP_15[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[7,] = get(OECD_fore_result_SHAP_20[ij])$MSE_male_all[,1]
  mat_fore_mse_h1_male_temp[8,] = get(OECD_fore_result_SHAP_50[ij])$MSE_male_all[,1]
  
  
  assign(mat_fore_mse_h1_female[ij], mat_fore_mse_h1_female_temp)
  assign(mat_fore_mse_h1_male[ij], mat_fore_mse_h1_male_temp)
  
  rm(mat_fore_mse_h1_female_temp, mat_fore_mse_h1_male_temp)
}

# Convert matrix into a dataframe for plotting purpose

df_fore_mse_h1_female = paste0("df_fore_mse_h1_female_", OECD_countries, sep = "")
df_fore_mse_h1_male = paste0("df_fore_mse_h1_male_", OECD_countries, sep = "")

for(ij in 1:length(OECD_countries))
{
  df_fore_mse_h1_female_temp = melt(apply(get(mat_fore_mse_h1_female[ij]), 2, standardise))
  colnames(df_fore_mse_h1_female_temp) = c("Ensemble", "Age", "MSE")
  assign(df_fore_mse_h1_female[ij], df_fore_mse_h1_female_temp)
  rm(df_fore_mse_h1_female_temp)
  
  df_fore_mse_h1_male_temp = melt(apply(get(mat_fore_mse_h1_male[ij]), 2, standardise))
  colnames(df_fore_mse_h1_male_temp) = c("Ensemble", "Age", "MSE")
  assign(df_fore_mse_h1_male[ij], df_fore_mse_h1_male_temp)
  rm(df_fore_mse_h1_male_temp)
}

# Replicate the plots in our paper.

## Female in Japan

age_MSE_JPN_female = ggplot(df_fore_mse_h1_female_JPN, aes(Age, Ensemble)) + 
  geom_raster(aes(fill = MSE)) +
  scale_fill_gradientn(
    colours = c("white", "red"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
  labs(fill = "MSE standardized by Age          ", title = "Female data in Japan") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5))

age_MSE_JPN_female
  
## Male in Japan
  
age_MSE_JPN_male = ggplot(df_fore_mse_h1_male_JPN, aes(Age, Ensemble)) + 
  geom_raster(aes(fill = MSE)) +
  scale_fill_gradientn(
    colours = c("white", "red"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) +
  labs(title = "Male data in Japan") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5))  

age_MSE_JPN_male 
  
## Female in Italy  
  
age_MSE_ITA_female = ggplot(df_fore_mse_h1_female_ITA, aes(Age, Ensemble)) + 
  geom_raster(aes(fill = MSE)) +
  scale_fill_gradientn(
    colours = c("white", "red"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) +
  labs(title = "Female data in Italy") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5))  
  
age_MSE_ITA_female

## Male in Italy
  
age_MSE_ITA_male = ggplot(df_fore_mse_h1_male_ITA, aes(Age, Ensemble)) + 
  geom_raster(aes(fill = MSE)) +
  scale_fill_gradientn(
    colours = c("white", "red"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) +
  labs(title = "Male data in Italy") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5))  

age_MSE_ITA_male

## Create a 2-by-2 panel of plots
age_MSE_JPN_ITA = ggarrange(age_MSE_JPN_female, age_MSE_JPN_male, 
          age_MSE_ITA_female, age_MSE_ITA_male, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

age_MSE_JPN_ITA
  
# Age-stratified MSE by age groups

## Female in Japan

group_fore_mse_h1_female_JPN =  df_fore_mse_h1_female_JPN %>% 
  mutate(age_group = case_when(
    Age == 0 ~ "0",
    Age >= 1 & Age <= 4 ~ "1-4",
    Age >= 5 & Age <= 9 ~ "5-9",
    Age >= 10 & Age <= 14 ~ "10-14",
    Age >= 15 & Age <= 19 ~ "15-19",
    Age >= 20 & Age <= 24 ~ "20-24",
    Age >= 25 & Age <= 29 ~ "25-29",
    Age >= 30 & Age <= 34 ~ "30-34",
    Age >= 35 & Age <= 39 ~ "35-39",
    Age >= 40 & Age <= 44 ~ "40-44",
    Age >= 45 & Age <= 49 ~ "45-49",
    Age >= 50 & Age <= 54 ~ "50-54",
    Age >= 55 & Age <= 59 ~ "55-59",
    Age >= 60 & Age <= 64 ~ "60-64",
    Age >= 65 & Age <= 69 ~ "65-69",
    Age >= 70 & Age <= 74 ~ "70-74",
    Age >= 75 & Age <= 79 ~ "75-79",
    Age >= 80 & Age <= 84 ~ "80-84",
    Age >= 85 & Age <= 89 ~ "85-89",
    Age >= 90 & Age <= 94 ~ "90-94",
    Age >= 95 & Age <= 99 ~ "95-99",
    Age == 100 ~ "100"
    )) %>%
  group_by(Ensemble, age_group) %>%
  summarise(avg_MSE = mean(MSE, na.rm = TRUE), .groups = "drop")
  
group_MSE_JPN_female = ggplot(group_fore_mse_h1_female_JPN, aes(age_group, Ensemble)) + 
  geom_raster(aes(fill = avg_MSE)) +
  scale_fill_gradientn(
    colours = c("#0C7BDC", "#FFC20A"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
  labs(fill = "MSE standardized across Age Group        ", title = "Female data in Japan") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

## Male in Japan

group_fore_mse_h1_male_JPN =  df_fore_mse_h1_male_JPN %>% 
  mutate(age_group = case_when(
    Age == 0 ~ "0",
    Age >= 1 & Age <= 4 ~ "1-4",
    Age >= 5 & Age <= 9 ~ "5-9",
    Age >= 10 & Age <= 14 ~ "10-14",
    Age >= 15 & Age <= 19 ~ "15-19",
    Age >= 20 & Age <= 24 ~ "20-24",
    Age >= 25 & Age <= 29 ~ "25-29",
    Age >= 30 & Age <= 34 ~ "30-34",
    Age >= 35 & Age <= 39 ~ "35-39",
    Age >= 40 & Age <= 44 ~ "40-44",
    Age >= 45 & Age <= 49 ~ "45-49",
    Age >= 50 & Age <= 54 ~ "50-54",
    Age >= 55 & Age <= 59 ~ "55-59",
    Age >= 60 & Age <= 64 ~ "60-64",
    Age >= 65 & Age <= 69 ~ "65-69",
    Age >= 70 & Age <= 74 ~ "70-74",
    Age >= 75 & Age <= 79 ~ "75-79",
    Age >= 80 & Age <= 84 ~ "80-84",
    Age >= 85 & Age <= 89 ~ "85-89",
    Age >= 90 & Age <= 94 ~ "90-94",
    Age >= 95 & Age <= 99 ~ "95-99",
    Age == 100 ~ "100")) %>%
  group_by(Ensemble, age_group) %>%
  summarise(avg_MSE = mean(MSE, na.rm = TRUE), .groups = "drop")

group_MSE_JPN_male = ggplot(group_fore_mse_h1_male_JPN, aes(age_group, Ensemble)) + 
  geom_raster(aes(fill = avg_MSE)) +
  scale_fill_gradientn(
    colours = c("#0C7BDC", "#FFC20A"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
  labs(fill = "MSE standardized across Age group       ", title = "Male data in Japan") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

## Female in Italy

group_fore_mse_h1_female_ITA =  df_fore_mse_h1_female_ITA %>% 
  mutate(age_group = case_when(
    Age == 0 ~ "0",
    Age >= 1 & Age <= 4 ~ "1-4",
    Age >= 5 & Age <= 9 ~ "5-9",
    Age >= 10 & Age <= 14 ~ "10-14",
    Age >= 15 & Age <= 19 ~ "15-19",
    Age >= 20 & Age <= 24 ~ "20-24",
    Age >= 25 & Age <= 29 ~ "25-29",
    Age >= 30 & Age <= 34 ~ "30-34",
    Age >= 35 & Age <= 39 ~ "35-39",
    Age >= 40 & Age <= 44 ~ "40-44",
    Age >= 45 & Age <= 49 ~ "45-49",
    Age >= 50 & Age <= 54 ~ "50-54",
    Age >= 55 & Age <= 59 ~ "55-59",
    Age >= 60 & Age <= 64 ~ "60-64",
    Age >= 65 & Age <= 69 ~ "65-69",
    Age >= 70 & Age <= 74 ~ "70-74",
    Age >= 75 & Age <= 79 ~ "75-79",
    Age >= 80 & Age <= 84 ~ "80-84",
    Age >= 85 & Age <= 89 ~ "85-89",
    Age >= 90 & Age <= 94 ~ "90-94",
    Age >= 95 & Age <= 99 ~ "95-99",
    Age == 100 ~ "100")) %>%
  group_by(Ensemble, age_group) %>%
  summarise(avg_MSE = mean(MSE, na.rm = TRUE), .groups = "drop")

group_MSE_ITA_female = ggplot(group_fore_mse_h1_female_ITA, aes(age_group, Ensemble)) + 
  geom_raster(aes(fill = avg_MSE)) +
  scale_fill_gradientn(
    colours = c("#0C7BDC", "#FFC20A"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
  labs(fill = "MSE standardized across Age Group        ", title = "Female data in Italy") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(x = "Age Group")

## Male in Italy

group_fore_mse_h1_male_ITA =  df_fore_mse_h1_male_ITA %>% 
  mutate(age_group = case_when(
    Age == 0 ~ "0",
    Age >= 1 & Age <= 4 ~ "1-4",
    Age >= 5 & Age <= 9 ~ "5-9",
    Age >= 10 & Age <= 14 ~ "10-14",
    Age >= 15 & Age <= 19 ~ "15-19",
    Age >= 20 & Age <= 24 ~ "20-24",
    Age >= 25 & Age <= 29 ~ "25-29",
    Age >= 30 & Age <= 34 ~ "30-34",
    Age >= 35 & Age <= 39 ~ "35-39",
    Age >= 40 & Age <= 44 ~ "40-44",
    Age >= 45 & Age <= 49 ~ "45-49",
    Age >= 50 & Age <= 54 ~ "50-54",
    Age >= 55 & Age <= 59 ~ "55-59",
    Age >= 60 & Age <= 64 ~ "60-64",
    Age >= 65 & Age <= 69 ~ "65-69",
    Age >= 70 & Age <= 74 ~ "70-74",
    Age >= 75 & Age <= 79 ~ "75-79",
    Age >= 80 & Age <= 84 ~ "80-84",
    Age >= 85 & Age <= 89 ~ "85-89",
    Age >= 90 & Age <= 94 ~ "90-94",
    Age >= 95 & Age <= 9 ~ "95-9",
    Age == 100 ~ "100")) %>%
  group_by(Ensemble, age_group) %>%
  summarise(avg_MSE = mean(MSE, na.rm = TRUE), .groups = "drop")

group_MSE_ITA_male = ggplot(group_fore_mse_h1_male_ITA, aes(age_group, Ensemble)) + 
  geom_raster(aes(fill = avg_MSE)) +
  scale_fill_gradientn(
    colours = c("#0C7BDC", "#FFC20A"),
    rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
  labs(fill = "MSE standardized across Age Group        ", title = "Male data in Italy") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5), axis.title.y=element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(x = "Age Group")
  
## Create a 2-by-2 panel of plots

group_MSE_JPN_ITA = ggarrange(group_MSE_JPN_female, group_MSE_JPN_male, 
                              group_MSE_ITA_female, group_MSE_ITA_male, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

group_MSE_JPN_ITA

