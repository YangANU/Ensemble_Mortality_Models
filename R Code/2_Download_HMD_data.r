##########################################
# Download OECD countries' mortality data
##########################################

# Load required R packages

library(ftsa)
library(demography)
library(xlsx)

### Either use the provided XLSX file or download mortality data from HMD directly.

###############################
# Use the provided .xlsx files
###############################

## If using the provided MS Excel Spreadsheets in the "OECD_data" folder, run the following code and skip the "Download data directly from HMD" section.

OECD_countries = c("AUT", "BEL", "CZE", "DNK", "EST", "FIN",
                   "FRATNP", "HUN", "ISL", "IRL", "ITA", "JPN",
                   "LVA", "LTU", "LUX", "NLD", "NZL_NP", "NOR",
                   "POL", "ESP", "SWE", "CHE", "GBR_NP", "USA")

OECD_full_name = c("Austria", "Belgium", "Czech", "Denmark", "Estonia",
                   "Finland", "France", "Hungary", "Iceland", "Ireland",
                   "Italy", "Japan", "Latvia", "Lithuania", "Luxemburg",
                   "Netherlands", "New_Zealand", "Norway", "Poland",
                   "Spain", "Sweden", "Switzerland", "UK", "USA") 

OECD_selected = c("AUT_selected", "BEL_selected", "CZE_selected", "DNK_selected", "EST_selected", 
                  "FIN_selected", "FRA_selected", "HUN_selected", "ISL_selected", "IRL_selected", 
                  "ITA_selected", "JPN_selected", "LVA_selected", "LTU_selected", "LUX_selected", 
                  "NLD_selected", "NZL_selected", "NOR_selected", "POL_selected", "ESP_selected", 
                  "SWE_selected", "CHE_selected", "GBR_selected", "USA_selected")



for(ik in 1:length(OECD_countries))
{
  
  temp_list = list()
  
  country_temp_name = paste0(OECD_full_name[ik], "_HMD_data", sep = "")
  
  ###########
  # data_raw
  ###########
  
  temp_list$data_raw = list()
  
  # Deaths
  temp_list$data_raw$Deaths = read.xlsx(file = paste0("../OECD_data/", country_temp_name,".xlsx"), sheetName = "Deaths")
  
  
  # Exposures
  temp_list$data_raw$Exposures = read.xlsx(file = paste0("../OECD_data/", country_temp_name,".xlsx"), sheetName = "Exposures")
  
  # Mx
  temp_list$data_raw$Mx = read.xlsx(file = paste0("../OECD_data/", country_temp_name,".xlsx"), sheetName = "Mx")
  
  
  ##################
  # Demography data
  ##################

  temp_list$data_demog = list()
  country = OECD_countries[ik]
  mx_year = unique(temp_list$data_raw$Mx$Year)
  mx_age = unique(temp_list$data_raw$Mx$Age)
  
  ## female
  temp_list$data_demog[[1]] = demography::demogdata(data = matrix(temp_list$data_raw$Mx$Female, nrow = length(mx_age), ncol = length(mx_year)),
                                          pop = matrix(temp_list$data_raw$Exposures$Female, nrow = length(mx_age), ncol = length(mx_year)),
                                          ages = mx_age, years = mx_year, type = "mortality", label = country, name = "Female")
  
  ## male
  temp_list$data_demog[[2]] = demography::demogdata(data = matrix(temp_list$data_raw$Mx$Male, nrow = length(mx_age), ncol = length(mx_year)), 
                                          pop = matrix(temp_list$data_raw$Exposures$Male, nrow = length(mx_age), ncol = length(mx_year)),
                                          ages = mx_age, years = mx_year, type = "mortality", label = country, name = "Male")
  
  ## total
  temp_list$data_demog[[3]] = demography::demogdata(data = matrix(temp_list$data_raw$Mx$Total, nrow = length(mx_age), ncol = length(mx_year)), 
                                          pop = matrix(temp_list$data_raw$Exposures$Total, nrow = length(mx_age), ncol = length(mx_year)),
                                          ages = mx_age, years = mx_year, type = "mortality", label = country, name = "Total")
  names(temp_list$data_demog) = c("Female", "Male", "Total")
  
  
  assign(paste0(OECD_countries[ik], "_HMD", sep = ""), temp_list)
  
  
  print(ik)
}

##################################
# Download data directly from HMD
##################################


## The following R script contains functions for downloading and preparing age-specific mortality data for OECD countries.

# Define a function to download age-specific mortality rates from the Human Mortality Database
hmd_demog = function(country, username, password)
{
  variables = c("Deaths", "Exposures", "Mx")
  item = paste0(variables,"_1x1")
  
  data_raw = list()
  for(i in seq_along(item)) 
  {
    # Raw data
    data_raw[[i]] = HMDHFDplus::readHMDweb(country, item = item[i],
                                        username = username, 
                                        password = password, fixup = TRUE)
  }
  names(data_raw) = variables
  
  # Demography data
  data_demog = list()
  mx_year = unique(data_raw$Mx$Year)
  mx_age = unique(data_raw$Mx$Age)
  ## female
  data_demog[[1]] = demography::demogdata(data = matrix(data_raw$Mx$Female, nrow = length(mx_age), ncol = length(mx_year)), 
                                pop = matrix(data_raw$Exposures$Female, nrow = length(mx_age), ncol = length(mx_year)),
                                ages = mx_age, years = mx_year, type = "mortality", label = country, name = "Female")
  ## male
  data_demog[[2]] = demography::demogdata(data = matrix(data_raw$Mx$Male, nrow = length(mx_age), ncol = length(mx_year)), 
                                pop = matrix(data_raw$Exposures$Male, nrow = length(mx_age), ncol = length(mx_year)),
                                ages = mx_age, years = mx_year, type = "mortality", label = country, name = "Male")
  # total
  data_demog[[3]] = demography::demogdata(data = matrix(data_raw$Mx$Total, nrow = length(mx_age), ncol = length(mx_year)), 
                                pop = matrix(data_raw$Exposures$Total, nrow = length(mx_age), ncol = length(mx_year)),
                                ages = mx_age, years = mx_year, type = "mortality", label = country, name = "Total")
  names(data_demog) = c("Female", "Male", "Total")
  
  return(list(data_raw = data_raw, data_demog = data_demog))
}

# Download mortality data for OECD countries by using your registered HMD account
# Update the username and password arguments below using your registered details

username = "..."
password = "..."


for(ik in 1:length(OECD_countries))
{
  country_temp_name = paste0(OECD_countries[ik], "_HMD", sep = "")
  
  assign(country_temp_name, hmd_demog(OECD_countries[ik], username = username, password = password))
  
  print(ik)
}

#### (This step is optional) ####
#### Save the data into MS Excel spreadsheet (.xlsx) for easy reading ####
for(ik in 1:length(OECD_full_name))
{
  file_name = paste0(OECD_full_name[ik], "_HMD_data.xlsx")
  
  ## Adjust the folder path by changing the "file_name" below to save the data into a easy-to-find place
  
  # Deaths
  write.xlsx(get(paste0(OECD_countries[ik], "_HMD", sep = ""))$data_raw$Deaths, file = file_name, sheetName = "Deanths", row.names = FALSE)
  gc()
  # Exposures
  write.xlsx(get(paste0(OECD_countries[ik], "_HMD", sep = ""))$data_raw$Exposures, file = file_name, sheetName = "Exposures", append = TRUE, row.names = FALSE)
  gc()
  # Mx
  write.xlsx(get(paste0(OECD_countries[ik], "_HMD", sep = ""))$data_raw$Mx, file = file_name, sheetName = "Mx", append = TRUE, row.names = FALSE)
  gc()
  
  print(ik)
}


#################################################
# Determine the common training & testing period
#################################################

year_start_all = year_end_all = rep(0, length(OECD_countries))

for(ij in seq_along(OECD_countries))
{
  year_start_all[ij] = get(paste0(OECD_countries[ij], "_HMD", sep = ""))$data_raw$Mx$Year[1]
  year_end_all[ij] = tail(get(paste0(OECD_countries[ij], "_HMD", sep = ""))$data_raw$Mx$Year, 1)
}

min_year_selected = max(year_start_all) ## selected starting year = 1960
max_year_selected = min(year_end_all) ## selected ending year = 2019


for(ij in seq_along(OECD_selected))
{
  demog_obj = get(paste0(OECD_countries[ij], "_HMD", sep = ""))$data_demog
  
  demog_temp_female = extract.ages(extract.years(demog_obj$Female, years = min_year_selected:max_year_selected), 0:100)
  demog_temp_male = extract.ages(extract.years(demog_obj$Male, years = min_year_selected:max_year_selected), 0:100)
  demog_temp_total = extract.ages(extract.years(demog_obj$Total, years = min_year_selected:max_year_selected), 0:100)
  
  demog_temp = list(demog_temp_female, demog_temp_male, demog_temp_total)
  names(demog_temp) = c("Female", "Male", "Total")
  
  assign(OECD_selected[ij], demog_temp)
}








