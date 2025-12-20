library(DT)
library(ftsa)
library(ggplot2)
library(tidyr)
library(reshape2)

function(input, output)
{
  load("R_Shiny.RData")
  
  # Average
  ## female
  output$Average_female <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_female"))[[h]], lwd = 1, lty = 3, main = country_names[country_index], ylab = "Log Mortality Rate")
    lines(get(paste0(OECD_countries[country_index], "_average_fore_female"))[[h]], lwd = 2)
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  ## male
  output$Average_male <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_male"))[[h]], lwd = 1, lty = 3, main = country_names[country_index], ylab = "Log Mortality Rate")
    lines(get(paste0(OECD_countries[country_index], "_average_fore_male"))[[h]], lwd = 2)
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  # AIC
  ## female
  output$AIC_female <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_female"))[[h]], lwd = 1, lty = 3, main = country_names[country_index], ylab = "Log Mortality Rate")
    lines(get(paste0(OECD_countries[country_index], "_AIC_fore_female"))[[h]], lwd = 2)
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  ## male
  output$AIC_male <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_male"))[[h]], lwd = 1, lty = 3, main = country_names[country_index], ylab = "Log Mortality Rate")
    lines(get(paste0(OECD_countries[country_index], "_AIC_fore_male"))[[h]], lwd = 2)
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  # Shapley
  ## female
  output$shapley_female <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_female"))[[h]], lwd = 1, lty = 3, main = country_names[country_index], ylab = "Log Mortality Rate")
    lines(get(paste0(OECD_countries[country_index], "_SHAP_fore_female"))[[h]], lwd = 2)
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  ## male
  output$shapley_male <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_male"))[[h]], lwd = 1, lty = 3, main = country_names[country_index], ylab = "Log Mortality Rate")
    lines(get(paste0(OECD_countries[country_index], "_SHAP_fore_male"))[[h]], lwd = 2)
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  # Shapley Truncation
  ## female
  output$shapley_trunc_female <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_female"))[[h]], lwd = 1, lty = 3, ylab = "Log Mortality Rate")
    
    if(alpha == 0.05)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_5_fore_female"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 5%)"))
    }
    
    if(alpha == 0.1)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_10_fore_female"))[[h]], lwd = 2)
      title(paste(country_names[country_index], "(alpha = 10%)"))
    }
    
    if(alpha == 0.15)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_15_fore_female"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 15%)"))
    }
    
    if(alpha == 0.2)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_20_fore_female"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 20%)"))
    }
    
    if(alpha == 0.5)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_50_fore_female"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 50%)"))
    }
    
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  ## male
  output$shapley_trunc_male <- renderPlot({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    plot(get(paste0(OECD_countries[country_index], "_obs_male"))[[h]], lwd = 1, lty = 3, ylab = "Log Mortality Rate")
    
    if(alpha == 0.05)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_5_fore_male"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 5%)"))
    }
    
    if(alpha == 0.1)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_10_fore_male"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 10%)"))
    }
    
    if(alpha == 0.15)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_15_fore_male"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 15%)"))
    }
    
    if(alpha == 0.2)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_20_fore_male"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 20%)"))
    }
    
    if(alpha == 0.5)
    {
      lines(get(paste0(OECD_countries[country_index], "_SHAP_50_fore_male"))[[h]], lwd = 2) 
      title(paste(country_names[country_index], "(alpha = 50%)"))
    }
    
    legend("bottomright", lwd = c(1,1), lty = c(3,1), c("Observation", "Forecast"))
  })
  
  
  output$mse_table <- DT::renderDataTable({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    if(alpha == 0.05)
    {
      table_temp_mse = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_5_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.1)
    {
      table_temp_mse = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_10_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.15)
    {
      table_temp_mse = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_15_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.2)
    {
      table_temp_mse = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_20_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.50)
    {
      table_temp_mse = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_50_MSE_h_", h)))*100, 4)
    }
    
    
    DT::datatable(table_temp_mse, options = list(lengthMenu = c(5, 10, 25), pageLength = 5))
  })
  
  
  output$mae_table <- DT::renderDataTable({
    country_index = match(input$country, country_names)
    h = input$h
    alpha = input$alpha
    
    if(alpha == 0.05)
    {
      table_temp_mae = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_5_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.1)
    {
      table_temp_mae = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_10_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.15)
    {
      table_temp_mae = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_15_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.2)
    {
      table_temp_mae = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_20_MSE_h_", h)))*100, 4)
    }
    
    if(alpha == 0.50)
    {
      table_temp_mae = round(cbind(get(paste0("all_country_MSE_h_", h)), get(paste0("SHAP_50_MSE_h_", h)))*100, 4)
    }
    
    
    DT::datatable(table_temp_mae, options = list(lengthMenu = c(5, 10, 25), pageLength = 5))
  })
  
  
  
  output$base_mse_table <- DT::renderDataTable({
    country_index = match(input$country, country_names)

    data_table_female = data.frame(Country = rep(input$country, 10), Sex = rep("F", 10), h = 1:10, round(cbind(get(comparison_mse_female[country_index])$base[1:10,]), 4))
    data_table_male = data.frame(Country = rep(input$country, 10), Sex = rep("M", 10), h = 1:10, round(cbind(get(comparison_mse_male[country_index])$base[1:10,]), 4))
    data_table = rbind(data_table_female, data_table_male)
    
    DT::datatable(data_table, options = list(lengthMenu = c(10), pageLength = 10), rownames = FALSE)
  })
  
  
  output$base_mae_table <- DT::renderDataTable({
    country_index = match(input$country, country_names)
    
    data_table_female = data.frame(Country = rep(input$country, 10), Sex = rep("F", 10), h = 1:10, round(cbind(get(comparison_mae_female[country_index])$base[1:10,]), 4))
    data_table_male = data.frame(Country = rep(input$country, 10), Sex = rep("M", 10), h = 1:10, round(cbind(get(comparison_mae_male[country_index])$base[1:10,]), 4))
    data_table = rbind(data_table_female, data_table_male)
    
    DT::datatable(data_table, options = list(lengthMenu = c(10), pageLength = 10), rownames = FALSE)
  })
  

  output$comparison_mse_female <- renderPlot({
    country_index = match(input$country, country_names)
    
    data_input = data.frame(h = 1:10, get(comparison_mse_female[country_index])$combined) |> pivot_longer(-h)
    combined_names = colnames(get(comparison_mse_female[country_index])$combined)
    
   ggplot(data_input, aes(x = h, y = value, colour = name, linetype = name, shape = name)) + 
     geom_line() + 
     geom_point()+
     theme(legend.position="bottom") +
     xlab("Forecast Horizon (h)") + ylab("MSE x 100") + 
     guides(col = guide_legend(title = "Method")) +
     scale_linetype_manual(values = c(2,2,2,1,1,1,1,1),
                           breaks = combined_names) + 
     scale_shape_manual(values = c(1, 1, 1, 2, 3, 4, 5, 6),
                        breaks = combined_names) +
     scale_color_manual(values = c(hcl.colors(8, "Reds")[3:5],  hcl.colors(8, "Blues")[1:5]),
                        breaks = combined_names) +
     scale_x_continuous("Forecast Horizon (h)", limits = c(1,10), breaks = 1:10) +
     labs(color = "Method", linetype = "Method", shape = "Method")
  })
  
  output$comparison_mse_male <- renderPlot({
    country_index = match(input$country, country_names)
    
    data_input = data.frame(h = 1:10, get(comparison_mse_male[country_index])$combined) |> pivot_longer(-h)
    combined_names = colnames(get(comparison_mse_male[country_index])$combined)
    
    ggplot(data_input, aes(x = h, y = value, colour = name, linetype = name, shape = name)) + 
      geom_line() + 
      geom_point()+
      theme(legend.position="bottom") +
      xlab("Forecast Horizon (h)") + ylab("MSE x 100") + 
      guides(col = guide_legend(title = "Method")) +
      scale_linetype_manual(values = c(2,2,2,1,1,1,1,1),
                            breaks = combined_names) + 
      scale_shape_manual(values = c(1, 1, 1, 2, 3, 4, 5, 6),
                         breaks = combined_names) +
      scale_color_manual(values = c(hcl.colors(8, "Reds")[3:5],  hcl.colors(8, "Blues")[1:5]),
                         breaks = combined_names) +
      scale_x_continuous("Forecast Horizon (h)", limits = c(1,10), breaks = 1:10) +
      labs(color = "Method", linetype = "Method", shape = "Method")
  })
  
  
  output$comparison_mae_female <- renderPlot({
    country_index = match(input$country, country_names)
    
    data_input = data.frame(h = 1:10, get(comparison_mae_female[country_index])$combined) |> pivot_longer(-h)
    combined_names = colnames(get(comparison_mae_female[country_index])$combined)
    
    ggplot(data_input, aes(x = h, y = value, colour = name, linetype = name, shape = name)) + 
      geom_line() + 
      geom_point()+
      theme(legend.position="bottom") +
      xlab("Forecast Horizon (h)") + ylab("MSE x 100") + 
      guides(col = guide_legend(title = "Method")) +
      scale_linetype_manual(values = c(2,2,2,1,1,1,1,1),
                            breaks = combined_names) + 
      scale_shape_manual(values = c(1, 1, 1, 2, 3, 4, 5, 6),
                         breaks = combined_names) +
      scale_color_manual(values = c(hcl.colors(8, "Reds")[3:5],  hcl.colors(8, "Blues")[1:5]),
                         breaks = combined_names) +
      scale_x_continuous("Forecast Horizon (h)", limits = c(1,10), breaks = 1:10) +
      labs(color = "Method", linetype = "Method", shape = "Method")
  })
  
  output$comparison_mae_male <- renderPlot({
    country_index = match(input$country, country_names)
    
    data_input = data.frame(h = 1:10, get(comparison_mae_male[country_index])$combined) |> pivot_longer(-h)
    combined_names = colnames(get(comparison_mae_male[country_index])$combined)
    
    ggplot(data_input, aes(x = h, y = value, colour = name, linetype = name, shape = name)) + 
      geom_line() + 
      geom_point()+
      theme(legend.position="bottom") +
      xlab("Forecast Horizon (h)") + ylab("MSE x 100") + 
      guides(col = guide_legend(title = "Method")) +
      scale_linetype_manual(values = c(2,2,2,1,1,1,1,1),
                            breaks = combined_names) + 
      scale_shape_manual(values = c(1, 1, 1, 2, 3, 4, 5, 6),
                         breaks = combined_names) +
      scale_color_manual(values = c(hcl.colors(8, "Reds")[3:5],  hcl.colors(8, "Blues")[1:5]),
                         breaks = combined_names) +
      scale_x_continuous("Forecast Horizon (h)", limits = c(1,10), breaks = 1:10) +
      labs(color = "Method", linetype = "Method", shape = "Method")
  })
  
  # Age-stratified MSE Heatmap
  ## female
  output$age_stratified_mse_female <- renderPlot({
    h = input$h
    ij = match(input$country, country_names)
    age_stratified_mse = matrix(NA, nrow = 8, ncol = 101)
    rownames(age_stratified_mse) = c("SMA", "AIC", "SHAP", "SHAP5",
                                     "SHAP10", "SHAP15", "SHAP20", "SHAP50")
    colnames(age_stratified_mse) = 0:100
    
    age_stratified_mse[1,] = get(OECD_fore_result_average[ij])$MSE_female_all[,h]
    age_stratified_mse[2,] = get(OECD_fore_result_AIC[ij])$MSE_female_all[,h]
    age_stratified_mse[3,] = get(OECD_fore_result_SHAP_no_truncate[ij])$MSE_female_all[,1]
    age_stratified_mse[4,] = get(OECD_fore_result_SHAP_5[ij])$MSE_female_all[,h]
    age_stratified_mse[5,] = get(OECD_fore_result_SHAP_10[ij])$MSE_female_all[,h]
    age_stratified_mse[6,] = get(OECD_fore_result_SHAP_15[ij])$MSE_female_all[,h]
    age_stratified_mse[7,] = get(OECD_fore_result_SHAP_20[ij])$MSE_female_all[,h]
    age_stratified_mse[8,] = get(OECD_fore_result_SHAP_50[ij])$MSE_female_all[,h]
    
    age_stratified_mse_df = melt(apply(age_stratified_mse, 2, function(x){(x-min(x))/(max(x)-min(x))}))
    colnames(age_stratified_mse_df) = c("Ensemble", "Age", "MSE")
    
    ggplot(age_stratified_mse_df, aes(Age, Ensemble)) + 
      geom_raster(aes(fill = MSE)) +
      scale_fill_gradientn(
        colours = c("white", "red"),
        rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
      labs(fill = "MSE standardized by Age          ", 
           title = paste0("Female data in ", input$country, sep = "")) + 
      theme(plot.title = element_text(size = 18, hjust = 0.5), legend.position = "bottom")
  })
  
  ## male
  output$age_stratified_mse_male <- renderPlot({
    h = input$h
    ij = match(input$country, country_names)
    age_stratified_mse = matrix(NA, nrow = 8, ncol = 101)
    rownames(age_stratified_mse) = c("SMA", "AIC", "SHAP", "SHAP5",
                                     "SHAP10", "SHAP15", "SHAP20", "SHAP50")
    colnames(age_stratified_mse) = 0:100
    
    age_stratified_mse[1,] = get(OECD_fore_result_average[ij])$MSE_male_all[,h]
    age_stratified_mse[2,] = get(OECD_fore_result_AIC[ij])$MSE_male_all[,h]
    age_stratified_mse[3,] = get(OECD_fore_result_SHAP_no_truncate[ij])$MSE_male_all[,1]
    age_stratified_mse[4,] = get(OECD_fore_result_SHAP_5[ij])$MSE_male_all[,h]
    age_stratified_mse[5,] = get(OECD_fore_result_SHAP_10[ij])$MSE_male_all[,h]
    age_stratified_mse[6,] = get(OECD_fore_result_SHAP_15[ij])$MSE_male_all[,h]
    age_stratified_mse[7,] = get(OECD_fore_result_SHAP_20[ij])$MSE_male_all[,h]
    age_stratified_mse[8,] = get(OECD_fore_result_SHAP_50[ij])$MSE_male_all[,h]
    
    age_stratified_mse_df = melt(apply(age_stratified_mse, 2, function(x){(x-min(x))/(max(x)-min(x))}))
    colnames(age_stratified_mse_df) = c("Ensemble", "Age", "MSE")
    
    ggplot(age_stratified_mse_df, aes(Age, Ensemble)) + 
      geom_raster(aes(fill = MSE)) +
      scale_fill_gradientn(
        colours = c("white", "red"),
        rescaler = ~ scales::rescale_mid(.x, mid = 0.5)) + 
      labs(fill = "MSE standardized by Age          ", 
           title = paste0("male data in ", input$country, sep = "")) + 
      theme(plot.title = element_text(size = 18, hjust = 0.5), legend.position = "bottom")
  })
  
  # Prediction Interval Score
  output$interval_score <- DT::renderDataTable({
    h = input$h
    
    OECD_countries = c("AUT", "BEL", "CZE", "DNK", "EST", "FIN",
                       "FRATNP", "HUN", "ISL", "IRL", "ITA", "JPN",
                       "LVA", "LTU", "LUX", "NLD", "NZL_NP", "NOR",
                       "POL", "ESP", "SWE", "CHE", "GBR_NP", "USA")
    
    OECD_full_name = c("Austria", "Belgium", "Czech", "Denmark", "Estonia",
                       "Finland", "France", "Hungary", "Iceland", "Ireland",
                       "Italy", "Japan", "Latvia", "Lithuania", "Luxemburg",
                       "Netherlands", "New_Zealand", "Norway", "Poland",
                       "Spain", "Sweden", "Switzerland", "UK", "USA") 
    
    
    OECD_interval_test_average = paste0(OECD_countries, "_interval_test_average")
    # OECD_interval_test_envelope = paste0(OECD_countries, "_interval_test_envelope")
    OECD_interval_test_interior_trimming = paste0(OECD_countries, "_interval_test_interior_trimming")
    OECD_interval_test_mse = paste0(OECD_countries, "_interval_test_mse")
    OECD_interval_test_AIC = paste0(OECD_countries, "_interval_test_AIC")
    OECD_interval_test_SHAP_no_truncate = paste0(OECD_countries, "_interval_test_SHAP_no_truncate")
    OECD_interval_test_SHAP_5 = paste0(OECD_countries, "_interval_test_SHAP_5")
    OECD_interval_test_SHAP_10 = paste0(OECD_countries, "_interval_test_SHAP_10")
    OECD_interval_test_SHAP_15 = paste0(OECD_countries, "_interval_test_SHAP_15")
    OECD_interval_test_SHAP_20 = paste0(OECD_countries, "_interval_test_SHAP_20")
    OECD_interval_test_SHAP_50 = paste0(OECD_countries, "_interval_test_SHAP_50")
    
    interval_test_table_basic = interval_test_table_SHAP = list()
    
    for(country_index in 1:length(OECD_countries))
    {
      #### Benchmark ensembles
      ## Simple Average
      SA_F = get(OECD_interval_test_average[country_index])$mean_interval_score_female[h]
      SA_M = get(OECD_interval_test_average[country_index])$mean_interval_score_male[h]
      
      ## Iterior Trimming
      IT_F = get(OECD_interval_test_interior_trimming[country_index])$mean_interval_score_female[h]
      IT_M = get(OECD_interval_test_interior_trimming[country_index])$mean_interval_score_male[h]
      
      ## AIC
      AIC_F = get(OECD_interval_test_AIC[country_index])$mean_interval_score_female[h]
      AIC_M = get(OECD_interval_test_AIC[country_index])$mean_interval_score_male[h]
      
      ## MSE
      MSE_F = get(OECD_interval_test_mse[country_index])$mean_interval_score_female[h]
      MSE_M = get(OECD_interval_test_mse[country_index])$mean_interval_score_male[h]
      
      interval_test_table_basic[[country_index]] = cbind(SA_F, SA_M, IT_F, IT_M,
                                                         AIC_F, AIC_M, MSE_F, MSE_M)
      
     
      
      #### Proposed ensembles
      ## SHAP no truncation
      SHAP_0_F = get(OECD_interval_test_SHAP_no_truncate[country_index])$mean_interval_score_female[h]
      SHAP_0_M = get(OECD_interval_test_SHAP_no_truncate[country_index])$mean_interval_score_male[h]
      
      ## SHAP_5
      SHAP_5_F = get(OECD_interval_test_SHAP_5[country_index])$mean_interval_score_female[h]
      SHAP_5_M = get(OECD_interval_test_SHAP_5[country_index])$mean_interval_score_male[h]
      
      ## SHAP_10
      SHAP_10_F = get(OECD_interval_test_SHAP_10[country_index])$mean_interval_score_female[h]
      SHAP_10_M = get(OECD_interval_test_SHAP_10[country_index])$mean_interval_score_male[h]
      
      ## SHAP_20
      SHAP_20_F = get(OECD_interval_test_SHAP_20[country_index])$mean_interval_score_female[h]
      SHAP_20_M = get(OECD_interval_test_SHAP_20[country_index])$mean_interval_score_male[h]
      
      interval_test_table_SHAP[[country_index]] = cbind(SHAP_0_F,  SHAP_0_M,
                                                        SHAP_5_F, SHAP_5_M,
                                                        SHAP_10_F, SHAP_10_M,
                                                        SHAP_20_F, SHAP_20_M)
      
    }
    
    interval_test_table_SHAP_df = do.call(rbind.data.frame, interval_test_table_SHAP)
    interval_test_table_SHAP_df = rbind(interval_test_table_SHAP_df, apply(interval_test_table_SHAP_df, 2, mean))
    rownames(interval_test_table_SHAP_df) = c(OECD_full_name, "Mean")
    
    interval_test_table_basic_df = do.call(rbind.data.frame, interval_test_table_basic)
    interval_test_table_basic_df = rbind(interval_test_table_basic_df, apply(interval_test_table_basic_df, 2, mean))
    rownames(interval_test_table_basic_df) = c(OECD_full_name, "Mean")
    
    DT::datatable(round(cbind(interval_test_table_basic_df, interval_test_table_SHAP_df), 4), options = list(lengthMenu = c(10), pageLength = 10), rownames = TRUE)
    
  })
  
  
  
  
  
  
}






#  shiny::runApp()


