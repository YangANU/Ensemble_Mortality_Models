library(DT)
library(ftsa)

function(input, output)
{
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
  
  
}



#  shiny::runApp()