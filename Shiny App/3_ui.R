library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Define UI
dashboardPage(
  dashboardHeader(title = "Age-Specific Mortality Forecasts for OECD Countries"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("country", "Choose a country:", choices = country_names, selected = "USA"),
      sliderInput("h", "Forecasting horizon (h):", min = 1, max = 10, value = 1),
      selectInput("alpha", paste0("Shapley parameter ", "(\u03b1):"),
                  choices = c(0.05, 0.1, 0.15, 0.2, 0.5)),
      menuItem("Plot of Forecasts", tabName = "plots"),
      menuItem("MSE & MAE", tabName = "table"),
      menuItem("Comparison of Forecasts", tabName = "comparison"),
      menuItem("Age-stratified MSE", tabName = "age_stratified"),
      menuItem("Interval Score", tabName = "interval_score")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("plots",
              fluidRow(
                column(2, h3("Method", align = "center")),
                column(5, h3("Female", align = "center")),
                column(5, h3("Male", align = "center"))
              ),
              fluidRow(
                column(2, h3("Average", style= "margin-top: 200px;", align = "center")),
                column(5, box(plotOutput("Average_female"), width = NULL)),
                column(5, box(plotOutput("Average_male"), width = NULL))
              ),
              fluidRow(
                column(2, h3("AIC", style= "margin-top: 200px;", align = "center")),
                column(5, box(plotOutput("AIC_female"), width = NULL)),
                column(5, box(plotOutput("AIC_male"), width = NULL))
              ),
              fluidRow(
                column(2, h3("Shapley", style= "margin-top: 200px;", align = "center")),
                column(5, box(plotOutput("shapley_female"), width = NULL)),
                column(5, box(plotOutput("shapley_male"), width = NULL))
              ),
              fluidRow(
                column(2, h3(paste0("Shapley with Truncation"), style= "margin-top: 200px;", align = "center")),
                column(5, box(plotOutput("shapley_trunc_female"), width = NULL)),
                column(5, box(plotOutput("shapley_trunc_male"), width = NULL))
              )
      ),
      tabItem("table",
              fluidRow(
                column(12,
                       wellPanel(
                         h4("Mean Squared Errors (MSE x 100)"),
                         DT::dataTableOutput("mse_table")
                       )
                )
              ),
              fluidRow(
                column(12,
                       wellPanel(
                         h4("Mean Absolute Errors (MAE x 100)"),
                         DT::dataTableOutput("mae_table")
                       )
                )
              ),
              fluidRow(
                column(12,
                       wellPanel(
                         h4("Individual Model MSEs (x 100)"),
                         DT::dataTableOutput("base_mse_table")
                       )
                )
              ),
              fluidRow(
                column(12,
                       wellPanel(
                         h4("Individual Model MAEs (x 100)"),
                         DT::dataTableOutput("base_mae_table")
                       )
                )
              )
      ),
      tabItem("comparison",
              wellPanel(
                h2("MSE x 100", align = "center"),
                fluidRow(
                  column(6, h3("Female", align = "center")),
                  column(6, h3("Male", align = "center"))
                ),
                fluidRow(
                  column(6, box(plotOutput("comparison_mse_female"), width = NULL)),
                  column(6, box(plotOutput("comparison_mse_male"), width = NULL))
                )
              ),
              wellPanel(
                h2("MAE x 100", align = "center"),
                fluidRow(
                  column(6, h3("Female", align = "center")),
                  column(6, h3("Male", align = "center"))
                ),
                fluidRow(
                  column(6, box(plotOutput("comparison_mae_female"), width = NULL)),
                  column(6, box(plotOutput("comparison_mae_male"), width = NULL))
                )
              )
      ),
      tabItem("age_stratified",
              wellPanel(
                h2("Age-stratified MSE Heatmap", align = "center"),
                fluidRow(
                  column(6, h3("Female", align = "center")),
                  column(6, h3("Male", align = "center"))
                ),
                fluidRow(
                  column(6, box(plotOutput("age_stratified_mse_female"), width = NULL)),
                  column(6, box(plotOutput("age_stratified_mse_male"), width = NULL))
                )
              )
      ),
      tabItem("interval_score",
              wellPanel(
                h2("Mean Pointwise Interval Score", align = "center"),
                fluidRow(
                  column(12,
                         DT::dataTableOutput("interval_score")
                  )
                )
              )
      )
    )
  )
)
