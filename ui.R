library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Define UI
dashboardPage(
  dashboardHeader(title = "Age-Specific Mortality Forecasts for OECD Countries"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("country", "Choose a country:", choices = country_names),
      sliderInput("h", "Forecasting horizon (h):", min = 1, max = 10, value = 1),
      selectInput("alpha", paste0("Shapley parameter ", "(\u03b1):"),
                  choices = c(0.05, 0.1, 0.15, 0.2, 0.5))
    )
  ),
  dashboardBody(
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
    ),
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
    )
  )
)
