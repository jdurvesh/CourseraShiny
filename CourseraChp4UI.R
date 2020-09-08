library(tidyverse)
library(shiny)
library(DT)

ui <-fluidPage(
  
  titlePanel(h3("Forecast Accuracy by Mean Absolute Percent Error(MAPE) or Weighted MAPE")),
  
  sidebarLayout(
    sidebarPanel( 
      # Enter the inputs for generating Sales 
      sliderInput("num_prod","Enter number of products",min=100,max=200,value=200),
      sliderInput("mean_sales","Enter the average sales",min=250,max=1000,value=400),
      sliderInput("sd_sales","Enter the standard dev sales",min=100, max=150,value=125),
      hr(),
      # Enter the inputs for generationg Forecast 
      sliderInput("mean_fcst","Enter the  forecast",min=150,max=520,value=150),
      sliderInput("sd_fcst","Enter the standard dev forecast",min=25, max=50,value=25),
      hr(),
      br(),
      checkboxInput(inputId = "Compute","Display Simulated Data"),
      selectInput("accmetric","Choose a Accuracy Metric",choices=c("Weighted_MAPE","MAPE")),
      actionButton('simulate','Simulate'),
      actionButton('UpdateUI',"Update Mean_Forecast via Server"),
      actionButton('AccuracyCal',"Calculate Accuracy")
      #submitButton(text="ApplyChanges")
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tab1",
                 h4("The simiulated values for Sales and Forecast based on selected mean and sd"),
                 h4("The distribution used to simulate the values is a normal distribution"),
                 DT::dataTableOutput("basedata1"),
                 h6("Accuracy in next Tab is computed basis of this data ")
        ),
        tabPanel("Tab2",
                 h3("Accuracy"),
                 p("The Value Printed below is the  accuracy for the simulated Data"),
                 verbatimTextOutput("Accuracy")       
                 
        ),
        tabPanel("Tab3",
                 h4("Top Deviations that have impacted Accuracy"),
                 DT::dataTableOutput("acctable")        
        )
      )
    )
  )
)