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
server<-function(input,output,session)
{
    ## Tab1 
    
    Sales<- eventReactive(input$simulate,{
        
        rnorm(input$num_prod,input$mean_sales,input$sd_sales) %>%
            floor()
        
    })
    
    Fcst <- eventReactive(input$simulate,{
        
        rnorm(input$num_prod,input$mean_fcst,input$sd_fcst) %>%
            floor()
        
    })
    
    
    basedata<-reactive({
        
        if(input$Compute){
            data.frame(Sales(),Fcst())
        }
        else
            NULL
        
    })
    
    
    output$basedata1<-renderDataTable(basedata())
    
    ## Tab2 
    Acc <- eventReactive(input$AccuracyCal,{
        
        if(input$accmetric =="Weighted_MAPE") {
            
            
            Dev = abs(Sales()-Fcst())
            Dev_pct = Dev/pmax(Sales(),Fcst())
            X_sum = sum(Sales())
            Sales_pct = Sales()/X_sum
            Weight_Error = Dev_pct*Sales_pct
            
            
            1- sum(Weight_Error)
        }
        
        
        else if(input$accmetric == "MAPE"){
            
            Dev = abs(Sales()-Fcst())
            Dev_pct = Dev/pmax(Sales(),Fcst())
            1- mean(Dev_pct)
            
            
        }
        
        else
            NULL
        
    })
    
    
    output$Accuracy <-renderPrint(Acc())
    
    #Tab3 
    
    output$acctable <- renderDataTable({
        
        if(input$accmetric =="Weighted_MAPE")  {
            
            X_sum <-sum(Sales())
            basedata() %>% 
                mutate(Dev = abs(Sales()-Fcst()),
                       Dev_pct = Dev/pmax(Sales(),Fcst()),
                       Sales_pct = Sales()/X_sum,
                       Weight_Error = Dev_pct*Sales_pct*100)%>%
                arrange(-Weight_Error) %>%
                round(digits=3)%>%
                head()
            
        }
        
        else if (input$accmetric == "MAPE") {
            basedata() %>% 
                mutate(Dev = abs(Sales()-Fcst()),
                       Dev_pct = Dev/pmax(Sales(),Fcst()))%>%
                round(digits =2) %>%
                arrange(-Dev_pct)%>%
                head()
            
        }
        else 
            NULL
    })
    
    ## UI Update Mean Forecast by Server 
    ## As you update the Avg value of Sale for simulation the slider for Avg Forecast is updated
    ## If the value of average sales is 400 then the value of     
    observeEvent(input$UpdateUI,{
        updateSliderInput(session,"mean_fcst",value=(input$mean_sales*0.8))
    })
    
}

shinyApp(ui=ui,server=server)