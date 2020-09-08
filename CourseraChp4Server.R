
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