library(wetlandACAD)
library(shiny)
library(dplyr)

shinyServer<-function(input,output,session){

  output$hydroPlot<-renderPlot({
  plot_hydro_site_year(df=welld, yvar=input$SentSite, years=input$Years,
                       site= as.character(sentsites$sitename[sentsites$well==input$SentSite])
                       )
  })
}

