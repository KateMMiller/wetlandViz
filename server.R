library(wetlandACAD)
library(shiny)
library(dplyr)
library(leaflet)
library(shinyjs)

shinyServer<-function(input,output,session){

  #-----------------------------
  # Wetland Map Controls
  #-----------------------------
  
  # Render wetland map
  output$WetlandMap<-renderLeaflet({
    leaflet() %>% 
      setView(lng = mean(-68.711, -67.953), lat = mean(44.484, 43.953), zoom=10) %>% 
      setMaxBounds(lng1 = -68.711, lng2 = -67.953, lat1 = 44.484, lat2 = 43.953)
  })
  
  # Map Colors
  #CircleColors<-reactive({
  #  colorBin(palette = c("DodgerBlue", "ForestGreen"), domain = sitedata$Site_Type)
  #})
  
  observe({
    leafletProxy("WetlandMap") %>% 
      addCircles(data=sitedata, radius=40, 
                 lng=sitedata$Longitude, lat=sitedata$Latitude, 
                 color=~ifelse(sitedata$Site_Type=='RAM','ForestGreen','DodgerBlue'),
                 layerId=sitedata$Label, popup= ~as.character(sitedata$Label))
  })
  
  # Make Attribution
  NPSAttrib<-HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")
  
  # #Add a tile layer
  observe({
    leafletProxy("WetlandMap") %>%
      clearTiles() %>%
      addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8))%>%
      addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>%
      addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.68926899,nps.502a840b/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib, options=tileOptions(minZoom=8) ) %>%
      addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=T))
  })
  
  # End of controls for Wetland Map
  
  #-------------------------------
  # Hydrograph Plot Controls
  #-------------------------------
  # Render hydroplot
  output$hydroPlot<-renderPlot({
  plot_hydro_site_year(df=welld, yvar=input$SentSite, years=input$Years,
                       site= as.character(sentsites$sitename[sentsites$well==input$SentSite])
                       )
  })
  
}

