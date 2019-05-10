library(wetlandACAD)
library(shiny)
library(dplyr)
library(leaflet)
library(shinyjs)
library(tidyr)

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
  
  
  # Plot wetland points on map
  # Select data to map on plot
  MapData<-reactive({
    if(input$DataGroup=='vmmi'){
      df<-vmmimap %>% select(Site_Type,Label,Latitude,Longitude,Mean_C,Pct_Cov_TolN,Sphagnum_Cover,Invasive_Cover,VMMI,VMMI_Rating)
    } else {
      df<-sppmap %>% select(Site_Type,Label,Latitude,Longitude,Latin_Name,Common,PctFreq,Ave_Cov)}
    return(df)
  })
  
  observe({
    if(input$DataGroup == 'vmmi'){
      colorData<-MapData()$VMMI_Rating
      pal<-colorFactor(palette=c('green','yellow','FireBrick'), domain = colorData)
    #  pop<-paste("<b>","Site:", ifelse(MapData()$Site_Type=='Sentinel', paste0(MapData()$Label," (Sentinel)"),
    #                                     paste0(MapData()$Label)),"</b>", "<br>",
    #               "Mean C:", round(MapData()$Mean_C,1), "<br>",
    #               "% Cover Tol Spp:", round(MapData()$Pct_Cov_TolN,1), "<br>",
    #               "% Bryophyte:", round(MapData()$Sphagnum_Cover,1), "<br>",
    #               "% Invasive:", round(MapData()$Invasive_Cover,1), "<br>",
    #               "VegMMI Score:", round(MapData()$VMMI,1),"<br>",
    #               "VegMMI Rating:", MapData()$VMMI_Rating, "<br>") 
    } else if (input$DataGroup == 'spplist'){
      colorData<-unique(MapData()$Site_Type)
      pal<-colorFactor(palette=c('DodgerBlue','ForestGreen'), domain = colorData)
     # pop<-paste("<b>","Site:", ifelse(MapData()$Site_Type=='Sentinel', paste0(MapData()$Label," (Sentinel)"),
    #                                   paste0(MapData()$Label)),"</b>", "<br>","Spp List Not Yet Functional")
    }
    
    leafletProxy("WetlandMap") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addCircleMarkers(data=MapData(), radius=10, 
                       lng = MapData()$Longitude, lat = MapData()$Latitude, layerId=MapData()$Label,
                       fillColor = ~pal(colorData), 
                       fillOpacity = 1, weight = 1.5, color= "DimGrey"#, popup=pop
      ) %>% 
      addLegend('bottomleft',pal=pal,values=colorData)
    
  })
  
  observeEvent(input$WetlandMap_marker_click, {
    MarkerClick<-input$WetlandMap_marker_click
    site<-MapData()[MapData()$Label==MarkerClick$id,]
    
    tempdata<-if(input$DataGroup=='vmmi'){
      
      vmmimap %>% filter(Label==MarkerClick$id) %>% select(Mean_C:VMMI_Rating) #%>% gather('Metric','Value')
      
              } else {
                
        sppmap %>% filter(Label==MarkerClick$id) %>% select(Latin_Name, Common)}
    
    content<-paste0("<b>",h4("Site: ", ifelse(site$Site_Type=='Sentinel', paste0(site$Label," (Sentinel)"),
                                              paste0(site$Label))),"</b>", "<br>",
                    
                 if (input$DataGroup=='vmmi') {                                                         
                    tagList(tags$table(class='table',
                        tags$thead(tags$th('Metric'), tags$th("Values")), 
                        tags$tbody(
                      mapply(FUN=function(Name,Value){
                        tags$tr(
                          tags$td(sprintf("%s: ", Name)),
                          tags$td(align='right',sprintf("%s", Value))
                        )
                      },
                      Name=names(tempdata),
                      Value=tempdata,SIMPLIFY=FALSE
                      ))))
                   
                  } else if (input$DataGroup=='spplist'){
                    tagList(tags$table(
                        tags$thead(tags$th('Latin Name'), tags$th('Common')),
                        tags$tbody(                          
                      mapply(FUN=function(Latin,Common){
                        tags$tr(
                          tags$td(sprintf("%s ", Latin)),
                          tags$td(aling='right',sprintf("%s", Common))
                        )
                      },
                      Latin=tempdata$Latin_Name,
                      Common=tempdata$Common, SIMPLIFY = FALSE
                      )))) 
                      }
                      
                    )
    
    leafletProxy("WetlandMap") %>% 
      clearPopups() %>% 
      addPopups(map=., lat=site$Latitude, lng=site$Longitude, layerId="MarkerClickPopup",popup=content)
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

