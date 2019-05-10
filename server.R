library(wetlandACAD)
library(shiny)
library(dplyr)
library(leaflet)
library(shinyjs)
library(tidyr)

shinyServer <- function(input, output, session) {
  #-----------------------------
  # Wetland Map Controls
  #-----------------------------
  
  # Render wetland map
  output$WetlandMap <- renderLeaflet({
    leaflet() %>%
      setView(
        lng = mean(-68.711,-67.953),
        lat = mean(44.484, 43.953),
        zoom = 10
      ) %>%
      setMaxBounds(
        lng1 = -68.711,
        lng2 = -67.953,
        lat1 = 44.484,
        lat2 = 43.953
      )
  })
  
  # Plot wetland points on map
  # Select data to map on plot
  MapData <- reactive({
    if (input$DataGroup == 'vmmi') {
      df <- vmmimap %>% select(Site_Type, Label, Latitude, Longitude, Mean_C, Pct_Cov_TolN, 
                               Sphagnum_Cover, Invasive_Cover, VMMI, VMMI_Rating)
    } else {
      if(input$DataGroup == 'spplist' & input$Species == 'All'){
      df <- sppfull %>% select(Site_Type, Label, Latitude, Longitude, Latin_Name, Present, HGM_Class:Cowardin_Class)
      
      } else {
        
        if(input$DataGroup == 'spplist' & input$Species !='All'){
      df<- sppfull %>% select(Site_Type, Label, Latitude, Longitude, Latin_Name, Present, HGM_Class:Cowardin_Class) %>% 
            filter(Latin_Name %in% input$Species) %>% droplevels() 
        }
      }
    } 
    return(df)
  })
  
  # Set up color palette and filter for colors on map
  observe({
    if (input$DataGroup == 'vmmi') {
      colorData <- MapData()$VMMI_Rating
      pal <- colorFactor(palette = c('green', 'yellow', 'FireBrick'), levels=c('Good','Fair','Poor'))
      
    } else if (input$DataGroup == 'spplist') {
      
      if(input$Species == 'All'){
      colorData <- MapData()$Site_Type
      pal <- colorFactor(palette = c("DodgerBlue","ForestGreen"), domain=c('Sentinel','RAM'))}
      
      if(input$Species != 'All'){
      colorData <- MapData()$Present
      pal <- colorFactor(palette = c('DimGrey', "green"), levels=c("Absent","Present"))
      }
      
    }
    
    leafletProxy("WetlandMap") %>%
      clearShapes() %>%
      clearControls() %>%
      addCircleMarkers(
        data = MapData(),
        radius = 10,
        lng = MapData()$Longitude,
        lat = MapData()$Latitude,
        layerId = MapData()$Label,
        fillColor = ~ pal(colorData),
        fillOpacity = 0.75,
        weight = 1.5,
        color = "DimGrey"#, popup=pop
      ) %>%
      addLegend('bottomleft', pal = pal, values = colorData)
    
  })
  
  # Set up popups for vmmi ratings or species list
  observeEvent(input$WetlandMap_marker_click, {
    MarkerClick <- input$WetlandMap_marker_click
    site <- MapData()[MapData()$Label == MarkerClick$id, ]
    
    tempdata <- if (input$DataGroup == 'vmmi') {
      vmmimap %>% filter(Label == MarkerClick$id) %>% select(Mean_C:VMMI_Rating) %>% droplevels()
      
    } else {
      sppmap %>% filter(Label == MarkerClick$id) %>% 
        select(Latin_Name, Common, HGM_Class:Cowardin_Class) %>% droplevels()
    }
    
    content <-
      paste0("<b>", h4("Site: ",if(site$Site_Type == 'Sentinel'){paste0(site$Label, " (Sentinel)")
          } else { if(site$Site_Type == 'RAM'){
          paste0(site$Label)}}
      ), "</b>",
      if (input$DataGroup == 'vmmi') {
        tagList(tags$table(
          class = 'table',
          tags$thead(tags$th('Metric'), tags$th("Values")),
          tags$tbody(
            mapply(
              FUN = function(Name, Value) {
                tags$tr(tags$td(sprintf("%s: ", Name)),
                        tags$td(align = 'right', sprintf("%s", Value)))
              },
              Name = names(tempdata),
              Value = tempdata,
              SIMPLIFY = FALSE
            )
          )
        ))
      } else {
      if (input$DataGroup == 'spplist'){
        paste0(
        h5("HGM Class:", paste0(site$HGM_Class)), 
        h5("HGM Subclass:", paste0(site$HGM_Subclass)),
        h5("Cowardin:", paste0(site$Cowardin_Class))
        )}
      }
      ) # end of paste0
    
    
    leafletProxy("WetlandMap") %>%
      clearPopups() %>%
      addPopups(
        map = .,
        lat = site$Latitude,
        lng = site$Longitude,
        layerId = "MarkerClickPopup",
        popup = content
      )
  })
  
  # Make Attribution
  NPSAttrib <-
    HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  # #Add a tile layer
  observe({
    leafletProxy("WetlandMap") %>%
      clearTiles() %>%
      addTiles(
        group = "Map",
        urlTemplate = "//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
        attribution = NPSAttrib,
        options = tileOptions(minZoom = 8)
      ) %>%
      addTiles(
        group = "Imagery",
        urlTemplate = "//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
        attribution = NPSAttrib,
        options = tileOptions(minZoom = 8)
      ) %>%
      addTiles(
        group = "Slate",
        urlTemplate = "//{s}.tiles.mapbox.com/v4/nps.68926899,nps.502a840b/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
        attribution = NPSAttrib,
        options = tileOptions(minZoom = 8)
      ) %>%
      addLayersControl(
        map = .,
        baseGroups = c("Map", "Imagery", "Slate"),
        options = layersControlOptions(collapsed = T)
      )
  }) 
  
  # End of controls for Wetland Map
  
  #-------------------------------
  # Hydrograph Plot Controls
  #-------------------------------
  # Render hydroplot
  output$hydroPlot <- renderPlot({
    plot_hydro_site_year(
      df = welld,
      yvar = input$SentSite,
      years = input$Years,
      site = as.character(sentsites$sitename[sentsites$well ==
                                               input$SentSite])
    )
  }) # End Hydrograph renderPlot
  
  #-------------------------------
  # Species List Tab Controls
  #-------------------------------
  spptable<-reactive({
     spplisttbl<- sppmap %>% filter(Label==input$WetlandSite) %>% select(Latin_Name, Common) %>% droplevels()
     return(spplisttbl)
  })
  
  output$SpeciesList <- renderTable({
    spptable()
  }, width='80%') # End of Species List Tab
  
}
