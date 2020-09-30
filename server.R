library(wetlandACAD)
library(shiny)
library(dplyr)
library(leaflet)
library(shinyjs)
library(tidyr)
library(htmltools)
library(DT)
library(ggplot2)
#library(leaflet.extras)

server <- function(input, output) {
  #-----------------------------
  # Wetland Map Controls
  #-----------------------------
  
  # About map button
  observe ({
    ### Maps  
    onclick(id = "aboutMapButton", expr = toggle(id = "aboutMapPanel"))
    onclick(id = "CloseaboutMap", expr = toggle(id = "aboutMapPanel")) 
    onclick(id = "aboutHydroButton", expr = toggle(id = "aboutHydroPanel"))
    onclick(id = "CloseaboutHydro", expr = toggle(id = "aboutHydroPanel"))
    onclick(id = "aboutSppButton", expr = toggle(id = "aboutSppPanel"))
    onclick(id = "CloseaboutSpp", expr = toggle(id = "aboutSppPanel"))
    
  })
  
  # Render wetland map
  output$WetlandMap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>% 
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
        baseGroups = c("OpenStreet", "Map", "Imagery", "Slate"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      setView(
        lng = -68.312,
        lat = 44.25,
        #lng = mean(-68.711,-67.953),
        #lat = mean(44.484, 43.953),
        zoom = 10
      ) %>%
      setMaxBounds(
        lng1 = -69,
        lng2 = -67.5,
        lat1 = 44.9,
        lat2 = 43.5
      ) 
  })
  
  # Make NPS map Attribution
  NPSAttrib <-
    HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  # Select data to map on plot
  MapData <- reactive({
    df<-switch(input$DataGroup,
           "vmmi" = vmmimap %>% select(Site_Type, Label, Latitude, Longitude, Mean_C, Pct_Cov_TolN, 
                                       Sphagnum_Cover, Invasive_Cover, VMMI, VMMI_Rating),
           
           "sitetype" = sitemap %>% select(Site_Type, Label, Latitude, Longitude, Year, HGM_Class,
                                           HGM_Subclass, Cowardin_Class),
           
           "spplist"= 
             switch(input$SppType,
                    "allspp"=
                      if(input$Species !='Select a species'){
                        spplist %>% select(Site_Type, Label, Latitude, Longitude, Year,
                                           Latin_Name, Present, HGM_Class:Cowardin_Class) %>% 
                          filter(Latin_Name %in% input$Species)     
                      } else {
                        sitemap %>% select(Site_Type, Label, Latitude, Longitude, Year, HGM_Class,
                                           HGM_Subclass, Cowardin_Class)
                      },
                    "invspp"=     
                      sppinv %>% select(Site_Type, Label, Latitude, Longitude, inv_present) %>% 
                      droplevels()
                     )
           )
  })
  
  # Create reactive palette
  pal <- reactive({
    if (input$DataGroup == 'vmmi') {
      pal <- colorFactor(palette = c('green', 'yellow', 'FireBrick'), 
                         levels=c('Good','Fair','Poor'))} 
    if (input$DataGroup == 'sitetype') {
      pal <- colorFactor(palette = c("DodgerBlue","ForestGreen"), domain=c('Sentinel','RAM'))} 
    
    if (input$DataGroup == 'spplist') {
      
      if(input$SppType == 'allspp' & input$Species!='Select a species'){
        pal <- colorFactor(palette = c("DimGrey","green"), domain=c('Absent','Present'))}
      
      if(input$SppType == 'allspp' & input$Species=='Select a species'){
        pal <- colorFactor(palette = c("DodgerBlue","ForestGreen"), domain=c('Sentinel','RAM'))}    
      
      if(input$SppType == 'invspp'){
        pal <- colorFactor(palette=c('DimGrey','green'), levels=c('Absent','Present'))}
    }
    return(pal)
  })
  
  # Create reactive ColorData 
  colorData <- reactive({
    if (input$DataGroup == 'vmmi') {
      colorData <- MapData()$VMMI_Rating}
    
    if (input$DataGroup == 'sitetype') {
      colorData <- MapData()$Site_Type} 
    
    if (input$DataGroup == 'spplist') {
      
      if(input$SppType == 'allspp' & input$Species!='Select a species'){
        colorData <- MapData()$Present}
      
      if(input$SppType == 'allspp' & input$Species=='Select a species'){
        colorData <- MapData()$Site_Type}
      
      if(input$SppType == 'invspp'){
        colorData <- MapData()$inv_present}
    }
    return(colorData)
  })
  
  # Observe the zoom level of the map to later toggle plot names on/off based on zoom
  observeEvent(input$WetlandMap_zoom, {
      })
  
  # Set up data, initial map, color palette, and filter for colors on map
  observe({
    req(input$WetlandMap_zoom)
    
    leafletProxy("WetlandMap") %>%
      clearPopups() %>% 
      clearControls() %>%
      addCircleMarkers(
        data = MapData(),
        radius = 10,
        lng = MapData()$Longitude,
        lat = MapData()$Latitude,
        layerId = MapData()$Label,
        label = if(input$WetlandMap_zoom > 12) MapData()$Label else NULL,
        labelOptions = labelOptions(noHide=T, textOnly = TRUE, direction = 'bottom', textsize = "12px"),
        fillColor = pal()(colorData()),
        fillOpacity = 0.75,
        weight = 1.5,
        color = "DimGrey"
        ) %>%
      addLegend('bottomleft', pal = pal(), values = colorData())

    output$Photo_N<-renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$Photo_E<-renderText({c('<p> </p>')})
    output$Photo_S<-renderText({c('<p> </p>')})
    output$Photo_W<-renderText({c('<p> </p>')})
    
  })
  
  # Reset view of map panel
  observeEvent(input$reset_view, {
    input$DataGroup
    reset("plotZoom")
    reset("DataGroup")
    
    leafletProxy("WetlandMap") %>% 
      clearPopups() %>%
      clearControls() %>% 
      setView(
        lng = -68.312,
        lat = 44.25,
        zoom = 10
      ) 

    output$Photo_N<-renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$Photo_E<-renderText({c('<p> </p>')})
    output$Photo_S<-renderText({c('<p> </p>')})
    output$Photo_W<-renderText({c('<p> </p>')})
  })
  
  # Set up popups for vmmi ratings or species list
  observeEvent(input$WetlandMap_marker_click, {
    MarkerClick <- input$WetlandMap_marker_click
    site <- MapData()[MapData()$Label == MarkerClick$id, ]

    tempdata <- 
        if (input$DataGroup == 'vmmi') {
           vmmimap %>% filter(Label == MarkerClick$id) %>% 
                       select(Mean_C:VMMI_Rating) %>% droplevels()} 
        
        else if(input$DataGroup=='sitetype'){
           sitemap %>% filter(Label == MarkerClick$id) %>% 
                      select(Site_Type, Year, HGM_Class, HGM_Subclass, Cowardin_Class)}

        else if(input$DataGroup=='spplist' & input$SppType=='invspp'){
            sppmap %>% filter(Label == MarkerClick$id) %>% 
            mutate(species=ifelse(Invasive==TRUE, paste(Latin_Name),paste('No invasives'))) %>% 
            select(species) %>% unique() %>% droplevels()}
          

    content <-
      paste0("<b>", h4("Site: ",if(site$Site_Type == 'Sentinel'){paste0(site$Label, " (Sentinel)")
          } else { if(site$Site_Type == 'RAM'){paste0(site$Label)}}), "</b>",
      if (input$DataGroup == 'vmmi') {
        tagList(tags$table(
          class = 'table',
          tags$thead(tags$th('Metric'), tags$th("Values")),
          tags$tbody(
            mapply(FUN = function(Name, Value) {
                tags$tr(tags$td(sprintf("%s: ", Name)),
                        tags$td(align = 'right', sprintf("%s", Value)))
              },
              Name = names(tempdata[,1:6]),
              Value = tempdata[,1:6],
              SIMPLIFY = FALSE ) #end of mapply 
            ) #end of tags$tbody          
          ) # end of tags$table
        ) #end of tagList
      },
      
      if (input$DataGroup=='sitetype'){
        paste0(h5("HGM Class:", paste0(site$HGM_Class)), 
               h5("HGM Subclass:", paste0(site$HGM_Subclass)),
               h5("Cowardin:", paste0(site$Cowardin_Class)),
               h5("Sample Year:", paste0(site$Year)))},
      
      if (input$DataGroup == 'spplist'){
        
        if(input$SppType=='allspp'){
         paste0(h5("HGM Class:", paste0(site$HGM_Class)), 
               h5("HGM Subclass:", paste0(site$HGM_Subclass)),
               h5("Cowardin:", paste0(site$Cowardin_Class)),
               h5("Sample Year:", paste0(site$Year)))} 
        
        else if(input$SppType=='invspp'){
          paste0(h5("Invasive Detections:", br(),
                    paste0(
                    if(nrow(tempdata)==1){paste("None detected")}
            else if(nrow(tempdata)>0) {paste(tempdata %>% 
                                                filter(species!= 'No invasives') %>% 
                                                droplevels() %>% select(species) %>% unlist(), 
                                             collapse=", ")}
          ) 
          )
          )
             }  
        
        }

      ) # end of paste0
    
    photoN<- as.character(vmmimap %>% filter(Label == MarkerClick$id) %>% 
                            mutate(photoN = paste0(North_View, '.gif')) %>%  
                            select(photoN) %>% droplevels())
    
    output$Photo_N <- renderText({c('<img src="',photoN,'" height="250"/>')})
    
    
    photoE<- as.character(vmmimap %>% filter(Label == MarkerClick$id) %>% 
                            mutate(photoE = paste0(East_View, '.gif')) %>%  
                            select(photoE) %>% droplevels())
    
    output$Photo_E <- renderText({c('<img src="',photoE,'" height="250"/>')})
    
    photoS<- as.character(vmmimap %>% filter(Label == MarkerClick$id) %>% 
                            mutate(photoS = paste0(South_View, '.gif')) %>%  
                            select(photoS) %>% droplevels())
    
    output$Photo_S <- renderText({c('<img src="',photoS,'" height="250"/>')})
    
    photoW<- as.character(vmmimap %>% filter(Label == MarkerClick$id) %>% 
                            mutate(photoW = paste0(West_View, '.gif')) %>%  
                            select(photoW) %>% droplevels())
    
    output$Photo_W <- renderText({c('<img src="',photoW,'" height="250"/>')})
    
    leafletProxy("WetlandMap") %>%
      clearPopups() %>%
      addPopups(
        lat = site$Latitude,
        lng = site$Longitude,
        popup = content
      ) 
  })
  
  # Set up ability to zoom to given plot
  observeEvent(input$plotZoom, {
    req(input$plotZoom)
    
    plot_selected <- MapData() %>% filter(Label == input$plotZoom) %>%  droplevels()
    
    output$Photo_N<-renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$Photo_E<-renderText({c('<p> </p>')})
    output$Photo_S<-renderText({c('<p> </p>')})
    output$Photo_W<-renderText({c('<p> </p>')})
    
    leafletProxy('WetlandMap') %>% 
      clearControls() %>%
      clearPopups() %>% 
      setView(
        lng =  plot_selected$Longitude, 
        lat = plot_selected$Latitude, 
        zoom = 16) 
    delay(400, leafletProxy("WetlandMap") %>% 
      addCircles(
        lng = plot_selected$Longitude,
        lat = plot_selected$Latitude,
        layerId = plot_selected$Label,
        group = 'pulse',
        radius = 19,
        color = '#00ffff',
        fillOpacity = 0,
        weight = 5)) 
    delay(1000, 
    leafletProxy('WetlandMap') %>% 
      clearShapes())
  })
  

  # Download data button
  output$downloadData <- downloadHandler(
    req(input$DataGroup),
    filename = function() {
      paste(input$DataGroup, ".csv", sep="")
    }, 
    content = function(file){
      write.csv(MapData(), file, row.names=F)
    }
  )

  # End of controls for Wetland Map
  
  #-------------------------------
  # Hydrograph Plot Controls
  #-------------------------------
  # Render hydroplot
  plotInput <- reactive({
    plot_hydro_site_year(
      df = welld,
      yvar = input$SentSite,
      years = input$Years,
      site = NULL#as.character(sentsites$sitename[sentsites$well ==
    #                                           input$SentSite])
    )
      })

  output$hydroPlot <- renderPlot({
    plotInput()
  })  

  output$info <- renderTable({
    req(input$plot_brush)
    yvar = input$SentSite
    brushedPoints(welld[,c('timestamp','year','doy_h', yvar, 'precip_cm')], 
               input$plot_brush, xvar = "doy_h", yvar = input$SentSite)
    
      }, rownames=T)
  
  sentSiteName <- reactive({
    req(input$SentSite)
    ssname<- as.character(sentsites$sitename[sentsites$well == input$SentSite])
    return(ssname)
  })

  output$sentSiteTitle <- renderText({paste0(sentSiteName())})
  
  # Download hydrograph button
  output$downloadHydroPlot <- downloadHandler(
    filename = function() {
      paste0(sub(" ", "_", as.character(sentsites$sitename[sentsites$well ==
                                               input$SentSite])), "_",
             ifelse(length(input$Years)>1, paste0(range(input$Years)[1], "-", range(input$Years)[2]),
                    paste0(input$Years[1])), ".jpeg")
    }, 
    content = function(file){
      ggsave(file, plot = plotInput(), width = 14, height = 10, device = 'jpeg')
    }
  )
  
  # Reactive hydro stats dataset for hydroTable
  
  hydroData <- reactive({
     df <- well_stats %>% 
       filter(site == input$SentSite, 
              year %in% input$Years, 
              metricLab == input$metric) %>% 
       mutate(value = round(value, 2)) %>% 
       droplevels() 
     return(df)
  })
  
  # Reactive title for hydroTable
  output$tableTitle <- renderText({
    paste0("Table: ",unique(hydroData()$metricLab), 
           " for ",
           unique(hydroData()$Label))
  })
    
  # JS code to overwrite default DT to plot a top border
  headerCallback <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('border-top', '2px solid black');",
    "}"
  )
  
  # Render hydro stats table
  output$hydroTable<-renderDT(
    hydroData()[,c("year","value")],
    #caption = paste0(unique(hydroData()$metricLab), 
    #                 " for ", unique(hydroData()$Label)),
    class = "display nowrap compact",
    rownames = FALSE,
    options = list(
      headerCallback = JS(headerCallback),
      columns = list(
        list(title = 'Year'),
        list(title = 'Value')
      ),
      columnDefs = list(list(className = 'dt-center', targets="_all")),
      autoWidth = FALSE,
      columnDefs = list(list(targets=c(0), visible=TRUE, width='30%'),
                   list(targets=c(1), visible=TRUE, width='60%')),
      searching = FALSE,
      dom = 't',
      scrollX = TRUE)) 
  
  # Download hydro data button
  output$downloadHydroData <- downloadHandler(
    filename = function() {
      paste("well_stats", ".csv", sep="")
    },
    content = function(file){
      write.csv(well_stats, file, row.names=F)
    }
  )
  

  #-------------------------------
  # Species List Tab Controls
  #-------------------------------
  # Make reactive spp list
  spptable<-reactive({
     spplisttbl<- sppmap %>% filter(Label==input$WetlandSite) %>% 
       mutate(Invasive = ifelse(Invasive==FALSE,paste("No"), paste("Yes"))) %>% 
       select(Latin_Name, Common, Invasive) %>% droplevels()
     return(spplisttbl)
  })
  
  # render data table
  output$SpeciesList <- renderDT(
    spptable(),
    class = "display nowrap compact",
    rownames = FALSE,
    options = list(
      headerCallback = JS(headerCallback),
      columns = list(
        list(title = 'Latin Name'),
        list(title = 'Common Name'),
        list(title = "Invasive?")),
      columnDefs = list(list(className = 'dt-center', targets="_all")),
      autoWidth = FALSE,
      columnDefs = list(list(targets=c(0), visible=TRUE, width='50%'),
                        list(targets=c(1), visible=TRUE, width='35%'),
                        list(targets=c(2), visible=TRUE, width='15%')),
      searching = FALSE,
      dom = 't',
      scroller = TRUE, 
      scrollX = T, 
      pageLength = 150) # Number of elements to allow on one page 
  
  )
  
  # download button for site-level species list
  # Download hydro data button
  output$downloadSpeciesData <- downloadHandler(
    filename = function() {
      paste0(sub(" ", "_", input$WetlandSite), "_",
       "species_list", ".csv", sep="")
    },
    content = function(file){
      write.csv(spptable(), file, row.names=F)
    }
  )
  
  
}
