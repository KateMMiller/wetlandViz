library(wetlandACAD) # from doi-nps/wetlandACAD
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
  
  # About the Data buttons
  observeEvent(input$aboutMapButton, showModal(
    modalDialog(title="About the Map", 
                footer = tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
                includeHTML("./www/aboutMap.html")                  
    )
  ))
  
  observeEvent(input$aboutHydroButton, showModal(
    modalDialog(title="About the Hydrographs", 
                footer = tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
                includeHTML("./www/aboutHydro.html")                  
    )
  ))
  
  observeEvent(input$aboutSppButton, showModal(
    modalDialog(title="About the Species Lists", 
                footer = tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
                includeHTML("./www/aboutSppList.html")                  
    )
  ))
  
  ESRIimagery <- "http://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
  ESRItopo <- "http://services.arcgisonline.com/arcgis/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
  ESRINatGeo <- "http://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}"
   

  # Render wetland map - Keep |> b/c .
  output$WetlandMap <- renderLeaflet({
    leaflet() |>
      setView(
        lng = -68.312,
        lat = 44.25,
        #lng = mean(-68.711,-67.953),
        #lat = mean(44.484, 43.953),
        zoom = 10
      ) |>
      setMaxBounds(
        lng1 = -69,
        lng2 = -67.5,
        lat1 = 44.9,
        lat2 = 43.5
      ) |> 
      #addTiles(group = "OpenStreetMap") #|> 
      addTiles(group = "Topo", urlTemplate = ESRItopo, options = tileOptions(minZoom = 8)) |>
      addTiles(group = "Imagery", urlTemplate = ESRIimagery, options = tileOptions(minZoom = 8)) |>
      addTiles(group = "NatGeo", urlTemplate = ESRINatGeo, options = tileOptions(minZoom = 8)) |>
      addLayersControl(
        #map = .,
        baseGroups = c("Topo", "Imagery", "NatGeo"),
        options = layersControlOptions(collapsed = T)
      )
  })

  
    # Select data to map on plot
  MapData <- reactive({
    df <- switch(input$DataGroup,
           "vmmi" = vmmimap |> select(Site_Type, Label, Year, Latitude, Longitude, Mean_C, Pct_Cov_TolN, 
                                       Sphagnum_Cover, Invasive_Cover, VMMI, VMMI_Rating),
           
           "sitetype" = sitemap |> select(Site_Type, Label, Year, Latitude, Longitude, Year, HGM_Class,
                                           HGM_Subclass, Cowardin_Class),
           
           "spplist"= 
             switch(input$SppType,
                    "allspp"=
                      if(input$Species !='Select a species'){
                        spplist |> select(Site_Type, Label, Year, Latitude, Longitude, Year,
                                           Latin_Name, Present, HGM_Class:Cowardin_Class) |> 
                          filter(Latin_Name %in% input$Species)     
                      } else {
                        sitemap |> select(Site_Type, Label, Year, Latitude, Longitude, Year, HGM_Class,
                                           HGM_Subclass, Cowardin_Class)
                      },
                    "invspp"=     
                      sppinv |> select(Site_Type, Label, Year, Latitude, Longitude, inv_present) |> 
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
      
      if(input$SppType == 'allspp' & input$Species != 'Select a species'){
        colorData <- MapData()$Present}
      
      if(input$SppType == 'allspp' & input$Species == 'Select a species'){
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
    
    leafletProxy("WetlandMap") |>
      clearPopups() |> 
      clearControls() |>
      addCircleMarkers(
        data = MapData(),
        radius = 10,
        lng = MapData()$Longitude,
        lat = MapData()$Latitude,
        layerId = MapData()$Label,
        label = if(input$WetlandMap_zoom > 12) MapData()$Label else NULL,
        labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = 'bottom', textsize = "12px"),
        fillColor = pal()(colorData()),
        fillOpacity = 0.75,
        weight = 1.5,
        color = "DimGrey"
        ) |>
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
    
    leafletProxy("WetlandMap") |>
      clearPopups() |>
      clearControls() |> 
      setView(
        lng = -68.312,
        lat = 44.25,
        zoom = 10
      ) 

    output$Photo_N <- renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$Photo_E <- renderText({c('<p> </p>')})
    output$Photo_S <- renderText({c('<p> </p>')})
    output$Photo_W <- renderText({c('<p> </p>')})
  })
  
  # # Set up observe for species selected from Map Panel list
  # #+++++ ENDED HERE +++++
  # observeEvent(input$Species, {
  #   req(input$Species)
  #   species_selected <- MapData() |> filter(Latin_Name %in% input$Species) |> select(Latin_Name) |>
  #     droplevels()
  # })
  
  
  # Set up popups for vmmi ratings or species list
  observeEvent(input$WetlandMap_marker_click, {
    MarkerClick <- input$WetlandMap_marker_click
    site <- MapData()[MapData()$Label == MarkerClick$id, ]
    
    tempdata <- 
        if (input$DataGroup == 'vmmi') {
           vmmimap |> filter(Label == MarkerClick$id) |> 
                       select(Mean_C:VMMI_Rating) |> droplevels()} 
        
        else if(input$DataGroup == 'sitetype'){
           sitemap |> filter(Label == MarkerClick$id) |> 
                      select(Site_Type, HGM_Class, HGM_Subclass, Cowardin_Class)}
    
        else if(input$DataGroup == 'spplist' & input$SppType == 'allspp'){
           sppmap |> filter(Label == MarkerClick$id) |> 
                     filter(Latin_Name == input$Species) |> 
                     select(species = Latin_Name, PctFreq) |> unique() |> droplevels()}

        else if(input$DataGroup == 'spplist' & input$SppType == 'invspp'){
           sppmap |> filter(Label == MarkerClick$id) |> 
                     mutate(species = ifelse(Invasive == TRUE, paste(Latin_Name), paste('No invasives'))) |> 
                     select(species) |> unique() |> droplevels()}
          

    content <-
      paste0("<b>", h4("Site: ",if(site$Site_Type == 'Sentinel'){paste0(site$Label, " (Sentinel)")
          } else {if(site$Site_Type == 'RAM'){paste0(site$Label)}}), "</b>",
          h5("Sample Year: ", unique(site$Year)),
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
              SIMPLIFY = FALSE ), #end of mapply 
            ) #end of tags$tbody          
          ) # end of tags$table
        ) #end of tagList
      },
      
      if (input$DataGroup == 'sitetype'){
        paste0(h5("HGM Class:", paste0(site$HGM_Class)), 
               h5("HGM Subclass:", paste0(site$HGM_Subclass)),
               h5("Cowardin:", paste0(site$Cowardin_Class)))},
      
      if (input$DataGroup == 'spplist'){
        
        if(input$SppType == 'allspp'){
          if(input$Species != "Select a species" & nrow(tempdata) > 0){
          paste0(h5("HGM Class:", paste0(site$HGM_Class)), 
               h5("HGM Subclass:", paste0(site$HGM_Subclass)),
               h5("Cowardin:", paste0(site$Cowardin_Class)),
               h5("Latin Name: ", paste0(tempdata$species)),
               h5("Percent Frequency:", paste0(tempdata$PctFreq)))
          } else {
            paste0(h5("HGM Class:", paste0(site$HGM_Class)), 
                   h5("HGM Subclass:", paste0(site$HGM_Subclass)),
                   h5("Cowardin:", paste0(site$Cowardin_Class)))
          }
          } 
        
        else if(input$SppType == 'invspp'){
          paste0(h5("Invasive Detections:", br(),
                    paste0(
                    if(nrow(tempdata) == 1){paste("None detected")}
            else if(nrow(tempdata) > 0) {paste(tempdata |> 
                                                filter(species!= 'No invasives') |> 
                                                droplevels() |> select(species) |> unlist(), 
                                             collapse=", ")}
          ) 
          )
          )
             }  
        
        }

      ) # end of paste0
    
    photoN <- as.character(vmmimap |> filter(Label == MarkerClick$id) |> 
                            mutate(photoN = paste0(North_View, '.JPG')) |>  
                            select(photoN) |> droplevels())
    
    output$Photo_N <- renderText({c('<img src="',photoN,'" height="250"/>')})
    
    
    photoE<- as.character(vmmimap |> filter(Label == MarkerClick$id) |> 
                            mutate(photoE = paste0(East_View, '.JPG')) |>  
                            select(photoE) |> droplevels())
    
    output$Photo_E <- renderText({c('<img src="',photoE,'" height="250"/>')})
    
    photoS<- as.character(vmmimap |> filter(Label == MarkerClick$id) |> 
                            mutate(photoS = paste0(South_View, '.JPG')) |>  
                            select(photoS) |> droplevels())
    
    output$Photo_S <- renderText({c('<img src="',photoS,'" height="250"/>')})
    
    photoW<- as.character(vmmimap |> filter(Label == MarkerClick$id) |> 
                            mutate(photoW = paste0(West_View, '.JPG')) |>  
                            select(photoW) |> droplevels())
    
    output$Photo_W <- renderText({c('<img src="',photoW,'" height="250"/>')})
    
    leafletProxy("WetlandMap") |>
      clearPopups() |>
      addPopups(
        lat = site$Latitude,
        lng = site$Longitude,
        popup = content
      ) 
  })
  
  # Set up ability to zoom to given plot
  observeEvent(input$plotZoom, {
    req(input$plotZoom)
    
    plot_selected <- MapData() |> filter(Label == input$plotZoom) |>  droplevels()
    
    output$Photo_N <- renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$Photo_E <- renderText({c('<p> </p>')})
    output$Photo_S <- renderText({c('<p> </p>')})
    output$Photo_W <- renderText({c('<p> </p>')})
    
    leafletProxy('WetlandMap') |> 
      clearControls() |>
      clearPopups() |> 
      setView(
        lng =  plot_selected$Longitude, 
        lat = plot_selected$Latitude, 
        zoom = 16) 
    delay(400, leafletProxy("WetlandMap") |> 
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
    leafletProxy('WetlandMap') |> 
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
  # Reactive WL data for hydrographs
  WLData <- reactive({
    df1 <- welld |> filter(Year %in% input$Years) 
    df <- df1[,c('timestamp','Year','doy_h', input$SentSite, 'precip_cm', 'doy', 'lag.precip')]
    colnames(df) <- c("timestamp", "Year", "doy_h", "WL", "precip_cm", "doy", 'lag.precip')
    df$precip5 <- df$lag.precip*5
    df$site = substr(input$SentSite, 1, 4)
    return(df)
  })
  
  sentSiteName <- reactive({
    req(input$SentSite)
    ssname<- as.character(sentsites$sitename[sentsites$well == input$SentSite])
    return(ssname)
  })
  
  # Render hydroplot
  # plotInput <- reactive({
  #   minWL = min(WLData()$WL, na.rm = T)
  # 
  #   p <- 
  #     ggplot(WLData(), aes(x = doy_h, y = WL, group = Year)) +
  #     geom_line(col = 'black')+
  #     geom_line(aes(x = doy_h, y = lag.precip*5 + minWL, group = Year), col ='blue')+
  #     facet_wrap(~Year, nrow = length(unique(WLData()$Year)))+
  #     geom_hline(yintercept = 0, col = 'brown')+
  #     theme_bw()+
  #     theme(plot.title = element_text(hjust = 0.5),
  #           panel.grid.minor = element_blank(),
  #           panel.grid.major = element_blank(),
  #           axis.text.y.right = element_text(color = 'blue'),
  #           axis.title.y.right = element_text(color = 'blue'),
  #           strip.text = element_text(size = 11))+
  #     labs(y = 'Water Level (cm)\n', x = 'Date')+
  #     scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274),
  #                        labels = c('May-01', 'Jun-01',
  #                                   'Jul-01', 'Aug-01',
  #                                   'Sep-01', 'Oct-01'))+
  #     scale_y_continuous(sec.axis = sec_axis(~.,
  #                                            breaks = c(minWL, minWL+10),
  #                                            name = 'Hourly Precip. (cm)\n',
  #                                            labels = c('0', '2')))
  #   # Manually coded hydroPlot for easier brush
  #   # plot_hydro_site_year(
  #   #   df = WLData(),
  #   #   yvar = "WL",
  #   #   years = input$Years,
  #   #   site = NULL)
  #   p
  #     })

  plot.range <- reactiveValues(x = NULL, y = NULL)
  
  plotInput <- reactive({
    minWL = min(WLData()$WL, na.rm = T)
    
    p <- 
      ggplot(WLData(), aes(x = doy_h, y = WL, group = Year)) +
      geom_line(aes(x = doy_h, y = precip5+minWL, group = Year), col ='blue')+
      geom_line(col = 'black')+
      facet_wrap(~Year, nrow = length(unique(WLData()$Year)))+
      geom_hline(yintercept = 0, col = 'brown')+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.y.right = element_text(color = 'blue'),
            axis.title.y.right = element_text(color = 'blue'),
            strip.text = element_text(size = 11))+
      labs(y = 'Water Level (cm)\n', x = 'Date')+
      scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274),
                         labels = c('May-01', 'Jun-01',
                                    'Jul-01', 'Aug-01',
                                    'Sep-01', 'Oct-01'))+
      scale_y_continuous(sec.axis = sec_axis(~.,
                                             breaks = c(minWL, minWL+10),
                                             name = 'Hourly Precip. (cm)\n',
                                             labels = c('0', '2'))) 
    p
    
  })
  
  output$hydroPlot <- renderPlot({
    plotInput()
  })  
  
  # observeEvent(input$plot_brush, {
  #   brush = input$plot_brush
  #   print(brush)
  #   if (!is.null(brush)) {
  #     plot.range$x <- c(brush$xmin, brush$xmax)
  #     plot.range$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     plot.range$x <- NULL
  #     plot.range$y <- NULL
  #   }
  # })

  # brushedData <- reactive({
  #   bdf <- brushedPoints(WLData()[,c("timestamp", "Year", "WL", "precip_cm", "doy_h")], 
  #                        brush = input$plot_brush,
  #                        xvar = "doy_h", 
  #                        yvar = "WL",
  #                        allRows = F)
  #  # print(head(bdf))
  #   return(bdf)})
  # 
  # output$tblinfo <- renderTable({
  #   brushedData()
  #   }, rownames = T)
  
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
  
  # Download WaterLevel button
  output$downloadWaterLevel <- downloadHandler(
    filename = function() {
      paste0(substr(input$SentSite, 1, 4), "_hourly_water_level_",
             ifelse(length(input$Years)>1, paste0(range(input$Years)[1], "-", range(input$Years)[2]),
                    paste0(input$Years[1])), ".csv")
    },
    content = function(file){
      write.csv(WLData(), file, row.names = F)
      
    }
  )

  # Reactive hydro stats dataset for hydroTable
  
  hydroData <- reactive({
     df <- well_stats |> 
       filter(site == input$SentSite, 
              Year %in% input$Years, 
              metricLab == input$metric) |> 
       mutate(value = round(value, 2)) |> 
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
  output$hydroTable <- renderDT(
    hydroData()[,c("Year","value")],
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
  spptable <- reactive({
     spplisttbl <- sppmap |> filter(Label == input$WetlandSite) |> 
       mutate(Invasive = ifelse(Invasive == FALSE,paste("No"), paste("Yes"))) |> 
       select(Latin_Name, Common, Invasive, Sample_Year = Year) |> droplevels()
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
        list(title = "Invasive?"),
        list(title = "Sample Year")),
      columnDefs = list(list(className = 'dt-center', targets="_all")),
      autoWidth = FALSE,
      columnDefs = list(list(targets = c(0), visible = TRUE, width='45%'),
                        list(targets = c(1), visible = TRUE, width='35%'),
                        list(targets = c(2), visible = TRUE, width='15%'),
                        list(targets = c(3), visible = TRUE, width = '5%')),
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
