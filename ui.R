library(shiny)
library(leaflet)
library(DT)

ui<-shinyUI(
  navbarPage(
    title = HTML(
      "<div> <a href='https://www.nps.gov/im/netn/'> <img src='ah_small_black.gif',
      alt='Forest Visualizer'> </a> NETN Wetland Data Visualizer</div>"
    ),
    position = "static-top", inverse = TRUE, collapsible = FALSE, fluid = TRUE,
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css",
    windowTitle = "NETN Wetland Data Visualizer", id = "MainNavBar",
    
    #--------------------------------------------------------------
    #          For Map Tab
    #--------------------------------------------------------------
    tabPanel(
      h4("Map", align = 'center'), style = "padding: 0", 
      useShinyjs(),
      div(class = 'outer', 
          tags$head(includeCSS("./www/mapstyles.css"))#,
          #tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
          #tags$head(includeCSS("https://www.nps.gov/lib/npmap.js/4.0.0/npmap.min.css")),
          #tags$head(includeScript("https://www.nps.gov/lib/npmap.js/4.0.0/npmap-bootstrap.min.js"))
          ),
      fluidRow(
        column(2, style = 'padding: 0 0 0 10px',
          div(id = "MapPanel", class="panel panel-default controls",
              h4('Map Controls', class = 'panel-heading'),
              tags$style(type='text/css', ".selectize-input{font-size: 12px;} 
                                           .selectize-dropdown{font-size: 12px;}"),
            tags$div(title = 'Choose data type to view',
              selectizeInput(
                inputId = 'DataGroup',
                label = h5('Type of data:'),
                choices = c('Veg MMI' = 'vmmi', 
                            'By Site Type' = 'sitetype',
                            'By Species' = 'spplist' 
                            ),
                selected="Veg MMI"
              )
            ),
            conditionalPanel(condition = "input.DataGroup=='spplist'",
                             radioButtons(inputId='SppType', label='Species type:',
                                          choices=c('All species'='allspp', 
                                                    'Invasive species'='invspp')
                             )
                             ),
            conditionalPanel(condition = "input.DataGroup=='spplist' & input.SppType=='allspp'",
                             tags$style(type='text/css', ".selectize-input {font-size: 14px;} 
                                        .selectize-dropdown {font-size: 14px;}"),
                             tags$div(title = "Select a species",
                               selectizeInput(
                                 inputId = "Species",
                                 label = 'Select a species:',
                                 choices = c('Select a species', spplistall)
                               )
                             )),
            tags$style(type='text/css', ".selectize-input { font-size: 14px;} 
                       .selectize-dropdown { font-size: 14px; }"),
            tags$div(title = 'Zoom to a Site',
                     selectizeInput(
                       inputId = 'plotZoom',
                       label = h5('Zoom to a Site:'),
                       choices = c('Select a site', plotlist  
                       )
                     )
            ),
            tags$div(title = "Reset the Map",
            actionButton('reset_view', "Reset Map", 
                         style="color:white;background-color: #5F9EA0; 
                         border-color:#436e70;font-size:11px")),
            tags$div(title = "Download the map data",
            downloadButton("downloadData", "Download Data", 
                           style="padding:0 1 0 5px;;color:black;background-color:#DCDCDC;
                                  border-color:#484848;font-size:11px;")),
            tags$div(title = "About the Map",
            actionButton(inputId="aboutMapButton",class="btn btn-primary",
                         style = "border-color:#5c4023; background-color:#a6763f; 
                            padding:0 1 0 5px;font-size:11px;",
                          label="About the map"))
                  )),
        
        column(10, style = "padding: 20px 40px", 
               tags$div(title = "Map of wetland sites",
               div(leafletOutput("WetlandMap", height = "600px")
               ))), br(),
        # hidden(
          fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",
                     top = 100, bottom = "auto",
                     height = "500",
                     style="z-index:1000;border-color:#5c4023;",
                     left = 500, width = "600", id = "aboutMapPanel", style = "padding: 10px",
                     div(class = "panel-heading", h3("About the Map" ),
                         style = "background-color:#a6763f;"),
                     div(class = "panel-body", style= "height: 300px;
                         overflow-y: scroll; border-color:black;",
                         includeHTML("./www/aboutMap.html")),
                     div(class="panel-footer",
                         actionButton(inputId="CloseaboutMap",
                                      class="btn btn-default",label="Close"))  ),
        #),
        tags$div(title = "photopoints from site",
        h5(column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_N"))),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_E")),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_S")),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_W"))
        )#ends tags$div
        ) # end fluidRow
    ),#end tabPanel
    
    
    #--------------------------------------------------------------
    #          For Hydrograph Tab
    #--------------------------------------------------------------
    tabPanel(
      h4("Hydrographs", align = 'center'), style = "padding: 0",
      useShinyjs(),
      div(class = "outer", tags$head(HTML('<link rel="icon", href="ah_small_black.gif", type="image/gif" />'))),
      
      fluidRow(
        column(2, style = "padding: 0 0 0 10px",
          div(id = "HydrographControlPanel", class="panel panel-default controls",
              h4("Hydrograph Controls", class='panel-heading'),
          tags$style(type='text/css', ".selectize-input { font-size: 14px;} .selectize-dropdown { font-size: 14px; }"),
            tags$div(title = 'Choose the site to plot',
            selectizeInput(
              inputId = "SentSite",
              label = h4("Sentinel Site:"),
              choices = c(
                "Big Heath" = 'BIGH_WL',
                "Duck Pond" = "DUCK_WL",
                "Heath Brook" = "HEBR_WL",
                "Hodgdon Swamp" = "HODG_WL",
                "Gilmore Meadow" = "GILM_WL",
                "Little Hunter's Brook" = "LIHU_WL",
                "New Mill Meadow" = "NEMI_WL",
                "Western Mtn. Swamp" = "WMTN_WL"
              ), selected = "Big Heath"
          )),
          tags$style(type='text/css', ".selectize-input { font-size: 14px;} 
                     .selectize-dropdown { font-size: 14px; }"),
          tags$div(title = 'Choose years to plot',
            selectizeInput(
              inputId = "Years",
              label = h4("Years to plot:"),
              multiple = T,
              choices = as.character(unique(welld$Year)),
              selected = 2022)
          ),
          tags$div(title = "Download Hydrograph",
          downloadButton("downloadHydroPlot", "Download graph", 
                         style="padding:0 1 0 5px;color:black;background-color:#DCDCDC;
                                  border-color:#484848;font-size:11px;"))),
          tags$div(title = "Select water level statistic", 
                     class = "panel panel-default controls",
                h4('Growing Season Statistics:', class='panel-heading'),
                   tags$hr(style = "border-color: black;"),
             selectizeInput(inputId = "metric",
                             label = span("Select a statistic", style = "font-size:15px"),
                             choices = unique(well_stats$metricLab),
                             selected = "Mean Water Level (cm)")),
          tags$div(title = "Table of water level statistics", 
            h5(textOutput('tableTitle')),
              span(dataTableOutput('hydroTable'), style = "font-size:15px")),
             br(),
          tags$div(title = "Download water level statistics",
          downloadButton("downloadHydroData", "Download Stats", 
                         style="padding:0 1 0 5px;color:black;background-color:#DCDCDC;
                                  border-color:#484848;font-size:11px;")),
          tags$div(title = "About the hydrology data", 
          actionButton(inputId="aboutHydroButton",class="btn btn-primary",
                       style = "border-color:#5c4023; background-color:#a6763f; 
                            padding:4px;font-size:11px;",
                       label="About the Data"))
        ), # end sidebarpanel
      # 
      # hidden(
        fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",
                   top = 100, bottom = "auto",
                   height = "500",
                   style="z-index:1000;border-color:#5c4023;",
                   left = 500, width = "600", id = "aboutHydroPanel", style = "padding: 10px",
                   div(class = "panel-heading", h3("About the Data" ),
                       style = "background-color:#a6763f;"),
                   div(class = "panel-body", style= "height: 300px;  overflow-y: scroll",
                       includeHTML("./www/aboutHydro.html")),
                   div(class="panel-footer",
                       actionButton(inputId="CloseaboutHydro",class="btn btn-default",label="Close"))
        ),
      #), # end hidden
        
       column(10,
          tags$div(title = "Wetland Hydrograph",
          h4("Wetland Hydrographs for: ", textOutput("sentSiteTitle", inline=T))),

          tags$div(title = "Select points to view below",
                   plotOutput("hydroPlot", width = '100%', height = "700px", 
                     brush = "plot_brush")),
          h5('Data from selected points:'),
          span(tableOutput("info"), style = "font-size:13px")
       )
      ) # end fluidpage
    ),  # end tabPanel Hydrograph
    
    #--------------------------------------------------------------
    #          For Species List Tab
    #--------------------------------------------------------------
    tabPanel(
      h4("Species Lists", align = 'center'), style = "padding: 0",
      #useShinyjs(),
      #div(class = "outer", tags$head(HTML('<link rel="icon", href="ah_small_black.gif", type="image/gif" />'))),
      
      fluidRow(
        column(2, style = "padding: 0 0 0 20px",
               div(id = "SppListControlPanel", class="panel panel-default controls",
                   h4("Species Lists", class='panel-heading'),
               
               tags$div(title = 'Choose the site for species list', class="panel panel-default controls",
                        selectizeInput(
                          inputId = "WetlandSite",
                          label = h4("Wetland Site:", class="panel-heading"),
                          choices = plotlist, selected = NULL)
               ),
               tags$div(title = "Download species list",
               downloadButton("downloadSpeciesData", "Download List", 
                              style="padding:0 10 0 0px;color:black;background-color:#DCDCDC;
                                  border-color:#484848;font-size:11px;")),
               tags$div(title = "About the species lists",
               actionButton(inputId = "aboutSppButton", class = "btn btn-primary",
                            style = "border-color:#5c4023; background-color:#a6763f; 
                            padding:0 10 0 0px;font-size:11px;",
                            label = "About the Data")))
        ),
        # hidden(
          fixedPanel(class="panel panel-primary controls",draggable=TRUE,cursor="auto",
                     top = 100, bottom = "auto",
                     height = "400",
                     style="z-index:1000;border-color:#5c4023;",
                     left = 500, width = "600", id = "aboutSppPanel",
                     div(class = "panel-heading", h3("About the Data"),
                         style = "background-color:#a6763f;"),
                     div(class = "panel-body", style= "height: 250px;  overflow-y: scroll",
                         includeHTML("./www/aboutSppList.html")),
                     div(class="panel-footer",
                         actionButton(inputId="CloseaboutSpp",class="btn btn-default",label="Close"))
          ),
        #),
        #end sidebarpanel
        mainPanel(width=7, style='padding: 0 0 0 10px',
          h4("Species Lists by Site"),
          tags$div(title = "Species list by site",
          span(dataTableOutput('SpeciesList'), style = "font-size:14px;"))
        ) # end mainPanel
      ) # end fluidRow
      
    ), # end of species tab panel
    #--------------------------------------------------------------
    #          For About Tab
    #--------------------------------------------------------------
    tabPanel(
      tags$div(
      h4("About", align = 'center'), style = "padding: 0"),
      includeHTML("./www/About.html")
      # end of About list tabpanel
    )
    ) # end of navbarpage
) # end shinyUI
