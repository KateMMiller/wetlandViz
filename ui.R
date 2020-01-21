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
          tags$head(includeCSS("./www/mapstyles.css")),
          tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
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
                            )
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
                       choices = c('Select a site', plotList  
                       )
                     )
            ),
            actionButton('reset_view', "Reset Map", class='btn btn-primary'),br(),
            tags$head(tags$style(".button{font-size: 12px;}")),
            downloadButton("downloadData", "Download Data", class="btn btn-primary"),br()
            )
        ),
        column(10, style = "padding: 20px 40px", 
               div(leafletOutput("WetlandMap", height = "600px")
               )), br(),
        
        h5(column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_N"))),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_E")),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_S")),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_W"))
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
            tags$div(title = 'Choose the site to plot'),
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
              ),selected = NULL)
          ),
          tags$style(type='text/css', ".selectize-input { font-size: 14px;} 
                     .selectize-dropdown { font-size: 14px; }"),
          tags$div(title = 'Choose years to plot',
            selectizeInput(
              inputId = "Years",
              label = h4("Years to plot:"),
              multiple = T,
              choices = as.character(unique(welld$year)),
              selected = 2013:2019)
          ),
          downloadButton("downloadHydroPlot", "Download Hydrograph", class="btn btn-primary"),
          tags$div(title = 'Growing season summary',
                   tags$hr(style="border-color: black;"),
                   h4('Growing Season Statistics:'),
              selectizeInput(inputId = "metric",
                             label = span("Select a statistic", style = "font-size:15px"),
                             choices = unique(well_stats$metricLab),
                             selected = "Mean Water Level (cm)"),
              h5(textOutput('tableTitle')),
              span(dataTableOutput('hydroTable'), style = "font-size:15px")),
          br(),
          downloadButton("downloadHydroData", "Download Water Level Stats", class="btn btn-primary")
        ),
        #end sidebarpanel
       # mainPanel(
       column(10,
          h4("Wetland Hydrographs"),
          plotOutput("hydroPlot", width = '100%', height = "700px")
        )
      ) # end fluidpage
    ),
    # end tabPanel Hydrograph
    
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
               )),
               downloadButton("downloadSpeciesData", "Download Species List", class="btn btn-primary")
        ),
        #end sidebarpanel
        mainPanel(width=7, style='padding: 0 0 0 10px',
          h4("Species Lists by Site"),
          span(dataTableOutput('SpeciesList'), style = "font-size:14px;")
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
