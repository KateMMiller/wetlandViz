library(shiny)
library(leaflet)

ui<-shinyUI(
  navbarPage(
    title = HTML(
      "<div> <a href='https://www.nps.gov/im/netn/'> <img src='ah_small_black.gif',
      alt='Forest Visualizer'> </a> NETN Wetland Data Visualizer</div>"
    ),
    position = "static-top", inverse = TRUE, collapsible = FALSE, fluid = TRUE,
    windowTitle = "NETN Wetland Data Visualizer", id = "MainNavBar",
    
    #--------------------------------------------------------------
    #          For Map Tab
    #--------------------------------------------------------------
    tabPanel(
      h4("Map", align = 'center'), style = "padding: 0", 
      useShinyjs(),
      div(class = 'outer', tags$head(includeCSS("./www/mapstyles.css")),
          tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js")
          )),
      fluidRow(
        column(2, style = 'padding: 0 0 0 10px',
          div(id = "MapPanel", h4('Map Controls', class = 'panel-heading'),
            tags$div(title = 'Choose data type to view',
              selectInput(
                inputId = 'DataGroup',
                label = 'Type of data:',
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
                             tags$div(title = "Select a species",
                               selectInput(
                                 inputId = "Species",
                                 label = 'Select a species:',
                                 choices = c('Select a species', spplistall)
                               )
                             )),
            downloadButton("downloadData", "Download")
          
            )
        ),
        column(10, style = "padding: 20px 20px",
               div(leafletOutput("WetlandMap", height = "600px")
               )), br(),
        column(3, style= "padding: 5px 10px", htmlOutput(outputId="Photo_N")),
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
          div(id = "HydrographControlPanel", class = "panel-heading"),
          tags$div(title = 'Choose the site to plot',
            selectizeInput(
              inputId = "SentSite",
              label = "Sentinel Site:",
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
          tags$div(title = 'Choose years to plot',
            selectizeInput(
              inputId = "Years",
              label = "Years to plot:",
              multiple = T,
              choices = as.character(unique(welld$year)),
              selected = 2013:2018)
          )
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
      useShinyjs(),
      div(class = "outer", tags$head(HTML('<link rel="icon", href="ah_small_black.gif", type="image/gif" />'))),
      
      fluidRow(
        column(3, style = "padding: 0 0 0 20px",
               div(id = "AppListControlPanel", class = "panel-heading"),
               tags$div(title = 'Choose the site to plot',
                        selectizeInput(
                          inputId = "WetlandSite",
                          label = "Wetland Site:",
                          choices = plotlist, selected = NULL)
               )
        ),
        #end sidebarpanel
        mainPanel(width=9, style='padding: 0 0 0 10px',
          h4("Species Lists by Site"),
          tableOutput("SpeciesList")
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
