library(shiny)
library(leaflet)

shinyUI(
  navbarPage(
    title = HTML(
      "<div> <a href='https://www.nps.gov/im/netn/'> <img src='ah_small_black.gif',
      alt='Forest Visualizer'> </a> NETN Wetland Data Visualizer</div>"
    ),
    position = "static-top",
    inverse = TRUE,
    collapsible = FALSE,
    fluid = TRUE,
    windowTitle = "NETN Wetland Data Visualizer",
    id = "MainNavBar",
    
    #--------------------------------------------------------------
    #          For Map Tab
    #--------------------------------------------------------------
    tabPanel(
      h4("Map", align = 'center'),
      style = "padding: 0",
      useShinyjs(),
      div(class='outer',
          tags$head(includeCSS("./www/mapstyles.css")),
          tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))),
      fluidRow(
        column(2, style = 'padding: 0 0 0 10px',
               div(id = "MapPanel", h4('Map Controls', class = 'panel-heading'),
                   tags$div(title='Choose data type to view', 
                            selectInput(inputId='DataGroup',
                            label='Type of data:',
                            choices=c('Veg MMI' = 'vmmi', 'By Species' = 'spplist'))))),
        column(10, style = "padding: 0",
               div(leafletOutput("WetlandMap",height="800px")))
      ) # end fluidRow
      
    
      ), #end tabPanel
    
    
    #--------------------------------------------------------------
    #          For Hydrograph Tab
    #--------------------------------------------------------------
    tabPanel(
      h4("Hydrographs", align = 'center'),
      style = "padding: 0",
      useShinyjs(),
      div(class = "outer", tags$head(
        HTML(
          '<link rel="icon", href="ah_small_black.gif", type="image/gif" />'
        )
      )),
      
      fluidRow(
        column(2, style = "padding: 0 0 0 10px", 
               div(id="HydrographControlPanel", class = "panel-heading"),
               tags$div(title='Choose the site to plot', 
                        selectizeInput(inputId="SentSite", label="Sentinel Site:", 
                                       choices=c("Big Heath" = 'BIGH_WL', "Duck Pond" = "DUCK_WL", 
                                                 "Heath Brook" = "HEBR_WL", "Hodgdon Swamp" = "HODG_WL",
                                                 "Gilmore Meadow" = "GILM_WL", "Little Hunter's Brook" = "LIHU_WL",
                                                 "New Mill Meadow" = "NEMI_WL", "Western Mtn. Swamp" = "WMTN_WL"),
                                       selected=NULL)),
               tags$div(title='Choose years to plot',
                        selectizeInput(inputId = "Years", label = "Years to plot:", multiple=T,
                                       choices=as.character(unique(welld$year)),
                                       selected=2013:2018))), #end sidebarpanel
        mainPanel(
          h4("Wetland Hydrographs"),
          plotOutput("hydroPlot", width = '100%', height = "700px")
        )
        ) # end fluidpage
    )# end tabPanel Hydrograph
    
    
    ) # end ot title
) # end shinyUI