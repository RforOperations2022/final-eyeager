# Set up
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(DT)
library(lubridate)

# Load iNaturalist Data
# Data Source: https://www.inaturalist.org/observations?d1=2021-03-01&d2=2022-03-01&iconic_taxa=Fungi&order_by=observed_on&place_id=124749&subview=map
iNat.load <- read.csv('observations-216426.csv')
iNat.load <- subset(iNat.load, quality_grade=='research')
iNat.load$month <- month(ymd(iNat.load$observed_on))

# Define UI for application
ui <- navbarPage("Pittsburgh Biodiversity",
                 theme = shinytheme("spacelab"),
                 tabPanel("Map",
                          sidebarLayout(
                              sidebarPanel(
                                  # Select Taxon Name
                                  selectInput(inputId = "taxonSelect",
                                              label = "Select taxon for markers",
                                              choices = unique(iNat.load['iconic_taxon_name']),
                                              selected = c("Fungi"),
                                              selectize = F,
                                              multiple = F),
                                  # Select month for markers
                                  sliderInput(inputId = "monthSelect",
                                              label = "Select month for markers",
                                              min = 1,
                                              max = 12,
                                              value = 6,
                                              step = 1),
                                  # Select whether heatmap
                                  checkboxInput(inputId = "heatmapSelect",
                                                label = "Draw heatmap of all taxons and months?",
                                                value = FALSE,
                                                width = '100%'),
                                  # Data download
                                  downloadButton(outputId = "downloadData", label = "Download")),
                              mainPanel(
                                  # Using Shiny JS
                                  shinyjs::useShinyjs(),
                                  # Style the background and change the page
                                  tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #8DA68A;}"),
                                  # Map Output
                                  leafletOutput("leaflet")
                              )
                          )
                 ),
                 # Data Table Panel
                 tabPanel("Data",
                          fluidPage(
                              wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
    
    values <- reactiveValues()
    
    # Basic Map
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(provider = providers$Esri.WorldGrayCanvas) %>%
            setView(-79.9959, 40.4406, 12) # %>%
    })
    
    # Filter data by taxon selection
    taxonInput <- reactive({
        subset(iNat.load, iconic_taxon_name %in% input$taxonSelect)
    })
    # Filter data by month selection
    monthInput <- reactive({
        subset(taxonInput(), month == input$monthSelect)
    })
    
    # Create reactive for heatmap checkbox
    heatmapInput <- reactive({
        if (input$heatmapSelect == TRUE) {
            iNat.load
        }
        else {
            data.frame(longitude = numeric(), latitude = numeric())
        }
    })
    
    # Replace layer with data filtered by taxon name and month
    observe({
        markerData <- monthInput()
        # Data is selected taxon and month
        leafletProxy("leaflet", data = markerData) %>%
            clearMarkers() %>%
            addAwesomeMarkers(group = "markerData", 
                              popup = ~paste0("<b>", 'User', "</b>: ", 
                                      user_login, "<br><b>", "Common Name", 
                                      "</b>: ", common_name, "<br><img src = '", 
                                      image_url, "' width='150px' heigh='150px'>"), 
                              layerId = ~id) 
    })
    
    # Replace layer with data filtered by heatmapSelect
    observe({
        heatmapData <- heatmapInput()
        # Data is all taxon and months
        leafletProxy("leaflet", data = heatmapData) %>%
            clearHeatmap() %>%
            addHeatmap(lng=~as.numeric(longitude),
                       lat=~as.numeric(latitude),
                       radius = 40,
                       blur = 100)
    })
    
    # Render data table with taxon and month selected
    output$table <- DT::renderDataTable(iNat.load, options = list(scrollX = T))
    
    # Download button
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(filename) {
            write.csv(iNat.load, filename)
        }
    )
    # # Enable button once a marker has been selected
    # observeEvent(input$leaflet_marker_click$id, {
    #     enable("delete")
    # })
    # # Add layerID to list of removed projects
    # observeEvent(input$delete, {
    #     enable("restore")
    #     isolate({
    #         values$removed <- c(values$removed, input$leaflet_marker_click$id)
    #     })
    # })
    # # Reset removed Projects
    # observeEvent(input$restore, {
    #     values$removed <- c()
    #     disable("restore")
    # })
    # # Subset to data Only on screen
    # onScreen <- reactive({
    #     req(input$leaflet_bounds)
    #     bounds <- input$leaflet_bounds
    #     latRng <- range(bounds$north, bounds$south)
    #     lngRng <- range(bounds$east, bounds$west)
    #     
    #     subset(greenInfInputs()@data, latitude >= latRng[1] & latitude <= latRng[2] & longitude >= lngRng[1] & longitude <= lngRng[2])
    # })
    # # Print Projects
    # output$text <- renderText({
    #     paste("You are viewing", nrow(onScreen()), "projects")
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)

