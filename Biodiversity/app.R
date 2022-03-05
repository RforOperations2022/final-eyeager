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
library(tidyverse)
library(plotly)

# Load iNaturalist Data
# Data Source: https://www.inaturalist.org/observations?d1=2021-03-01&d2=2022-03-01&iconic_taxa=Fungi&order_by=observed_on&place_id=124749&subview=map
iNat.load <- read.csv('observations-216426.csv')
iNat.load <- subset(iNat.load, quality_grade=='research')
iNat.load$month <- month(ymd(iNat.load$observed_on))

# Define UI for application
ui <- navbarPage("Pittsburgh Biodiversity",
                 theme = shinytheme("spacelab"),
                 tabPanel("Viz",
                          sidebarLayout(
                              sidebarPanel(
                                  # Select Taxon Name
                                  selectInput(inputId = "taxonSelect",
                                              label = "Select taxon for markers",
                                              choices = unique(iNat.load['iconic_taxon_name']),
                                              selected = c("Fungi"),
                                              selectize = T,
                                              multiple = T),
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
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Map", leafletOutput("leaflet")),
                                              tabPanel("Plots", plotOutput("species"), plotOutput("users"))
                                              )
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
    
    # Plot data reactive
    plot_data <- reactive({
        taxonInput() %>%
        group_by(iconic_taxon_name,month) %>%
        summarise(n_species = n_distinct(scientific_name),
                  n_users = n_distinct(user_login),
                  n_obs = n()) 
    })
    
    # Leaderboard data
    leaderboard_data <- reactive({
        monthInput() %>%
            group_by(user_login) %>%
            summarise(n_obs = n()) %>%
            arrange(n_obs) %>%
            top_n(10) %>%
            mutate(user_login = factor(user_login, levels = user_login))
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
    
    output$species <- renderPlot({
        ggplot(plot_data(), 
               aes(x=month,y=n_species,group=iconic_taxon_name,color=iconic_taxon_name)) + 
            geom_line() +
            geom_point() +
            ggtitle('Number of Species by Taxon') + 
            xlab('Month') +
            ylab('Number of species') + 
            theme(legend.position = "left")
    })
    
    output$users <- renderPlot({
        ggplot(leaderboard_data(), aes(x=user_login,y=n_obs)) + geom_point() + 
            coord_flip() + 
            geom_segment(aes(x=user_login, xend=user_login, y=0, yend=n_obs)) +
            ggtitle('Monthly Leaderboard') +
            ylab('Number of uploads') +
            xlab('Username')
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

