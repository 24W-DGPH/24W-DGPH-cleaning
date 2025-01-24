#
#
#    LINELIST DGPH R4EPI EXAMPLE
#
#
# Version 0.1
# 2024-11-29

renv::restore()

# Load Packages -------------------------
pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  matchmaker, # dictionary-based cleaning
  epikit, # age_categories() function
  tidyverse, # data management and visualization
  #
  styler, # source code formatting
  lintr, # detects bad code patterns, which are not errors
  #
  skimr, # preview tibbles (aka data frames)
  todor, # add TODO comments to your project
  
  # Working with Dates 
  lubridate,  # general package for handling and converting dates  
  parsedate,  # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  
  # Working with Strings
  stringr,
  tools,
  
  # Work with Factors
  forcats,       # factors]
  
  # Build complex Tables
  kableExtra,
  
  # Table Viz
  flextable,      # make HTML tables 
  officer,
  
  # GG Plot Extras
  ggforce,
  
  shiny,
  
  # mapping
  sf,               # For working with spatial data
  rnaturalearth,    # For country data
  rnaturalearthdata,
  
  leaflet
)

renv::snapshot()

source("clean.R")
source("visualize.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("My app"),
  sidebarLayout(
    sidebarPanel(
      plotOutput("hist_age"),
      sliderInput("select_age", label = "Age", min = 0, max = 100, value = c(0,100)),
      plotOutput("hist_bmi"),
      sliderInput("select_bmi", label = "BMI", min = 0, max = 100, value = c(0,100)),
    ),
    mainPanel(
      plotOutput("map"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive function to filter data based on inputs
  filtered_data <- reactive({
    filter_data(linelist, input$select_age, input$select_bmi)
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet_map(filtered_data(), input$select_age, input$select_bmi)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
