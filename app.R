#
#
#    LINELIST DGPH R4EPI EXAMPLE
#
#
# Version 0.1
# 2024-11-29


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
  
  shiny
)

source("clean.R")
source("visualize.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("My app"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("select_age1", label = "Age 1", min = 0, max = 100, value = c(18,65)),
      sliderInput("select_bmi1", label = "BMI 1", min = 0, max = 100, value = c(15,25)),
      sliderInput("select_age2", label = "Age 2", min = 0, max = 100, value = c(18,65)),
      sliderInput("select_bmi2", label = "BMI 2", min = 0, max = 100, value = c(15,25))
    ),
    mainPanel(
      plotOutput("scatter1"),
      plotOutput("scatter2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$scatter1 <- renderPlot({
      scatter(linelist, input$select_age1, input$select_bmi1)
    })
    output$scatter2 <- renderPlot({
      scatter(linelist, input$select_age2, input$select_bmi2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
