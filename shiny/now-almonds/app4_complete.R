library(shiny)
library(readr)
library(lubridate)
library(dplyr)

## Load functions to fetch data
source("./R/rp_synoptic_tasminmax.R")

## Load API Token from a text file
my_synoptic_token <- readLines("./.secrets/my_synoptic_token.txt", n = 1)

## Import the stations
stations_tbl <- read_csv("stations.csv",
                         col_types = list(stid = col_character(),
                                          name = col_character(),
                                          x = col_double(),
                                          y = col_double(),
                                          tz = col_character()))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(title = "Navel Orangeworm Flight Predictor"),
  
  selectInput(inputId = "in_stid",
              label = "Select a weather station", 
              choices = stations_tbl$stid),
  
  dateInput(inputId = "in_biofix", 
            label = "Enter the first date when you started to see an increase in NOW eggs in your traps",
            min = make_date(year = year(Sys.Date()), month = 3, day = 1),
            max = today(),
            value = make_date(year = year(Sys.Date()), month = 4, day = 15)),
  
  actionButton("btn_calc", label = "Calculate"),
  
  p(),
  
  uiOutput(outputId = "out_report"),
  
  p(),
  
  hr(),
  
  img(src = "ucce-logo_540x85.png")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_station <- reactive({
    stations_tbl |> filter(stid == input$in_stid)
  })
  
  weather_tbl <- eventReactive(input$btn_calc,
   {
     cat("Going to fetch data! \n")
     
     recent_past_tbl <- rp_synoptic_tasminmax(
       stid = selected_station()$stid, 
       start_date = input$in_biofix, 
       end_date = Sys.Date() - 1, 
       tz = selected_station()$tz, 
       token = my_synoptic_token)
     
     recent_past_tbl
     
   })

  output$out_report <- renderUI(
    p("number of weather records = ", nrow(weather_tbl()))
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
