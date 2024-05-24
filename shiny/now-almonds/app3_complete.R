library(shiny)
library(readr)
library(lubridate)
library(dplyr)

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
  
  uiOutput(outputId = "out_report"),
  
  hr(),
  
  img(src = "ucce-logo_540x85.png")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_station <- reactive({
    stations_tbl |> filter(stid == input$in_stid)
  })
  
  output$out_report <- renderUI(
    p("Selected station = ", selected_station()$name, br(), 
      "longitude = ", selected_station()$x, br(),
      "latitude = ", selected_station()$y)
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)
