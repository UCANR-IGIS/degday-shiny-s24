library(shiny)
library(readr)

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
              label = "Select a weather station"),
  
  tags$hr(),
  
  img(src = "ucce-logo_540x85.png")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
