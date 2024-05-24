library(shiny)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

## Load functions to fetch data
source("./R/rp_synoptic_tasminmax.R")
source("./R/stf_openmeteo_tasminmax.R")
source("./R/hist_caladapt_tasminmax.R")

## Load API Token from a text file
my_synoptic_token <- readLines("./.secrets/my_synoptic_token.txt", n = 1)

## Import the stations
library(degday)
stations_tbl <- read_csv("stations.csv",
                         col_types = list(stid = col_character(),
                                          name = col_character(),
                                          x = col_double(),
                                          y = col_double(),
                                          tz = col_character()))

## Define parameters for the degree days calculations
lower_thresh <- 50
upper_thresh <- 94
first_flight_accdd <- 1056
second_flight_accdd <- first_flight_accdd + 750

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
  
  plotOutput(outputId = "out_ddplot", width = "700px", height = "360px"),
  
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
     
     short_term_forecast_tbl <- stf_openmeteo_tasminmax(
       stid = selected_station()$stid,
       coords = c(selected_station()$x, selected_station()$y),
       start_date = Sys.Date(),
       end_date = Sys.Date() + 7,
       tz = selected_station()$tz)
     
     hist_dlyavg_tbl <- hist_caladapt_tasminmax(
       stid = selected_station()$stid,
       start_date = Sys.Date() + 8,
       end_date = make_date(year = year(Sys.Date()), month = 9, day = 30))
     
     recent_past_tbl |> 
       bind_rows(short_term_forecast_tbl) |> 
       bind_rows(hist_dlyavg_tbl)
     
   })
  
  acc_degday_tbl <- reactive({
    req(weather_tbl())
    cat(" - computing degree days \n")
    weather_tbl() |> 
      mutate(daily_degday = dd_sng_sine(daily_min = tasmin, 
                                        daily_max = tasmax, 
                                        thresh_low = lower_thresh,
                                        thresh_up = upper_thresh,
                                        quiet = TRUE),
             acc_degday = cumsum(daily_degday))
  })

  flight_dates <- reactive({
    req(acc_degday_tbl())
    
    first_flight_date <- acc_degday_tbl() |> 
      filter(acc_degday >= first_flight_accdd) |> 
      slice_min(date, n = 1) |> 
      pull(date)
    
    second_flight_date <- acc_degday_tbl() |> 
      filter(acc_degday >= second_flight_accdd) |> 
      slice_min(date, n = 1) |> 
      pull(date)
    
    c(first_flight_date, second_flight_date)
  })
  
  output$out_ddplot <- renderPlot({
    req(acc_degday_tbl(), flight_dates())
    
    ggplot(acc_degday_tbl(), mapping = aes(x = date, y = acc_degday, col = period)) +
      geom_line() + 
      geom_vline(xintercept = flight_dates()[1], color = "navy", linewidth = 0.5) +
      geom_vline(xintercept = flight_dates()[2], color = "navy", linewidth = 0.5) +
      labs(title = "Predicted Navel Orangeworm Flights",
           subtitle = paste0("Station: ", input$in_stid)) +
      xlab("") +
      ylab("accumulated degree days (F)") +
      theme(plot.title = element_text(size = 22),
            plot.subtitle = element_text(size = 18),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
    
  })
  
  output$out_report <- renderUI(
    p("First flight is predicted for", flight_dates()[1], br(),
      "Second flight is predicted for", flight_dates()[2])
  )
  
  output$out_weather_tbl <- renderTable({
    acc_degday_tbl() |> mutate(date = as.character(date))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
