## This function returns the short-term forecast using OpenMeteo
stf_openmeteo_tasminmax <- function(stid, coords, start_date, end_date, tz, period = "stf") {
  
  if (!tz %in% OlsonNames()) stop("Unknown value for tz")
  if (length(coords) != 2) stop("coords must have length two")
  if (!is.Date(start_date)) stop("start_date must be a Date")
  if (!is.Date(end_date)) stop("end_date must be a Date")
  if (end_date <= start_date) stop("end_date must come after start_date")
  
  library(openmeteo)  
  library(dplyr)
  library(conflicted)
  conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflict_prefer("count", "dplyr", quiet = TRUE)
  conflict_prefer("select", "dplyr", quiet = TRUE)
  conflict_prefer("arrange", "dplyr", quiet = TRUE)
  
  cat(" - fetching data from OpenMeteo for the short-term forecast \n")
  
  ## Define a filename for the cached API response
  stn_omstf_tas_fn <- file.path(tempdir(), paste("openmeteo-stf_", paste(coords, collapse = "-") , "_", start_date, "_", end_date, ".rds", sep=""))
  
  if (file.exists(stn_omstf_tas_fn)) {
    cat("   - using a cached API response \n")
    om_forecast_dly_tbl <- readRDS(stn_omstf_tas_fn)
  } else {
  
    om_forecast_dly_tbl <- weather_forecast(
      location = coords[c(2,1)],
      start = start_date,
      end = end_date,
      daily = c("temperature_2m_min", "temperature_2m_max"),
      response_units = list(temperature_unit = "fahrenheit"),
      timezone = tz)
    
    saveRDS(om_forecast_dly_tbl, file = stn_omstf_tas_fn)
  }
  
  stn_shrtfrcst_dlytas_tbl <- om_forecast_dly_tbl |> 
    mutate(stid = stid, period = period) |> 
    select(stid, period, date, tasmin = daily_temperature_2m_min, tasmax = daily_temperature_2m_max)
  
}


