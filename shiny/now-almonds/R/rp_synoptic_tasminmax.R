## This function called the Synoptic API, and return daily minimum and maximum temperature values

rp_synoptic_tasminmax <- function(stid, start_date, end_date, tz, token, period = "rp") {
  
  library(httr2)
  library(lubridate)
  library(dplyr)
  library(purrr)
  
  library(conflicted)
  conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflict_prefer("count", "dplyr", quiet = TRUE)
  conflict_prefer("select", "dplyr", quiet = TRUE)
  conflict_prefer("arrange", "dplyr", quiet = TRUE)
  
  if (!tz %in% OlsonNames()) stop("Unknown value for tz")
  if (!is.Date(start_date)) stop("start_date must be a Date")
  if (!is.Date(end_date)) stop("end_date must be a Date")
  if (end_date <= start_date) stop("end_date must come after start_date")
 
  cat(" - fetching data from Synoptic for the recent past \n")
  
  start_dt_chr <- start_date |>
    as_datetime(tz = tz) |> 
    with_tz("UTC") |> 
    format("%Y%m%d%H%M")
  
  end_dt_chr <- end_date |>
    as_datetime(tz = tz) |> 
    (`+`)(hours(23)) |> 
    (`+`)(minutes(59)) |> 
    with_tz("UTC") |> 
    format("%Y%m%d%H%M")
  
  # cat(start_dt_chr, "\n")
  # cat(end_dt_chr, "\n")
  
  ## Define a filename for the cached API response
  stn_tas_resp_fn <- file.path(tempdir(), paste("synoptic-resp-", stid, "-", start_dt_chr, "-", end_dt_chr, ".rds", sep=""))
  
  if (file.exists(stn_tas_resp_fn)) {
    # Load a cached copy
    cat("   - using a cached API response \n")
    stn_tas_resp <- readRDS(stn_tas_resp_fn)
    
  } else {
    stn_tas_req <- request("https://api.synopticdata.com/v2/stations/timeseries") |> 
      req_headers("Accept" = "application/json") |> 
      req_url_query(token = token,
                    start = start_dt_chr,
                    end = end_dt_chr,
                    stid = stid,
                    vars = "air_temp",
                    units = "english",
                    obtimezone = "local",
                    .multi = "comma")  
    
    stn_tas_resp <- req_perform(stn_tas_req)
    
    if (resp_status(stn_tas_resp) != 200) stop("The Synoptic API returned an error")
    saveRDS(stn_tas_resp, file = stn_tas_resp_fn)
  }
  
  stn_tas_lst <- stn_tas_resp |> resp_body_json()
  
  ## Pull out the date-times
  obs_dt <- stn_tas_lst$STATION[[1]]$OBSERVATIONS$date_time |> 
    modify_if(is_null, ~NA) |> 
    unlist() |> 
    ymd_hms(tz = tz, quiet = TRUE)
  
  ## Pull out the air temp valus
  obs_tas <- stn_tas_lst$STATION[[1]]$OBSERVATIONS$air_temp_set_1 |> 
    modify_if(is_null, ~NA) |> 
    unlist()
  
  ## Create the tibble of hourly data
  stn_hlytas_tbl <- tibble(stid = stid,
                           dt = obs_dt,
                           tas = obs_tas)
  
  ## Return a tibble of the daily min and max
  stn_hlytas_tbl |> 
    mutate(date = date(dt), period = period) |> 
    group_by(stid, period, date) |> 
    summarise(tasmin = min(tas), tasmax = max(tas), .groups = "drop")

}

