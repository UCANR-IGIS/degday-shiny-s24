## This function returns the daily 30yr normal minimum and maximum temperature from CalAdapt

hist_caladapt_tasminmax <- function(stid, start_date, end_date, period = "hist") {
  
  library(lubridate)
  library(dplyr)
  library(conflicted)
  conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflict_prefer("count", "dplyr", quiet = TRUE)
  conflict_prefer("select", "dplyr", quiet = TRUE)
  conflict_prefer("arrange", "dplyr", quiet = TRUE)

  if (!is.Date(start_date)) stop("start_date must be a Date")
  if (!is.Date(end_date)) stop("end_date must be a Date")
  if (end_date <= start_date) stop("end_date must come after start_date")
  
  cat(" - retrieving 30-year daily normals from gridMet that have already been downloaded from Cal-Adapt  \n")
  
  ## Load the saved 30-year daily normals
  stn_hist_tasminmax_tbl <- readRDS("stn_hist_tasminmax.rds")

  ## Verify we have a valid stid
  if (!stid %in% unique(stn_hist_tasminmax_tbl$stid)) stop("Unknown value for stid")
  
  ## Compute the Julian start and end dates
  start_yday <- yday(start_date)
  end_yday <- yday(end_date)

  stn_hist_tasminmax_tbl |> 
    filter(stid == .env$stid, dayofyear >= start_yday, dayofyear <= end_yday) |> 
    mutate(date = make_date(year=year(start_date), month = 1, day = 1) + dayofyear - 1,
           period = period) |> 
    select(stid, period, date, tasmin, tasmax)
    

}


