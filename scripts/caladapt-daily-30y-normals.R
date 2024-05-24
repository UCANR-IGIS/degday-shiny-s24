## This script will download 30 years of daily minimum and maximum temperature data for 4 locations in California
## using the gridMet interpolated raster data from Cal-Adapt.

## It will then average these together to produce "30-year daily normals" for these four locations, and save them to disk.

library(caladaptr)
library(readr)
library(units)
library(dplyr)
library(tidyr)

stations_tbl <- read_csv(here::here("shiny/now-almonds/stations.csv"),
                         col_types = list(stid = col_character(),
                                          name = col_character(),
                                          x = col_double(),
                                          y = col_double()))
stations_tbl

stn_hist_tas_cap <- ca_loc_pt(coords = stations_tbl[, c("x", "y")],
                              id = stations_tbl$stid) |> 
  ca_years(start = 1990, end = 2020) |> 
  ca_slug(c("tmmn_day_gridmet", "tmmx_day_gridmet"))

## Preflight check
stn_hist_tas_cap |> ca_preflight()

# Fetch the data
stn_hist_tas_tbl <- stn_hist_tas_cap |> ca_getvals_tbl()

stn_hist_tasminmax_tbl <- stn_hist_tas_tbl |> 
  group_by(stid = id, dayofyear = yday(dt), slug) |> 
  summarize(avg_tempK = mean(val), .groups = "drop") |> 
  mutate(avg_tempF = set_units(avg_tempK, degF)) |> 
  pivot_wider(id_cols= c(stid, dayofyear), names_from = slug, values_from = avg_tempF) |> 
  mutate(dayofyear = as.integer(dayofyear),
         tasmin = as.numeric(tmmn_day_gridmet),
         tasmax = as.numeric(tmmx_day_gridmet)) |> 
  select(stid, dayofyear, tasmin, tasmax) |> 
  arrange(stid, dayofyear)
  
stn_hist_tasminmax_tbl

## Inspect
# dim(stn_hist_tasminmax_tbl)
# stn_hist_tasminmax_tbl$stid |> table()
# stn_hist_tasminmax_tbl$dayofyear |> range()
# stn_hist_tasminmax_tbl |> group_by(dayofyear) |> summarize(count_rows = n(), .groups = "drop") |> View()
# stn_hist_tasminmax_tbl |> group_by(stid) |> summarize(count_rows = n(), .groups = "drop") |> View()

## Save to disk
(stn_hist_tasminmax_fn <- here::here("shiny/now-almonds/stn_hist_tasminmax.rds"))
saveRDS(stn_hist_tasminmax_tbl, stn_hist_tasminmax_fn)

