library(dplyr)
library(duckdb)
library(ggplot2)

options(digits = 10)

# DEFS
source("R/defs/plot_themes.R")
# TAG
source("R/functions/tag/load_node_detection_data.R")
# NDOE
source("R/functions/node/node_functions.R")
# GRID SEARCH
source("R/functions/grid_search/grid_search_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY INPUTS HERE
## -----------------------------------------------------------------------------
# Specify the path to your database file
database_file <- "~/development/aos_test/data/meadows.duckdb"
# Specify the path to the deployment info file
deployment_info_file <- "data/meadows/meadows_deployments_2023.csv"

# Specify the RSSI vs Distance fit coefficients from calibration
# a <- -103.0610446987
a <- -115.0 # Lowered to work for the 1/8 wave tags?
b <- -60.6023833206
c <- 0.0120558164
rssi_coefs <- c(a, b, c)

# Specify the time range of node data you want to import for this analysis
#   This range should cover a large time window where your nodes were in
#   a constant location.  All node health records in this time window
#   will be used to accurately determine the position of your nodes
node_start_time <- as.POSIXct("2023-08-01 00:00:00", tz = "GMT")
node_stop_time <- as.POSIXct("2023-08-07 00:00:00", tz = "GMT")

# Selected Tag Id
selected_tag_id <- "2D4B782D" # SWSP - Power Tag
# Analysis Time Range
det_start_time <- as.POSIXct("2023-10-03 12:00:00", tz = "GMT")
det_stop_time <- as.POSIXct("2023-10-06 12:00:00", tz = "GMT")

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"

## -----------------------------------------------------------------------------
##  1.) LOAD NODE HEALTH DATA FROM FILES
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
node_health_df <- tbl(con, "node_health") |>
  filter(time >= node_start_time & time <= node_stop_time) |>
  collect()
DBI::dbDisconnect(con)
# Remove duplicates
node_health_df <- node_health_df %>% distinct(node_id, time, recorded_at, .keep_all = TRUE)

## -----------------------------------------------------------------------------
##  2.) GET NODE LOCATIONS
## -----------------------------------------------------------------------------
# Calculate the average node locations
node_locs <- calculate_node_locations(node_health_df)
# Plot the average node locations
node_loc_plot <- plot_node_locations(node_health_df, theme = classic_plot_theme)
node_loc_plot
# Write the node locations to a file
# export_node_locations("output/node_locations.csv",node_locs)
# Draw a map with the node locations
node_map <- map_node_locations(node_locs, tile_url = my_tile_url)
node_map

## -----------------------------------------------------------------------------
##  3.) LOAD STATION DETECTION DATA
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
detection_df <- tbl(con, "raw") |>
  filter(time >= det_start_time & time <= det_stop_time) |>
  filter(tag_id == selected_tag_id) |>
  collect()
DBI::dbDisconnect(con)
detection_df <- detection_df %>% mutate(time_value = as.integer(time))

## -----------------------------------------------------------------------------
##  4.) BUILD A GRID
## -----------------------------------------------------------------------------
grid_center_lat <- 38.93664800
grid_center_lon <- -74.9462
grid_size_x <- 500 # meters
grid_size_y <- 800 # meters
grid_bin_size <- 5 # meters
# Create a data frame with the details about the grid
grid_df <- build_grid(
  node_locs = node_locs,
  center_lat = grid_center_lat,
  center_lon = grid_center_lon,
  x_size_meters = grid_size_x,
  y_size_meters = grid_size_y,
  bin_size = grid_bin_size
)
# Draw all of the grid bins on a map
grid_map <- draw_grid(node_locs, grid_df)
grid_map

## -----------------------------------------------------------------------------
##  6.) CALCULATE LOCATIONS
## -----------------------------------------------------------------------------
locations_df <- calculate_track(
  start_time = "2023-10-04 23:00:00",
  length_seconds = 6 * 3600,
  step_size_seconds = 15,
  det_time_window = 30, # Must have detection within this window to be included in position calculation
  filter_alpha = 0.7,
  filter_time_range = 60, # Time range to include detections in filtered value
  grid_df = grid_df,
  detection_df = detection_df,
  node_locs = node_locs,
  rssi_coefs = rssi_coefs,
  track_frame_output_path = NULL # If NULL no individual frames will be saved
)
print(track_df)
track_map <- map_track(node_locs, locations_df, my_tile_url)
track_map

source("R/functions/grid_search/grid_search_functions.R")
loc_density <- calc_location_density(grid_df = grid_df, locations_df = locations_df)
loc_desnity_map <- map_location_density(loc_density_df = loc_density,my_tile_url)
loc_desnity_map
