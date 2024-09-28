library(dplyr)
library(ggplot2)

options(digits = 10)

# DEFS
source("src/defs/plot_themes.R")
# TAG
source("src/functions/tag/load_node_detection_data.R")
# NDOE
source("src/functions/node/node_functions.R")
# GRID SEARCH
source("src/functions/grid_search/grid_search_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY INPUTS HERE
## -----------------------------------------------------------------------------
# Specify the path to the directory with your node health data
node_health_directory <- "data/meadows_stopover/station/nodes/"
# Specify the path to the directory with your station detection data
tag_data_directory <- "data/meadows_stopover/station/raw_data/"
# Specify the path to the deployment info file
deployment_info_file <- "data/meadows_stopover/meadows_deployments_2023.csv"

# Specify the RSSI vs Distance fit coefficients from calibration
#a <- -103.0610446987
a <- -115.0 # Lowered to work for the 1/8 wave tags?
b <- -60.6023833206 
c <- 0.0120558164
rssi_coefs <- c(a,b,c)

# Specify the time range of node data you want to import for this analysis
#   This range should cover a large time window where your nodes were in 
#   a constant location.  All node health records in this time window
#   will be used to accurately determine the position of your nodes
node_start_time <- "2023-08-01 00:00:00"
node_stop_time <- "2023-08-07 00:00:00"

# Selected Tag Id
selected_tag_id <- "2D4B782D" # SWSP - Power Tag
# Analysis Time Range
analysis_start_time <- as.POSIXct("2023-10-03 12:00:00", tz = "GMT")
analysis_stop_time <- as.POSIXct("2023-10-06 12:00:00", tz = "GMT")

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"
## -----------------------------------------------------------------------------
##  1.) LOAD NODE HEALTH DATA FROM FILES
## -----------------------------------------------------------------------------
node_health_df <- load_node_health_files(node_health_directory)
node_health_df <- node_health_df %>% mutate(time_value = get_time_value(node_health_df$Time))
node_health_df <- subset.data.frame(node_health_df, time_value >= get_time_value(node_start_time))
node_health_df <- subset.data.frame(node_health_df, time_value <= get_time_value(node_stop_time))
# If you want to select only specific nodes to include in calibration
# SKIP this line if you want to use all nodes!
# node_health_df <- subset.data.frame(node_health_df, NodeId %in% my_nodes)

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
##  3.) LOAD TAG NODE DETECTION DATA FROM STATION FILES
## -----------------------------------------------------------------------------
detection_df <- load_node_detection_data(tag_data_directory)
# Remove detections from other tags
detection_df <- subset.data.frame(detection_df, TagId == selected_tag_id)
# Remove detections out of analysis time range
detection_df <- subset.data.frame(detection_df, detection_df$Time >= analysis_start_time)
detection_df <- subset.data.frame(detection_df, detection_df$Time <= analysis_stop_time)

detection_df <- detection_df %>% mutate(time_value = get_time_value(detection_df$Time))


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
  length_seconds = 6*3600, 
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
track_map <- map_track(node_locs,locations_df,my_tile_url)
track_map
