library(dplyr)

options(digits = 10)

# DEFS
source("R/defs/plot_themes.R")
# UTILS
source("R/functions/utils/get_time_value.R")
# NDOE
source("R/functions/node/node_functions.R")
# TAG
source("R/functions/tag/tag_functions.R")
# SIDEKICK
source("R/functions/sidekick/load_sidekick_data.R")
# CALIBRATION
source("R/functions/calibration/calibration_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY PARAMETERS HERE
## -----------------------------------------------------------------------------
# Specify the path to the sidekick data file you recorded for calibration
sidekick_file_path <- "data/meadows/sidekick/calibration_2023_8_3_all.csv"
# Specify the path to your database file
database_file <- "~/development/aos_test/data/meadows.duckdb"

# Specify the tag ID that you used in your calibration
my_tag_id <- "072A6633"

# Specify the time range of node data you want to import for this analysis
#   This range should cover a large time window where you nodes were in
#   a constant location.  All node health records in this time window
#   will be used to accurately determine the position of your nodes
start_time <- as.POSIXct("2023-08-01 00:00:00", tz = "GMT")
stop_time <- as.POSIXct("2023-08-07 00:00:00", tz = "GMT")

# Specify a list of node Ids if you only want to include a subset in calibration
# IF you want to use all nodes, ignore this line and SKIP the step below
# where the data frame is trimmed to only nodes in this list
# my_nodes <- c("B25AC19E", "44F8E426", "FAB6E12", "1EE02113", "565AA5B9", "EE799439", "1E762CF3", "A837A3F4", "484ED33B")

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##  1.) LOAD NODE HEALTH DATA FROM FILES
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
node_health_df <- tbl(con, "node_health") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)
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
export_node_locations("examples/meadows/output/node_locations.csv", node_locs)
# Draw a map with the node locations
node_map <- map_node_locations(node_locs, tile_url = my_tile_url)
node_map

## -----------------------------------------------------------------------------
##  3.) LOAD STATION DETECTION DATA FROM FILES
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
detection_df <- tbl(con, "raw") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)

# Load from File
# detection_df <- load_node_detection_data(tag_data_directory, start_time = start_time, stop_time = stop_time)

# Get beeps from test tag only
detection_df <- subset.data.frame(detection_df, tag_id == my_tag_id)

## -----------------------------------------------------------------------------
##  4.) LOAD SIDEKICK CALIBRATION DATA FROM FILE
## -----------------------------------------------------------------------------
# Get Sidekick data from CSV (note I modified the headers for cleaner names in here)
sidekick_all_df <- load_sidekick_data(sidekick_file_path)
# Get beeps from test tag only
sidekick_tag_df <- subset.data.frame(sidekick_all_df, tag_id == my_tag_id)
# Show location of all beeps in relation to node locations
calibration_map <- map_calibration_track(node_locs, sidekick_tag_df, tile_url = my_tile_url)
calibration_map

## -----------------------------------------------------------------------------
##  5.) CALCULATE THE RSSI VS DISTANCE RELATIONSHIP
## -----------------------------------------------------------------------------
# This function will match sidekick detections to detections recorded by nodes
# and sent to the station.  Then using the sidekick location, the node locations
# calculated above, and the rssi measured in the node, a list of rssi and
# distance pairs is generated and returned
# For Blu Series tags use_sync=TRUE, for 434 MHz tags use_sync=FALSE
rssi_v_dist <- calc_rssi_v_dist(node_locs, sidekick_tag_df, detection_df, use_sync = FALSE)

# Plot the resulting RSSI and distance data
ggplot() +
  geom_point(data = rssi_v_dist, aes(x = distance, y = rssi, colour = node_id)) +
  labs(title="RSSI vs. Distance",x="Distance (m)",y="RSSI (dBm)",colour="Node ID") +
  classic_plot_theme

# Fit the RSSI vs distance data with exponential relationship
nlsfit <- nls(
  rssi ~ a - b * exp(-c * distance),
  rssi_v_dist,
  start = list(a = -105, b = -60, c = 0.17)
)
summary(nlsfit)
# Get the coefficients from the fit result
co <- coef(summary(nlsfit))
rssi_coefs <- c(co[1, 1], co[2, 1], co[3, 1])

# Add a predicted column to the RSSI vs distance data
rssi_v_dist$pred <- predict(nlsfit)
# Plot the RSSI vs distance data with the fit curve
calibration_plot <- plot_calibration_result(rssi_v_dist, classic_plot_theme)
calibration_plot

# Print the coefficients from the fit. You'll need these coefficients later
# for localization.
print(rssi_coefs)

## -----------------------------------------------------------------------------
##  GRID CALIBRATED!!!
## -----------------------------------------------------------------------------
