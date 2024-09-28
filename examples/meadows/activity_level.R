library(dplyr)
library(ggplot2)

options(digits = 10)

# DEFS
source("R/defs/plot_themes.R")
# TAG
source("R/functions/tag/tag_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY INPUTS HERE
## -----------------------------------------------------------------------------
# Specify the path to your database file
database_file <- "~/development/aos_test/data/meadows.duckdb"

start_time <- as.POSIXct("2023-10-01 00:00:00",tz = "GMT")
stop_time <- as.POSIXct("2023-10-21 00:00:00",tz = "GMT")

## -----------------------------------------------------------------------------
##  1.) LOAD TAG NODE DETECTION DATA 
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
detection_df <- tbl(con, "raw") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)
## -----------------------------------------------------------------------------
##  3.) TAG ACTIVITY
## -----------------------------------------------------------------------------
selected_tag_id <- "2D4B782D" # SWSP - Power Tag
tag_dets <- subset.data.frame(detection_df, tag_id == selected_tag_id)
tag_dets <- tag_dets[order(tag_dets$time, decreasing = FALSE), ]

tag_beep_interval <- 13 # seconds

tag_activity <- calculate_tag_activity(tag_dets, tag_beep_interval)
avg_tag_act <- calc_avg_activity(tag_activity, start_time, stop_time)

plot_start_time <- as.POSIXct("2023-10-03 06:00:00", tz = "GMT")
plot_stop_time <- as.POSIXct("2023-10-07 06:00:00", tz = "GMT")

# Scatter Plot of RSSI vs time by Node
ggplot(tag_dets) +
  geom_point(aes(x = time, y = tag_rssi, colour = node_id), shape=1) +
  xlim(plot_start_time, plot_stop_time) +
  ggtitle(paste("Detections",selected_tag_id)) +
  xlab("Time (UTC)") + 
  ylab("RSSI (dBm)") +
  classic_plot_theme

# Scatter Plot of activity vs time by Node
ggplot(tag_activity) +
  geom_point(aes(x = time, y = abs_act, colour = node_id), shape=1) +
  ggtitle(paste("Detections",selected_tag_id)) +
  xlab("Time (UTC)") +
  ylab("Activity (Arb. Units)") +
  xlim(plot_start_time, plot_stop_time) +
  classic_plot_theme

my_breaks <- c(1, 10, 100, 1000, 10000)
# 2D histogram of activity vs time
ggplot(tag_activity, aes(x = time, y = abs_act)) +
  geom_bin2d(binwidth = c(3600, 1)) +
  xlim(plot_start_time, plot_stop_time) +
  scale_fill_viridis_c(name = "Counts", trans = "log", breaks = my_breaks, labels = my_breaks) +
  xlab("Time (UTC)") +
  ylab("Activity / Hour (Arb. Units)") +  
  classic_plot_theme
  
# 2D histogram of activity vs time WITH avg activity
ggplot(tag_activity, aes(x = time, y = abs_act)) +
 geom_bin2d(binwidth = c(3600, 1)) +
  geom_line(data = avg_tag_act, aes(x = time, y = avg_activity),colour = "Red") +
  geom_point(data = avg_tag_act, aes(x = time, y = avg_activity), colour = "Red") +
  xlim(plot_start_time, plot_stop_time) +
  xlab("Time (UTC)") +
  ylab("Activity / Hour (Arb. Units)") +
  scale_fill_viridis_c(name = "Counts", trans = "log", breaks = my_breaks, labels = my_breaks) +
  classic_plot_theme

# Avg activity / hour Vs time
ggplot(tag_activity) +
  geom_line(data = avg_tag_act, aes(x = time, y = avg_activity),colour = "Red") +
  geom_point(data = avg_tag_act, aes(x = time, y = avg_activity), colour = "Red") +
  xlim(plot_start_time, plot_stop_time) +
  xlab("Time (UTC)") +
  ylab("Activity / Hour (Arb. Units)") +
  scale_fill_viridis_c(name = "Counts", trans = "log", breaks = my_breaks, labels = my_breaks) +
  classic_plot_theme
