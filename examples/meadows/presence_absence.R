library(dplyr)
library(duckdb)
library(ggplot2)

options(digits = 10)

# DEFS
source("R/defs/plot_themes.R")
# UTILS
source("R/functions/utils/get_time_value.R")
# NDOE
source("R/functions/node/node_functions.R")
# TAG
source("R/functions/tag/tag_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY PARAMETERS HERE
## -----------------------------------------------------------------------------
# Specify the path to your database file
database_file <- "~/development/aos_test/data/meadows.duckdb"

# Specify the path to the deployment info file
deployment_info_file <- "data/meadows/meadows_deployments_2023.csv"

# Specify the time range of node data you want to import for this analysis
start_time <- as.POSIXct("2023-08-01 00:00:00", tz = "GMT")
stop_time <- as.POSIXct("2023-12-31 00:00:00", tz = "GMT")

## -----------------------------------------------------------------------------
##  1.) LOAD STATION TAG DETECTION DATA
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
detection_df <- tbl(con, "raw") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)
## -----------------------------------------------------------------------------
##  2.) LOAD DEPLOYMENT INFO
## -----------------------------------------------------------------------------
# If you have deployment info you can load it in and trim the detections to only your tags
# You might want to include other info about the individuals in this file for
# later analysis.  Ex. species, weight, sex, ect.
deployment_df <- read.csv(deployment_info_file)

## -----------------------------------------------------------------------------
##  2*.) GET LIST OF DETECTED TAGS (ALTERNATIVE)
## -----------------------------------------------------------------------------
tag_det_count <- get_tag_detection_count(detection_df, min_det_count = 5000)

ggplot(tag_det_count, aes(x = factor(tag_id, tag_det_count$tag_id), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Detection Count", y = "Tag Id") +
  tag_hist_plot_theme

## -----------------------------------------------------------------------------
##  3.) GENERATE DETECTION SUMMARY
## -----------------------------------------------------------------------------

# Discard detections that aren't from deployed tags
detection_df <- subset.data.frame(detection_df, tag_id %in% deployment_df$TagId)

# OPTIONAL: Make selections here
# deployment_df <- subset.data.frame(deployment_df,Species == "NOWA")
# detection_df <- subset.data.frame(detection_df,TagId %in% deployment_df$TagId)

det_summary_df <- detection_summary(
  detection_df = detection_df,
  tag_list = deployment_df$TagId
)

det_summary_df <- det_summary_df[order(det_summary_df$last_det, decreasing = TRUE), ]

ggplot(detection_df, aes(x = time, y = factor(tag_id, det_summary_df$tag_id))) +
  geom_bin2d(binwidth = c(3600, 1)) + # Hour time bins
  scale_fill_continuous(type = "viridis") +
  labs(x = "Time (UTC)", y = "Tag Id") +
  tag_hist_plot_theme

## -----------------------------------------------------------------------------
##  3.) SHOW DETECTION HISTORY FOR SELECTED TAG
## -----------------------------------------------------------------------------
selected_tag_id <- "2D4B782D" # SWSP - Power Tag
plot_start_time <- as.POSIXct("2023-10-05 10:00:00", tz = "GMT")
plot_stop_time <- as.POSIXct("2023-10-05 14:00:00", tz = "GMT")
tag_dets <- subset.data.frame(detection_df, tag_id == selected_tag_id)
ggplot(tag_dets) +
  geom_point(aes(x = time, y = tag_rssi, colour = node_id), shape = 1) +
  xlim(as.POSIXct(plot_start_time), as.POSIXct(plot_stop_time)) +
  ggtitle(paste("Detections", selected_tag_id)) +
  xlab("Time (UTC)") +
  ylab("RSSI (dBm)") +
  classic_plot_theme
