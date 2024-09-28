library(dplyr)
library(ggplot2)

options(digits = 10)

# DEFS
source("src/defs/plot_themes.R")
# UTILS
source("src/functions/utils/get_time_value.R")
# NDOE
source("src/functions/node/node_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY PARAMETERS HERE
## -----------------------------------------------------------------------------
# Specify the path to the directory with your station detection data
database_directory <- "~/development/aos_test/data/meadows.duckdb"

# Specify the time range of node data you want to import for this analysis
start_time <- as.POSIXct("2023-10-01 00:00:00",tz="GMT")
stop_time <- as.POSIXct("2023-11-01 00:00:00",tz="GMT")

## -----------------------------------------------------------------------------
##  1.) LOAD NODE HEALTH DATA FROM FILES
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_directory, read_only = TRUE)
node_health_df <- tbl(con, "node_health") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)
node_health_df <- node_health_df %>% distinct(node_id, time, recorded_at, .keep_all = TRUE)

## -----------------------------------------------------------------------------
##  2.) CHECK THAT NODES ARE OPERATING
## -----------------------------------------------------------------------------
# Look at the number of health records received from each node
node_record_counts <- node_health_df %>% count(node_id)
node_record_counts <- node_record_counts[order(node_record_counts$n, decreasing = TRUE),]
ggplot(node_record_counts,aes(x=factor(node_id,node_record_counts$node_id),y=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x="Health Record Count",y="Node Id") +
  tag_hist_plot_theme

## -----------------------------------------------------------------------------
##  2.) CHECK BATTERY AND CHARGING
## -----------------------------------------------------------------------------
# Plot the Battery voltage vs. time for all nodes
ggplot(node_health_df) +
  geom_point(aes(x=time,y=battery,colour = node_id)) +
  classic_plot_theme

# Plot the Battery & Solar voltage vs. time for a specific node
selected_node_id <- "326710"
batt_solar_plot <- plot_battery_solar(node_health_df = node_health_df, selected_node_id = selected_node_id)
batt_solar_plot 

## -----------------------------------------------------------------------------
##  3.) CHECK GPS
## -----------------------------------------------------------------------------




## -----------------------------------------------------------------------------
##  4.) CHECK TIME OFFSET
## -----------------------------------------------------------------------------




