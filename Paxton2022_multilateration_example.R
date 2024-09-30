# Packages needed
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(data.table)

# Reset R's brain - removes all previous objects
rm(list=ls())
tagid = c("072A6633","2D4B782D") #"0C5F5CED"
timezone="UTC"
options(digits=9)

## Bring in functions 
source("R/Functions_Paxton-CTTUpdate.R")

#re-sample data for calibration 
#create time window by reducing location precision or can input data with TestId column (user-defined window)
mytest <- read.csv("calibration_2023_8_3_all.csv")
mytest$Time <- as.POSIXct(mytest$time_utc, tz="UTC")

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "/home/jess/Documents/workshop/full_data/meadows.duckdb", read_only = TRUE)

testdata <- tbl(con, "raw") |> 
  filter(time >= as.Date("2023-07-31") & time <= as.Date("2023-10-31")) |>
  filter(tag_id %in% tagid) |>
  collect()

start_buff = as.Date("2023-08-01", tz="UTC")
end_buff = as.Date("2023-08-07", tz="UTC")

nodehealth <- tbl(con, "node_health") |>
  filter(time >= start_buff  & time <= end_buff) |>
  collect()

DBI::dbDisconnect(con)

nodes <- node_file(nodehealth)

#################################################################
#   Step 1
# Run function to isolate raw RSS data from a node network that
# is associated with test data  
#################################################################
combined.data <- data.setup(mytest, testdata, nodes, tag_col="tag_id", tagid="072A6633", time_col="Time",timezone="UTC",x="lon",y="lat", loc_precision=6, fileloc="/home/jess/Documents/radio_projects/data/radio_projects/meadows/meadows.duckdb", filetype="raw")


##########################################################
#    Step 2
# Exponential Decay Function to Examine Relationship 
# between distance and Tag RSS values 
##########################################################
## Visualize data
  # Plot of the relationship between RSS and distance
ggplot(data = combined.data, aes(x = distance, y = avgRSS, color = node_id)) +
  geom_point(size = 2)

## Preliminary Exponential Decay Model to determine starting values for the final model 

    # SSasymp - self start for exponential model to finding the starting values of the data
    # Asym - horizontal asymptote (when large values ) - y values decay to this value 
    # R0 - numeric value when avgRSS (i.e., response variable) = 0 
    # lrc - natural logarithm of the rate constant (rate of decay)

  # Preliminary Model
#combined.data <- combined.data[!is.na(combined.data$distance),]
exp.mod <- nls(avgRSS ~ SSasymp(distance, Asym, R0, lrc), data = combined.data)
  # Summary of Model
summary(exp.mod)
  # rate of decay
exp(coef(exp.mod)[["lrc"]])

## Final Exponential Decay Model with user provided self-starting values 
## based on visualization of the data and values in the Preliminary Model Output

    # exponential model formula: avgRSS ~ a * exp(-S * distance) + K
      # a = intercept
      # S = decay factor
      # K = horizontal asymptote

##  ***** Variables to define for final model below - replace values below with values from exp.mod ****  ## 
a <- coef(exp.mod)[["R0"]]
S <- exp(coef(exp.mod)[["lrc"]])
K <- coef(exp.mod)[["Asym"]]
  
  # Final Model
nls.mod <- nls(avgRSS ~ a * exp(-S * distance) + K, start = list(a = a, S = S, K= K), 
               data = combined.data)
  # Model Summary
summary(nls.mod)
  # Model Coefficients 
coef(nls.mod)

## Check the fit of the model and get predicted values
  # Get residuals and fit of model and add variables to main table
combined.data$E <- residuals(nls.mod)
combined.data$fit <- fitted(nls.mod)

  # Plot residuals by fit or distance
#ggplot(combined.data, aes(x = distance, y = E, color = node_id)) +
#        geom_point(size = 2)

#ggplot(combined.data, aes(x = fit, y = E, color = node_id)) +
#  geom_point(size = 2)

  # Get model predictions
combined.data$pred <- predict(nls.mod)

## Plot with predicted line
ggplot(combined.data, aes(x = distance, y = avgRSS, color=node_id)) + 
  geom_point() +
  geom_line(aes(y = pred), color="black", lwd = 1.25) +
  scale_y_continuous(name = "RSS (dB)") +
  scale_x_continuous(name = "Distance (m)") +
  theme_classic()

K <- unname(coef(nls.mod)[3])
a <- unname(coef(nls.mod)[1])
S <- unname(coef(nls.mod)[2])

combined.data <- estimate.distance(combined.data, K, a, S)

tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
testout <- combined.data[combined.data$TestId==0,]
leaflet() %>%
    addTiles(
      urlTemplate = tile_url,
      options = tileOptions(maxZoom = 25)
    ) %>%
    addCircleMarkers(
      data = nodes,
      lat = nodes$node_lat,
      lng = nodes$node_lng,
      radius = 5,
      color = "cyan",
      fillColor = "cyan",
      fillOpacity = 0.5,
      label = nodes$node_id
    )  %>%
    addCircles(
      data=testout, 
      lat = testout$node_lat,
      lng = testout$node_lng,
      radius = testout$distance,
      color = "red",
      #fillColor = "red",
      fillOpacity = 0)

no.filters <- trilateration.TestData.NoFilter(combined.data)
RSS.FILTER <- c(-80, -85, -90, -95)
RSS.filters <- trilateration.TestData.RSS.Filter(combined.data, RSS.FILTER)
#DIST.FILTER <- c(315,500,750,1000)
# Calculate error of location estimates of each test location when Distance filters are applied prior to trilateration 
#Dist.filters <- trilateration.TestData.Distance.Filter(combined.data, DIST.FILTER)

SLIDE.TIME <- 2
GROUP.TIME <- "1 min"

test_data <- testdata %>%
  filter(time >= as.Date("2023-10-05") & time <= as.Date("2023-10-15")) %>%
  filter(tag_id == "2D4B782D") %>%
  collect()

# Function to prepare beep data for trilateration 
# by estimating distance of a signal based on RSS values
beep.grouped <- prep.data(test_data,nodes,SLIDE.TIME,GROUP.TIME,K, a, S) 

RSS.filter <- -95
location.estimates <- trilateration(beep.grouped, nodes, RSS.FILTER)

mapping(nodes, location.estimates)
