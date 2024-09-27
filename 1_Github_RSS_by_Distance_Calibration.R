# Packages needed
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(data.table)

# Reset R's brain - removes all previous objects
rm(list=ls())



#data.setup(test, tag_col="Tag.Id", tagid="0C5F5CED", time_col="Time..UTC.",timezone="UTC",x="Longitude",y="Latitude", node_ids=node_ids, loc_precision=4)

tag_col <- "Tag.Id"
tagid = c("072A6633","2D4B782D") #"0C5F5CED"
time_col="Time..UTC."
timezone="UTC"
x="Longitude"
y="Latitude"
#loc_precision=4
options(digits=9)
## Set by User
# Working Directory - Provide/path/on/your/computer/where/master/csv/file/of/nodes/is/found/and/where/Functions_CTT.Network.R/is/located
working.directory <- "/home/jess/Documents/radio_projects/EcolEvol.Manuscript_Optimizing.Trilateration"

# Directory for Output data - Provide/path/where/you/want/output/data/to/be/stored/
outpath <- "/home/jess/Documents/radio_projects/paxton/"

## Bring in functions 
setwd(working.directory)
source("Functions_Paxton-CTTUpdate.R")

#re-sample data for calibration 
#create time window by reducing location precision
#or can input data with TestId column (user-defined window)

#INPUT
#test DATA FRAME looks like...
#tag_col, time_col, x, y, latlon=T
#must pass Start.Time, Stop.Time, TestId if you pass your own
#tagid = character or vector of tag ids
#time_col = character or vector of time columns
#timezone only applies to if your time column(s) are characters

#OUTPUT: combined/matched up data set

#node_ids = c(
#  "B25AC19E",
#  "44F8E426",
#  "FAB6E12",
#  "1EE02113",
#  "565AA5B9",
#  "EE799439",
#  "1E762CF3",
#  "A837A3F4",
#  "484ED33B"
#)

mytest <- read.csv("~/Downloads/calibration_2023_8_3_all.csv")
mytest$Time <- as.POSIXct(mytest$time_utc, tz="UTC")
start <- min(mytest$Time)
end <- max(mytest$Time)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "/home/jess/Documents/radio_projects/data/radio_projects/meadows/meadows.duckdb", read_only = TRUE)

testdata <- tbl(con, "raw") |> #tbl(con, "blu") |>
  filter(time >= as.Date("2023-07-31") & time <= as.Date("2023-10-31")) |>
  filter(tag_id %in% tagid) |>
  collect()

#testdata$syncid <- paste(format(testdata$time, "%Y-%m-%d %H:%M"), testdata$sync, sep="_")

start_buff = as.Date("2023-08-01", tz="UTC") #start - 2*60*60
end_buff = as.Date("2023-08-07", tz="UTC") #end + 2*60*60

nodehealth <- tbl(con, "node_health") |>
  filter(time >= start_buff  & time <= end_buff) |>
  collect()

DBI::dbDisconnect(con)

nodes <- node_file(nodehealth)
#nodes <- nodes[nodes$node_id %in% node_ids,]

combined.data <- data.setup(mytest, testdata, nodes, tag_col="tag_id", tagid="072A6633", time_col="Time",timezone="UTC",x="lon",y="lat", loc_precision=6, fileloc="/home/jess/Documents/radio_projects/data/radio_projects/meadows/meadows.duckdb", filetype="raw") #, loc_precision=4
## Bring in 3 Needed files - Test Information, RSS values, and Node Information - change file names in " " as needed
#test.info_k <- read.csv("Test.Info_Example.csv", header = T)

#test <- test[-which(test$Tag.Type=="userLocation"),]



#testdata$TestId <- 0L

#str(test.info) # check that data imported properly

#beep.dat <- readRDS("BeepData_Example.rds") 
#str(beep.dat) # check that data imported properl

#nodes <- read.csv("Nodes_Example.csv", header = T)
#str(nodes)



# rearrange data



## Combine distances with summary data 



#################################################################
#   Step 1
# Run function to isolate raw RSS data from a node network that
# is associated with test data  
#################################################################

    ## Variables to define for function
        ## TEST.TYPE = category indicating the type of dataset
            ## "Calibration" - test dataset used for calibrating the relationship between RSS and Distance (purpose of this R script)
            ## "LocError" - test data used to determine localization error associated with RSS-based localization estimates for a node network
        ## DATE.FORMAT = format of the date column in Test.Info.csv file using R standard date expressions (e.g., "%m-%d-%y", "%Y-%m-%d")
        ## TIMEZONE = Time zone where data was collected, use grep("<insert location name here>", OlsonNames(), value=TRUE) to find valid time zone name


    ## Output in R environment
        ## combined.data - dataframe that contains the average RSS values for a given node associated with each unique test
            ## Columns:
                ## NodeId - unique identifier of the specified node
                ## TestId - unique identifier of the specified test
                ## avgRSS - average RSS value across a 3-min time period for the given node and test
                ## sdRSS - standard deviation of RSS values across the 3-min time period for the given node and test
                ## distance - true distance (in meters) between the specified node and test location
                ## NodeUTMx - Easting location of the specified node
                ## NodeUTMy - Northing location of the specified node
                ## TestUTMx - Easting location of the specified test
                ## TestUTMy - Northing location of the specified test

    ## Output saved
        ## Calibration_Dataset.csv - .csv file of the dataframe 'combine.data' saved in the folder specified by the outpath


##* Define Variables - replace values below with user specified values **## 
#TEST.TYPE <- "Calibration"
#DATE.FORMAT <- "%m/%d/%y"
#TIME.ZONE <- "Pacific/Guam"


# Combine RSS data from a node network with test information into a dataframe
#combined.data <- data.setup(TEST.TYPE, DATE.FORMAT, TIME.ZONE)


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
    # **** you will use these coefficient values to estimate distance in Github_TestDataset_LocalizationError.R and Github_Simulations.R scripts *****
coef(nls.mod)


## Check the fit of the model and get predicted values

  # Get residuals and fit of model and add variables to main table
combined.data$E <- residuals(nls.mod)
combined.data$fit <- fitted(nls.mod)

  # Plot residuals by fit or distance
ggplot(combined.data, aes(x = distance, y = E, color = node_id)) +
         geom_point(size = 2)
ggplot(combined.data, aes(x = fit, y = E, color = node_id)) +
  geom_point(size = 2)

  # Get model predictions
combined.data$pred <- predict(nls.mod)


## Save Final Dataset with Residuals and Predictions
#write.csv(combined.data, paste0(outpath, "Calibration_Dataset_withResiduals_Predictions.csv"),
#          row.names = F)


## Plot with predicted line

#pdf(paste0(outpath, "Relationship_Distance~RSSI.pdf"), width = 8, height = 6)

ggplot(combined.data, aes(x = distance, y = avgRSS)) + 
  geom_point() +
  geom_line(aes(y = pred), color = "#689FBB", lwd = 1.25) +
  scale_y_continuous(name = "RSS (dB)") +
  scale_x_continuous(name = "Distance (m)") +
  theme_classic()

#dev.off()

combined.data <- estimate.distance(combined.data, unname(coef(nls.mod)[3]), unname(coef(nls.mod)[1]), unname(coef(nls.mod)[2]))
no.filters <- trilateration.TestData.NoFilter(combined.data)
RSS.FILTER <- c(-80, -85, -90, -95)
RSS.filters <- trilateration.TestData.RSS.Filter(combined.data, RSS.FILTER)
DIST.FILTER <- c(315,500,750,1000)
# Calculate error of location estimates of each test location when Distance filters are applied prior to trilateration 
Dist.filters <- trilateration.TestData.Distance.Filter(combined.data, DIST.FILTER)

SLIDE.TIME <- 2
GROUP.TIME <- "1 min"

test_data <- testdata %>%
  filter(time >= as.Date("2023-10-05") & time <= as.Date("2023-10-15")) %>%
  filter(tag_id == "2D4B782D") %>%
  collect()

# Function to prepare beep data for trilateration 
# by estimating distance of a signal based on RSS values
beep.grouped <- prep.data(test_data,nodes,SLIDE.TIME,GROUP.TIME,K=unname(coef(nls.mod)[3]), a=unname(coef(nls.mod)[1]), S=unname(coef(nls.mod)[2])) 

DIST.filter <- 350
RSS.filter <- -95
location.estimates <- trilateration(beep.grouped, nodes, RSS.FILTER, DIST.FILTER)


