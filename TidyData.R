# Read the Training Data and then tidy it to multiple data and save it as RData files to be used when building models.

library(rjson)
library(data.table)

# install.packages("pryr")
library(pryr) #Advanced R by Hadley Wickham Memory Usage
# object_size(train)
# mem_change(rm(train))

### Control the number of trips read for training (all=-1)
### Control the number of closest trips used to calculate trip duration
# N_read <- 100000
# N_trips <- 1000
N_read <- -1
N_trips <- 10000

### Get starting & ending longitude and latitude
get_coordinate <- function(row){
  lonlat <- fromJSON(row)
  snapshots <- length(lonlat)  
  start <- lonlat[[1]]
  end <- lonlat[[snapshots]]
  return(list(start[1], start[2], end[1], end[2], snapshots))
} 

### Get Haversine distance
get_dist <- function(lon1, lat1, lon2, lat2) {  
  lon_diff <- abs(lon1-lon2)*pi/360
  lat_diff <- abs(lat1-lat2)*pi/360
  a <- sin(lat_diff)^2 + cos(lat1) * cos(lat2) * sin(lon_diff)^2  
  d <- 2*6371*atan2(sqrt(a), sqrt(1-a))
  return(d)
}

#Assumption is working directory is the Project Folder
system.time(train <- fread('Data/train.csv', stringsAsFactors=F, nrows=N_read))
system.time(test <- fread('Data/test.csv',  stringsAsFactors=F))

#save(data, file="data.RData") #Command to save subset of train.

train <- train[POLYLINE!='[]']
train <- train[DAY_TYPE =='A']

#Getting the start and end points of each trip
train[, r:=-seq(.N, 1, -1)]
train[, c('lon', 'lat', 'lon_end', 'lat_end', 'snapshots'):=get_coordinate(POLYLINE), by=r]


trainxy <-  as.data.frame(train)
trainxy$time  <- 0
trainxy$time <- as.POSIXct(as.numeric(trainxy$TIMESTAMP), origin="1970-01-01")

trainxy$date <- strptime(trainxy$time, "%Y-%m-%d")
trainxy$day <- weekdays(trainxy$date)
trainxy$time <- strptime(trainxy$time, "%Y-%m-%d %H:%M:%S")
trainxy$time <- (as.numeric(format(trainxy$time, "%H"))*60) 
+as.numeric(format(trainxy$time, "%M"))


library(MASS)
library(ggplot2)

### plotting a few cases

trainemorn <- trainxy[trainxy$time > 530 & trainxy$time < 590,]
trainemorn <- trainemorn[sample(1:nrow(trainemorn), 10000,replace=FALSE),]
m <- ggplot(trainemorn, aes(x = lat_end, y = lon_end))  
+ geom_point() 
+ xlim(41.10821, 41.25) 
+ ylim(-8.692272, -8.534187)

m + stat_density2d(aes(fill = ..level..), geom="polygon")

trainmorn <- trainxy[trainxy$time > 800 & trainxy$time < 860,]
trainmorn <- trainmorn[sample(1:nrow(trainmorn), 10000,replace=FALSE),]
m <- ggplot(trainmorn, aes(x = lat_end, y = lon_end))  
+ geom_point() 
+ xlim(41.10821, 41.25) 
+ ylim(-8.692272, -8.534187)

m + stat_density2d(aes(fill = ..level..), geom="polygon")

trainaft <- trainxy[trainxy$time > 1170 & trainxy$time < 1230,]
trainaft <- trainaft[sample(1:nrow(trainaft), 10000,replace=FALSE),]
m <- ggplot(trainaft, aes(x = lat_end, y = lon_end))  
+ geom_point() 
+ xlim(41.10821, 41.25) 
+ ylim(-8.692272, -8.534187)

m + stat_density2d(aes(fill = ..level..), geom="polygon")

traineve <- trainxy[trainxy$time > 1270 ,]
traineve <- traineve[sample(1:nrow(traineve), 10000,replace=FALSE),]
m <- ggplot(traineve, aes(x = lat_end, y = lon_end))  
+ geom_point() 
+ xlim(41.10821, 41.25) 
+ ylim(-8.692272, -8.534187)

m + stat_density2d(aes(fill = ..level..), geom="polygon")

m <- ggplot(trainxy[trainxy$time > 1270 & trainxy$time < 1330,], aes(x = lat_end, y = lon_end)) 
+ geom_point() 
+ xlim(41.10821, 41.25) 
+ ylim(-8.692272, -8.534187)

#*****************
#Adding Additional Kaggle Scripts
#*****************

#File to Clean and modify Data

#*********

library(rjson)
library(data.table)

### Control the number of trips read for training (all=-1)
### Control the number of closest trips used to calculate trip duration
# N_read <- 100000
# N_trips <- 1000
N_read <- -1
N_trips <- 10000

### Get starting longitude and latitude
get_coordinate <- function(row){
  lonlat <- fromJSON(row)
  snapshots <- length(lonlat)  
  lonlat <- lonlat[[1]]
  return(list(lonlat[1], lonlat[2], snapshots))
} 

### Get Haversine distance
get_dist <- function(lon1, lat1, lon2, lat2) {  
  lon_diff <- abs(lon1-lon2)*pi/360
  lat_diff <- abs(lat1-lat2)*pi/360
  a <- sin(lat_diff)^2 + cos(lat1) * cos(lat2) * sin(lon_diff)^2  
  d <- 2*6371*atan2(sqrt(a), sqrt(1-a))
  return(d)
}

library(readr)
library(rjson)

test  <- read_csv("../input/test.csv.zip")

#*********
# This benchmark predicts the maximum of the
# time that's elapsed so far in the trip and
# the mean time in the training set as the
# test trip duration

library(readr)
library(rjson)

test  <- read_csv("../input/test.csv.zip")
mean_train_time <- 660

positions <- function(row) as.data.frame(do.call(rbind, fromJSON(row$POLYLINE)))

submission <- test["TRIP_ID"]

for (i in 1:nrow(test)) {
  submission$TRAVEL_TIME[i] <- max(21.6*nrow(positions(test[i,])), 660)
  #submission$TRAVEL_TIME[i] <- max(22.5*nrow(positions(test[i,])), 660)
  #submission$TRAVEL_TIME[i] <- max(15*nrow(positions(test[i,])), mean_train_time)
}

write_csv(submission, "max_time_elapsed_mean_time_benchmark_21.3_660.csv")


#*********

# This script plots the current locations of taxis in the test set

library(ggmap)
library(readr)
library(rjson)

test <- read_csv("../input/test.csv.zip")
map  <- readRDS("../input/99percentile_bw_map_copyright_openstreetmap_contributors.rds")

positions <- function(row) as.data.frame(do.call(rbind, fromJSON(row$POLYLINE)))

coordinates <- data.frame(TripId=c(), Ordinal=c(), Lat=c(), Lon=c(), Status=c(), EndPoint=c())

for (i in 1:nrow(test)) {
  pos <- positions(test[i,])
  if (nrow(pos)==1) {
    status <- c("Start")
  } else {
    status <- c("Start", rep("During Trip", nrow(pos)-2), "Last Observation")
  }
  coordinates <- rbind(coordinates, data.frame(TripId=test$TRIP_ID[i],
                                               Ordinal=1:nrow(pos),
                                               Lat = pos$V2,
                                               Lon = pos$V1,
                                               Status = status,
                                               Endpoint = status != "During Trip"))
}

p <- ggmap(map) +
  geom_point(data=coordinates[coordinates$Status=="Last Observation",],
             aes(x=Lon, y=Lat, color=Status, size=Endpoint),
             color="#E41A1C",
             size=3) +
  ggtitle("Current Location of Taxis in the Test Set")
ggsave("test_trips_map.png", p, width=10, height=8, units="in")


#********

# This benchmark predicts the time that's elapsed so far in the trip as the trip duration

library(readr)
library(rjson)

test  <- read_csv("../input/test.csv.zip")

positions <- function(row) as.data.frame(do.call(rbind, fromJSON(row$POLYLINE)))

submission <- test["TRIP_ID"]

for (i in 1:nrow(test)) {
  submission$TRAVEL_TIME[i] <- 15*nrow(positions(test[i,]))
}

write_csv(submission, "time_elapsed_so_far_benchmark.csv")


#********

# This benchmark predicts the maximum of the
# time that's elapsed so far in the trip and
# the mean time in the training set as the
# test trip duration

library(readr)
library(rjson)

factor <- 0.85

test  <- read_csv("../input/test.csv.zip")
mean_train_time <- factor * 660

positions <- function(row) as.data.frame(do.call(rbind, fromJSON(row$POLYLINE)))

submission <- test["TRIP_ID"]

for (i in 1:nrow(test)) {
  submission$TRAVEL_TIME[i] <- max(15*nrow(positions(test[i,])), mean_train_time 
                                   +40)
}

write_csv(submission, "max_time_elapsed_mean_time_benchmark.csv")
#********