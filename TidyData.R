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

test$POLYLINE[1]



#save(data, file="data.RData") #Command to save subset of train.

# x = data.frame(num = 1:26, let = letters, LET = LETTERS)
# ## number of chunks
# n <- 12
# # What this does is that in 12 levels it groups the remainder in a sorted order so that
# # the split also comes in a sorted manner the rows determined by the count of remainder
# # dfchunk <- split(x, factor(sort(rank(1:nrow(x))%%n)))
# dfchunk <- split(x, factor(sort(1:nrow(x)%%n)))
# dfchunk


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

# Calculate distance in kilometers between two points
# Appears similiar to the above function
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
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

#suggestion from the discuss analytics vidhya portal
lat_lon <- function(x) {
  s <- strsplit(x, '\\](,)\\[')
  res <- s[[1]][c(1, length(s[[1]]))]
  vals <- gsub('\\[|\\]', '', res)
  vals
}

k = train$POLYLINE
d = data.frame(jsoncol=k,stringsAsFactors = FALSE)
f = lapply(d[[1]],lat_lon)
#suggestion ends here


get.route.trajectory <- function(x) {
  #converts a character matrix into a two row numeric matrix representing latitude and longitude
  
  #removes unwanted characters
  v <- gsub('\\[|\\]', '', x)
  
  #splits character into two columns
  l <- strsplit(v, '(,)')
  
  #combines into a two column numeric vector
  vec <- c(as.numeric(l[[1]][1]), as.numeric(l[[1]][2]))
  vec
}


get.route <- function (x) {
  #Gets the Character string of lattitude and longitude and returns a numeric matrix
  #with two columns of latitude and longitude
  
  #returns a single item list which contains a character vector of lattitude longitude pair
  s <- strsplit(x, '\\](,)\\[')
  
  #converts the character vector into a character matrix
  d <- matrix(data = s[[1]], ncol = 1)
  
  #converts the character matrix into two row numeric array
  route <-  vapply(d, get.route.trajectory, FUN.VALUE = double(2))
  
  #corrects the dimension of the numeric matrix
  route <- t(route)
  
  #Sets the names of the dimensions of the matrix
  colnames(route) <- c("latitude", "longitude")
  rownames(route) <- 1:nrow(route)
  route
}

x <- "[[-8.618643,41.141412],[-8.618499,41.141376],[-8.620326,41.14251]
      ,[-8.622153,41.143815],[-8.623953,41.144373],[-8.62668,41.144778]
      ,[-8.627373,41.144697],[-8.630226,41.14521],[-8.632746,41.14692]
      ,[-8.631738,41.148225],[-8.629938,41.150385],[-8.62911,41.151213]
      ,[-8.629128,41.15124],[-8.628786,41.152203],[-8.628687,41.152374]
      ,[-8.628759,41.152518],[-8.630838,41.15268],[-8.632323,41.153022]
      ,[-8.631144,41.154489],[-8.630829,41.154507],[-8.630829,41.154516]
      ,[-8.630829,41.154498],[-8.630838,41.154489]]"

get.route(x)

lonlat <- fromJSON(x)

route <- data.frame(lonlat)
route <- t(route)

nrow(route)
route[ ,"Trip.Id"] <- NA

#Sets the names of the dimensions of the matrix
colnames(route) <- c("TRIP_ID", "latitude", "longitude")
rownames(route) <- 1:nrow(route)


get.taxi.route <- function(t){
  #Given a string representing JSON returns a data frame of double
  #Better implementation than spliting and then converting to dataframe
  
  x <- t["POLYLINE"]
  trip.id <- t["TRIP_ID"]
  
  # return(trip.id)
  #convert it into list
  lonlat <- fromJSON(x)
  
  #change from list to dataframe and correct column, row
  route <- data.frame(lonlat)
  route <- t(route)
  
  #create a character vector of Trip Id
  v <- rep(trip.id, nrow(route))
  
  #add a new column
  route <- cbind(v, route)
  
  #Sets the names of the dimensions of the matrix
  colnames(route) <- c("TRIP_ID", "latitude", "longitude")
  rownames(route) <- 1:nrow(route)
  
  return(route)
} 

#combine two vectors and output a dataframe
dftwo <- data.frame(cbind(test$POLYLINE, test$TRIP_ID), stringsAsFactors = FALSE)

#add column identifiers
colnames(dftwo) <- c("POLYLINE", "TRIP_ID")

#get a list of matrix with the route trajectory
taxi.route <- apply(dftwo, 1, get.taxi.route)

#flatten the list into a matrix
route.all.taxi <- do.call(rbind, taxi.route)

get.start.end.latitude.longitude <- function(x){
  #return a vector of co-ordinates of start and end of taxi route
  start.latitude <- x[1, "latitude"]
  start.longitude <- x[1, "longitude"]
  end.latitude <- x[nrow(x), "latitude"]
  end.longitude <- x[nrow(x), "longitude"]
  start.end <- c("start.latitude" = start.latitude
                 , "start.longitude" = start.longitude
                 , "end.latitude" = end.latitude
                 , "end.longitude" = end.longitude)
  
  return(start.end)
}

# sapply returns a matrix but will have to swap column and row
start.end <- sapply(taxi.route, get.start.end.latitude.longitude)
start.end <- t(start.end)

#create new columns in the training data frame which maps to the column vectors of start.end
test$start.latitude <- start.end[, "start.latitude"]
test$start.longitude <- start.end[, "start.longitude"]
test$end.latitude <- start.end[, "end.latitude"]
test$end.longitude <- start.end[, "end.longitude"]

reverseGeoCode <- function(latlng) {
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))#Collapse and Encode URL Parameters
  library("RJSONIO") #Load Library
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK")
    address <- data.json["results.formatted_address"]
  return (address)
}
address <- reverseGeoCode(c(37.4418834, -122.1430195))
