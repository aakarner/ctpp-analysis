library(tidycensus)
library(tigris)
library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(progress)
library(trread)
library(gtsf)
library(sf)
library(ggplot2)

# Load the OTP API functions
source("src/otp-api-fn.R")

# Connect to the CTPP database and extract tracts for Harris County
harris_county <- c('201')

res_mode_white <- dbGetQuery(
  con, "
  SELECT st || cty || tr AS geoid10, lineno, est  
  FROM b102201
  INNER JOIN lookupres ON
  b102201.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  cty IN ('201')") %>%
  spread(lineno, est, fill = 0)


harris_tracts <- tracts(state = "TX", county = "Harris County", year = 2010,
                        cb = TRUE) %>%
  mutate(geoid10 = substr(GEO_ID, 10, 20))


# Does every tract in Harris have a matching GEOID from the CTPP?
stopifnot(all(harris_tracts$geoid10 %in% res_mode_white$geoid10))

# Calculate origin and destination centroids
# TODO: Create a population-weighted mean by downloading the decennial blocks
# from tidycensus and manually creating the centroid 

# Call otpConnect() to define a connection called otpcon
before <- otpConnect(
  hostname = "localhost",
  router = "before",
  port = "8080",
  ssl = "false")

after <- otpConnect(
  hostname = "localhost",
  router = "after",
  port = "8085",
  ssl = "false")

# Only consider tract centroids that are located within a half mile of transit
metroPost <- 
  import_gtfs("D:/Dropbox/Work/FTA connectivity/data/GTFS/20150818_htx.zip",
                         local = TRUE)
metroPost <- gtfs_as_sf(metroPost)
metroPostsf <- st_transform(metroPost$sf_routes, "+init=epsg:3673") # http://spatialreference.org/ref/epsg/3673/

metroPre <- import_gtfs("D:/Dropbox/Work/FTA connectivity/data/GTFS/20150517_htx.zip",
                         local = TRUE)
metroPre <- gtfs_as_sf(metroPre) 
metroPresf <- st_transform(metroPost$sf_routes, "+init=epsg:3673") # http://spatialreference.org/ref/epsg/3673/

metroAll <- rbind(metroPostsf, metroPresf)

metroBuff <- metroAll %>%
  st_buffer(805)


summarize(metroBuff)

ggplot() + geom_sf(data = harris_tracts) + geom_sf(data = metroBuff)

# Select tract 

centroids <- harris_tracts %>%
  st_transform("+init=epsg:3673") %>%
  st_centroid()

centroids <- centroids[st_within(centroids, metroBuff) %>% lengths > 0, ]
centroids <- cbind(geoid10 = centroids$geoid10, centroids)
centroids <- st_transform(centroids, "+init=epsg:4326")
centroids$latlong <- paste0(centroids$Y, ",", centroids$X)

centroids_final <- as.data.frame(st_coordinates(centroids))
centroids_final <- cbind(centroids_final, geoid10 = centroids$geoid10)

centroids_final$latlong <- paste0(centroids_final$Y, ",", centroids_final$X)

odmtx <- expand.grid(
  list(origins = centroids_final$latlong, 
       destinations = centroids_final$latlong),
       stringsAsFactors = FALSE)

total <- nrow(odmtx) # set number of records
pb <- progress_bar$new(total = total, format = "(:spin) [:bar] :percent") #progress bar

# API parameters here: 
# http://dev.opentripplanner.org/apidoc/1.0.0/resource_PlannerResource.html

# Connect to the postgres database to store skims
skimdb <- dbConnect(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = "houstontransit",
    host = "localhost",
    user = "alexk",
    password = "JLpIw45")



# Begin the for loop  
times <- paste0(strftime(seq(ISOdatetime(2015,9,1,6,30,0), 
             by = "min", 
             length.out = 120), format = "%H:%M"), "am")

for(i in 1:length(times)) {
  for (j in 1:total) {
  pb$tick()   # update progress bar
  
  response <-
    otpTripTime(
      before,
      from = odmtx[j, ]$origins,
      to = odmtx[j, ]$destinations,
      modes = 'WALK,TRANSIT',
      detail = TRUE,
      date = '2015-08-04',
      time = times[i],
      maxWalkDistance = "3000", # allows 1500m at both ends of journey
      walkReluctance = "2",
      minTransferTime = "600"
    )
  # If response is OK update dataframe
  if (response$errorId == "OK") {
    odmtx[j, "status"] <- response$errorId
    odmtx[j, "duration"] <- response$itineraries$duration
    odmtx[j, "waitingtime"] <- response$itineraries$waitingTime
    odmtx[j, "transfers"] <-response$itineraries$transfers
  } else {
    # record error
    odmtx[j, "status"] <- response$errorId}
  }
  
  dbWriteTable(skimdb, beforeskims, odmtx, append = TRUE)
}
