library(tidycensus)
library(tigris)
library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(progress)

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

centroids <- as.data.frame(st_coordinates(st_centroid(harris_tracts)))
centroids <- cbind(geoid10 = harris_tracts$geoid10, centroids)
centroids$latlong <- paste0(centroids$Y, ",", centroids$X)

odmtx <- expand.grid(
  list(origins = centroids$latlong, 
       destinations = centroids$latlong),
       stringsAsFactors = FALSE)

total <- nrow(odmtx) # set number of records
pb <- progress_bar$new(total = total, format = "(:spin) [:bar] :percent") #progress bar

# API parameters here: 
# http://dev.opentripplanner.org/apidoc/1.0.0/resource_PlannerResource.html

# Begin the for loop  
for (i in 1:total) {
  pb$tick()   # update progress bar
  
  response <-
    otpTripTime(
      before,
      from = odmtx[i, ]$origins,
      to = odmtx[i, ]$destinations,
      modes = 'WALK,TRANSIT',
      detail = TRUE,
      date = '2015-08-04',
      time = '08:00am',
      maxWalkDistance = "3000", # allows 1500m at both ends of journey
      walkReluctance = "2",
      minTransferTime = "600"
    )
  # If response is OK update dataframe
  if (response$errorId == "OK") {
    centroids[i, "status"] <- response$errorId
    centroids[i, "duration"] <- response$itineraries$duration
    centroids[i, "waitingtime"] <- response$itineraries$waitingTime
    centroids[i, "transfers"] <-response$itineraries$transfers
  } else {
    # record error
    centroids[i, "status"] <- response$errorId
  }
}

