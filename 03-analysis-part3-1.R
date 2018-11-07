# These two scripts (part3-1 and part3-2) create synthetic journey-to-work flows 
# for drive alone and transit cross-tabulated with race/ethnicity 
# using iterative proportional fitting. 
# These tables do not exist in the public distribution of the CTPP so must be 
# created. 
# part3-1 queries the CTPP database to extract all required control totals
# and flows. part 3-2 makes all marginals consistent and executes an iterative
# proportional fitting procedure to generate the required flow tables.
# Run part3-1 first, followed by part3-2.

library(DBI)
library(dplyr)
library(tidyr)
library(abind)
library(mipfp)

dbdir <- "monet_ctpp"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

# Get flows only for destinations within the H-GAC region
# This definition might reflect political arrangements
# Austin
# Brazoria
# Chambers
# Colorado (not in CBSA)
# Fort Bend
# Galveston
# Harris
# Liberty
# Matagorda
# Montgomery
# Walker
# Waller
# Wharton

# The 2012 combined statistical area includes:
# Austin
# Brazoria
# Chambers
# Fort Bend
# Galveston
# Harris
# Liberty
# Matagorda
# Montgomery
# Trinity
# Waller
# Walker
# WASHINGTON
# Wharton

# Harris County
# houston_counties <- c('201')

# H-GAC
# houston_counties <- c('015', '039', '071', '089', '157', '167', '201', 
#                       '291', '321', '339', '471', '473', '481')
       
# CBSA
houston_counties <- c('015', '039', '071', '157', '167', '201', 
                      '291', '321', '339', '455', '471', '473', '477', '481')
               

# A302103 - Means of transportation
# Part 3
# This query extracts all flows at the tract level where the origin and 
# destination tracts are in houston_counties
flows_mode_tr <- dbGetQuery(con, "SELECT * FROM a302103_tract") %>%
  select(geoid, origin, destination, lineno, est) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("geoid", "origin", "destination", 
                "total", "da", "cp2", "cp3", "cp4", "cp56", "cp7p", 
                "bus", "streetcar", "subway", "railroad", "ferry", "bike", 
                "walk", "taxi", "motorcycle", "other", "telecommute")) %>%
  mutate(destcty = substr(destination, 3, 5),
         origcty = substr(origin, 3, 5),
         destst = substr(destination, 1, 2), 
         carpool = cp2 + cp3 + cp4 + cp56 + cp7p,
         transit = bus + streetcar + subway,
         nonmotorized = bike + walk,
         other = railroad + ferry + taxi + motorcycle + other,
         qa = da + carpool + transit + nonmotorized + telecommute + other) %>%
  filter(destcty %in% houston_counties & origcty %in% houston_counties & 
           destst == 48)

# Get "lineno" information on both tables from the table shell
# res_mode_lookup <- dbGetQuery(
#   con, "
#   SELECT * FROM tableshell 
#   WHERE tblid = 'B102201'")
# 
# pow_mode_lookup <- dbGetQuery(
#   con, "
#   SELECT * FROM tableshell 
#   WHERE tblid = 'B202200'")

# Extract marginals for the study area
# Note that Part 1 and Part 2 define summary levels using different codes

# Part 1 - place of residence
# This query extracts place-of-residence control totals for the white population
res_mode_white <- dbGetQuery(
  con, paste0("
  SELECT st || cty || tr AS geoid10, lineno, est  
  FROM b102201
  INNER JOIN lookupres ON
  b102201.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  lineno BETWEEN 12 AND 22 AND
  cty IN 
    (", paste0("'", paste0(houston_counties, collapse = "','"), "'"), ")")) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("origin", "total", "da", "cp2", "cp3p", "bus", 
                "streetcar_subway", "rr_ferry", "bike", "walk", "taxi_other", 
                "telecommute")) %>%
  mutate(origcty = substr(origin, 3, 5),
         carpool = cp2 + cp3p,
         transit = bus + streetcar_subway,
         nonmotorized = bike + walk,
         other = rr_ferry + taxi_other,
         qa = da + carpool + transit + nonmotorized + telecommute + other)

# This query extracts place-of-residence control totals for the poc population
res_mode_poc <- dbGetQuery(
  con, paste0("
  SELECT st || cty || tr AS geoid10, lineno, est  
  FROM b102201
  INNER JOIN lookupres ON
  b102201.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  lineno BETWEEN 23 AND 33 AND
  cty IN 
    (", paste0("'", paste0(houston_counties, collapse = "','"), "'"), ")")) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("origin", "total", "da", "cp2", "cp3p", "bus", 
                "streetcar_subway", "rr_ferry", "bike", "walk", "taxi_other", 
                "telecommute")) %>%
  mutate(origcty = substr(origin, 3, 5),
         carpool = cp2 + cp3p,
         transit = bus + streetcar_subway,
         nonmotorized = bike + walk,
         other = rr_ferry + taxi_other,
         qa = da + carpool + transit + nonmotorized + telecommute + other)

# Part 2 - place of work
# This query extracts place-of-work control totals for the white population
pow_mode_white <- dbGetQuery(
  con, paste0("
  SELECT st || cty || tr AS geoid10, lineno, est  
  FROM b202200
  INNER JOIN lookuppow ON
  b202200.geoid = lookuppow.geoid
  WHERE sumlevel = 'C31' AND
  lineno BETWEEN 12 AND 22 AND
  cty IN 
    (", paste0("'", paste0(houston_counties, collapse = "','"), "'"), ")")) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("destination", "total", "da", "cp2", "cp3p", "bus", 
                "streetcar_subway", "rr_ferry", "bike", "walk", "taxi_other", 
                "telecommute")) %>%
  mutate(destcty = substr(destination, 3, 5),
         carpool = cp2 + cp3p,
         transit = bus + streetcar_subway,
         nonmotorized = bike + walk,
         other = rr_ferry + taxi_other,
         qa = da + carpool + transit + nonmotorized + telecommute + other)

# This query extracts place-of-work control totals for the poc population
pow_mode_minority <- dbGetQuery(
  con, paste0("
  SELECT st || cty || tr AS geoid10, lineno, est  
  FROM b202200
  INNER JOIN lookuppow ON
  b202200.geoid = lookuppow.geoid
  WHERE sumlevel = 'C31' AND
  lineno BETWEEN 23 AND 33 AND
  cty IN 
    (", paste0("'", paste0(houston_counties, collapse = "','"), "'"), ")")) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("destination", "total", "da", "cp2", "cp3p", "bus", 
                "streetcar_subway", "rr_ferry", "bike", "walk", "taxi_other", 
                "telecommute")) %>%
  mutate(destcty = substr(destination, 3, 5),
         carpool = cp2 + cp3p,
         transit = bus + streetcar_subway,
         nonmotorized = bike + walk,
         other = rr_ferry + taxi_other,
         qa = da + carpool + transit + nonmotorized + telecommute + other)

dbDisconnect(con)