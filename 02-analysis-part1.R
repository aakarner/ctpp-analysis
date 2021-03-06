# This script generates a couple simple maps from the CTPP Part 1 data that were 
# read into a Monet database in the 01 script.
# Specifically, it pulls down mode share information for Harris County, TX and 
# plots a couple choropleth maps. 

library(DBI)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tigris)

# Connect to the local Monet database
dbdir <- "monet_ctpp"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

harris_res <- dbGetQuery(
  con, 
  "SELECT lookupres.geoid, st, cty, tr, lineno, est, se FROM a102106
  INNER JOIN lookupres ON
  a102106.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  st = '48' AND
  cty = '201'")
                         
harris_res$geoid10 <- paste0(harris_res$st, harris_res$cty, harris_res$tr)

# Get "lineno" information from the table shell
mode_shares_res <- dbGetQuery(
  con, "
  SELECT * FROM tableshell 
  WHERE tblid = 'A102106'")

# https://stackoverflow.com/questions/28100780/use-with-replacement-functions-like-colnames
harris_res_wide <- harris_res %>%
  select(geoid10, lineno, est) %>%
  spread(lineno, est, fill = 0) %>%
   `colnames<-`(c("geoid10", "total", "da", "cp2", "cp3", "cp4", "cp56", "cp7p", 
             "bus", "streetcar", "subway", "railroad", "ferry", "bike", "walk",
             "taxi", "motorcycle", "other", "telecommute")) %>%
  mutate(carpool = cp2 + cp3 + cp4 + cp56 + cp7p,
         transit = bus + streetcar + subway,
         dashare = da / total,
         transhare = transit / total)

# Retreive Harris county tracts from tigris
tracts_harris <- tracts("TX", county = "201")

# Join and map part 1 data 
tracts_wgeo <- inner_join(tracts_harris, harris_res_wide,
                            by = c("GEOID" = "geoid10"))

ggplot() + geom_sf(data = tracts_wgeo, aes(fill = transhare))
ggplot() + geom_sf(data = tracts_wgeo, aes(fill = dashare))

dbDisconnect(con, shutdown = TRUE)