library(DBI)
library(MonetDBLite)
library(readr)
library(tigris)
library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)

# Create a local Monet database
dbdir <- "monet_ctpp"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

# Read in lookup tables
lookup_res <- read_delim("lookup tables/acs_2006thru2010_ctpp_res_geo.txt",
                         delim = "|",
                         guess_max = 200000)

lookup_pow <- read_delim("lookup tables/acs_2006thru2010_ctpp_pow_geo.txt",
                         delim = "|",
                         guess_max = 200000)


table_shell <- read_delim("lookup tables/acs_2006thru2010_ctpp_table_shell.txt",
                         delim = "|",
                         guess_max = 200000)

# Convert variable names to lower case and change reserved words
names(lookup_res) <- tolower(names(lookup_res))
names(lookup_pow) <- tolower(names(lookup_pow))
names(table_shell) <- tolower(names(table_shell))

names(lookup_res)[names(lookup_res) == "year"] <- "year_"
names(lookup_pow)[names(lookup_pow) == "year"] <- "year_"

# Write the tables to the db
dbWriteTable(con, "lookupres", lookup_res)
dbWriteTable(con, "lookuppow", lookup_pow)
dbWriteTable(con, "tableshell", table_shell)

# Reproduce Part 1 mode share information for the Houston MSA
monetdb.read.csv(con, 
                 "test data/TX_2006thru2010_A102106.csv",
                 "a102106",
                 nrow.check = 200000,
                 lower.case.names = TRUE)


# Get required subset of the residential geography crosswalk
dbSendQuery(
  con, "
  CREATE TABLE harrisres AS
  SELECT lookupres.geoid, st, cty, tr, lineno, est, se FROM a102106
  INNER JOIN lookupres ON
  a102106.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  st = '48' AND
  cty = '201'")

# Create a standard census geoid
dbSendQuery(
  con, "
  ALTER TABLE harrisres
  ADD COLUMN geoid10 char(11)")

dbSendQuery(
  con, "
  UPDATE harrisres
  SET geoid10 = st || cty || tr") # Double pipe concatenates multiple strings


# dbRemoveTable(con, "harrisres")
# dbGetQuery(con, "SELECT * FROM harrisres LIMIT 15")

harris_res <- dbGetQuery(con, "SELECT * FROM harrisres")

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
ggplot() + geom_sf(data = tracts_wgeo, aes(fill = dashare)) + coord_sf(crs = st_crs(2278))

dbDisconnect(con, shutdown = TRUE)
