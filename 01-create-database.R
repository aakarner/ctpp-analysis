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

# Example analyses -------------------------------------------------------------

# PART 1 - Place of residence --------------------------------------------------

# Table A102106 - Means of transportation
monetdb.read.csv(con, 
                 "data/TX_2006thru2010_A102106.csv",
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


# Part 3 - Flow ----------------------------------------------------------------

# Goal of this section is to create synthetic journey-to-work flows 
# by mode cross-tabulated with race/ethnicity

# Table A302103 - Means of transportation
# Table B302105 - Minority status
table_names <- c("a302103", "b302105")

# monetdb.read.csv() doesn't work with this file - the "source" column 
# throws an error because it contains text in the final row. 
# In general that column seems a little strange. 
a302103 <- read_csv("data/TX_2006thru2010_A302103.csv")
names(a302103) <- tolower(names(a302103))
dbWriteTable(con, "a302103", a302103)


b302105 <- read_csv("data/TX_2006thru2010_B302105.csv")
names(b302105) <- tolower(names(b302105))
dbWriteTable(con, "b302105", b302105)

# Extract tract-level geographic identifiers from flow tables
# Create separate origin and destination variables
for(i in table_names) { 
  # Define a summary level variable (why isn't this in the table to begin with?) 
  dbSendQuery(
    con, paste0("
    ALTER TABLE ", i, "
    ADD COLUMN sumlev char(3)"))

  dbSendQuery(
    con, paste0("
    UPDATE ", i, "
    SET sumlev = SUBSTRING(geoid, 1, 3)"))
  
  # Create a new table containing tract-tract pairs (sumlev C54)
  dbSendQuery(
    con, paste0("
    CREATE TABLE ", i,"_tract
    AS SELECT geoid, lineno, est, se
    FROM ", i, " 
    WHERE sumlev = 'C54'"))
  
  dbSendQuery(
    con, paste0("
    ALTER TABLE ", i, "_tract
    ADD COLUMN origin char(11)"))
  
  dbSendQuery(
    con, paste0("
    ALTER TABLE ", i, "_tract
    ADD COLUMN destination char(11)"))
  
  dbSendQuery(
    con, paste0("
    UPDATE ", i, "_tract
    SET origin = SUBSTRING(geoid, 8, 11)"))
  
  dbSendQuery(
    con, paste0("
    UPDATE ", i, "_tract
    SET destination = SUBSTRING(geoid, 19, 11)"))
}

# Clean up
rm(a302103)
rm(b302105)

# dbRemoveTable(con, "a302103")
# dbRemoveTable(con, "a302103_tract")
# dbRemoveTable(con, "b302105")
# dbRemoveTable(con, "b302105_tract")



# Create a data frame with flows by mode
flows_mode_tr <- dbGetQuery(con, "SELECT * FROM a302103_tract")

flows_mode_tr <- flows_mode_tr %>%
  select(geoid, origin, destination, lineno, est) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("geoid", "origin", "destination", 
                "total", "da", "cp2", "cp3", "cp4", "cp56", "cp7p", 
                "bus", "streetcar", "subway", "railroad", "ferry", "bike", 
                "walk", "taxi", "motorcycle", "other", "telecommute")) %>%
  mutate(carpool = cp2 + cp3 + cp4 + cp56 + cp7p,
         transit = bus + streetcar + subway,
         dashare = da / total,
         transhare = transit / total)


flows_minority_tr <- dbGetQuery(con, "SELECT * FROM b302105_tract")

# Get "lineno" information from the table shell
# minority_names <- dbGetQuery(
#   con, "
#   SELECT * FROM tableshell 
#   WHERE tblid = 'B302105'")

flows_minority_tr <- flows_minority_tr %>%
  select(geoid, origin, destination, lineno, est) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("geoid", "origin", "destination", 
                "total", "white_alone", "other")) %>%
  mutate(white_share = white_alone / total,
         poc_share = other / total) # It looks like 'other' is poc

# Join the mode share and minority status data to generate synthetic flows
flows_mode_minority <- 
  inner_join(flows_mode_tr, flows_minority_tr, by = c("geoid"))

dbDisconnect(con, shutdown = TRUE)
