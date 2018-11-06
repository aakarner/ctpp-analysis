# This script creates a Monet database and populates it with the tables needed 
# to generate mode- and race-specific origin-destination flow matrices. 

library(DBI)
library(MonetDBLite)
library(readr)
library(tidyr)
library(dplyr)

# Create a local in-memory Monet database to store CTPP data 
dbdir <- "monet_ctpp"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

# Required lookup tables -------------------------------------------------------

# monetdb.read.csv() doesn't work directly with these lookup table files 
# due to an encoding issue. Best solution is to read them first into R and then
# into the database using dbWriteTable().

lookup_res <- read_delim("lookup_tables/acs_2006thru2010_ctpp_res_geo.txt",
                         delim = "|",
                         guess_max = 200000)

lookup_pow <- read_delim("lookup_tables/acs_2006thru2010_ctpp_pow_geo.txt",
                         delim = "|",
                         guess_max = 200000)


table_shell <- read_delim("lookup_tables/acs_2006thru2010_ctpp_table_shell.txt",
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

# Required tables for Part 1 - Place of residence ------------------------------

# Part 1 tables can generally be read directly into the database using 
# monetdb.read.csv().

# Table A102106 - Means of transportation
monetdb.read.csv(con, 
                 "data/TX_2006thru2010_A102106.csv",
                 "a102106",
                 nrow.check = 200000,
                 lower.case.names = TRUE)

# This commented chunk below could be used to generate a table in the databse
# representing a subset of desired geographies. 
# Most of the subsets will be small, though, and can be created as needed 
# on the fly with appropriate queries and data wrangling techniques.

# Get required subset of the residential geography crosswalk
# To extract rows for Harris County, TX
# dbSendQuery(
#   con, "
#   CREATE TABLE harrisres AS
#   SELECT lookupres.geoid, st, cty, tr, lineno, est, se FROM a102106
#   INNER JOIN lookupres ON
#   a102106.geoid = lookupres.geoid
#   WHERE sumlevel = 'C11' AND
#   st = '48' AND
#   cty = '201'")
# 
# # Create a standard census geoid
# dbSendQuery(
#   con, "
#   ALTER TABLE harrisres
#   ADD COLUMN geoid10 char(11)")
# 
# dbSendQuery(
#   con, "
#   UPDATE harrisres
#   SET geoid10 = st || cty || tr") # Double pipe concatenates multiple strings
# 
# dbRemoveTable(con, "harrisres")
# dbGetQuery(con, "SELECT * FROM harrisres LIMIT 15")

# Table B102201 - Minority status by means of transportation (most detailed)
b102201 <- read_csv("data/TX_2006thru2010_B102201.csv")
names(b102201) <- tolower(names(b102201))

dbWriteTable(con, "b102201", b102201)
rm(b102201)

# Required tables for Part 2 - Place of work -----------------------------------

# Table B202200 - Minority status by means of transportation (most detailed)
b202200 <- read_csv("data/TX_2006thru2010_B202200.csv")
names(b202200) <- tolower(names(b202200))

dbWriteTable(con, "b202200", b202200)
rm(b202200)

# Table A202103 - Earnings in the past 12 months
a202103 <- read_csv("data/TX_2006thru2010_A202103.csv")
names(a202103) <- tolower(names(a202103))

dbWriteTable(con, "a202103", a202103)
rm(a202103)

# Required tables for Part 3 - Flow --------------------------------------------

# Part 3 tables cannot be read directly into the database using 
# monetdb.read.csv(). The "source" column throws an error because it contains 
# text in the final row. In general that column seems a little strange. 

# Instead, read required tables directly into R and use dbWriteTable().

# Table A302103 - Means of transportation
# Table B302105 - Minority status
table_names <- c("a302103", "b302105")

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

dbDisconnect(con, shutdown = TRUE)
