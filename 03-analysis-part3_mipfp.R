library(DBI)
library(dplyr)
library(tidyr)

# Goal of this section is to create synthetic journey-to-work flows 
# by mode cross-tabulated with race/ethnicity

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

# H-GAC
houston_counties <- c('015', '039', '071', '089', '157', '167', '201', 
                      '291', '321', '339', '471', '473', '481')
       
# CBSA
houston_counties <- c('015', '039', '071', '157', '167', '201', 
                      '291', '321', '339', '455', '471', '473', '477', '481')
               

# A302103 - Means of transportation
flows_mode_tr <- dbGetQuery(con, "SELECT * FROM a302103_tract") %>%
  select(geoid, origin, destination, lineno, est) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("geoid", "origin", "destination", 
                "total", "da", "cp2", "cp3", "cp4", "cp56", "cp7p", 
                "bus", "streetcar", "subway", "railroad", "ferry", "bike", 
                "walk", "taxi", "motorcycle", "other", "telecommute")) %>%
  mutate(destcty = substr(destination, 3, 5),
         origcty = substr(origin, 3, 5),
         carpool = cp2 + cp3 + cp4 + cp56 + cp7p,
         transit = bus + streetcar + subway,
         nonmotorized = bike + walk,
         other = railroad + ferry + taxi + motorcycle + other,
         qa = da + carpool + transit + nonmotorized + telecommute + other) %>%
  filter(destcty %in% houston_counties & origcty %in% houston_counties)

# The above call will give me all origins that have a destination bound for 
# H-GAC -- this could include origins that lie outside of the region. 
# 

# The minority-specific flows generated in the chunk below could be used 
# to split the flows by mode, assuming that the share of white/poc 
# is constant across modes, but we know this isn't true. 

# flows_minority_tr <- dbGetQuery(con, "SELECT * FROM b302105_tract")
# 
# # Get "lineno" information from the table shell
# # minority_names <- dbGetQuery(
# #   con, "
# #   SELECT * FROM tableshell 
# #   WHERE tblid = 'B302105'")
# 
# flows_minority_tr <- flows_minority_tr %>%
#   select(geoid, origin, destination, lineno, est) %>%
#   spread(lineno, est, fill = 0) %>%
#  `colnames<-`(c("geoid", "origin", "destination", 

#                 "total", "white_alone", "other")) %>%
#   mutate(white_share = white_alone / total,
#          poc_share = other / total) # It looks like 'other' is poc
# 
# # Join the mode share and minority status data to generate synthetic flows
# flows_mode_minority <- 
#   inner_join(flows_mode_tr, flows_minority_tr, by = c("geoid"))

# Extract marginals for the entire state
# Note that Part 1 and Part 2 define summary levels use different codes
res_mode_minority <- dbGetQuery(
  con, paste0("
  SELECT lookupres.geoid, st || cty || tr AS geoid10, 
  st, cty, tr, lineno, est, se 
  FROM b102201
  INNER JOIN lookupres ON
  b102201.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  cty IN (", paste0(houston_counties, collapse = ","), ")"))

# Flows into a specific county or counties
pow_mode_minority <- dbGetQuery(
  con, paste0("
  SELECT lookuppow.geoid, st || cty || tr AS geoid10, 
  st, cty, tr, lineno, est, se
  FROM b202200
  INNER JOIN lookuppow ON
  b202200.geoid = lookuppow.geoid
  WHERE sumlevel = 'C31' AND
  st = '48' AND 
  cty IN (", paste0(houston_counties, collapse = ","), ")"))

# Flows into the entire state
# pow_mode_minority <- dbGetQuery(
#   con, paste0("
#   SELECT lookuppow.geoid, st || cty || tr AS geoid10, 
#   st, cty, tr, lineno, est, se
#   FROM b202200
#   INNER JOIN lookuppow ON
#   b202200.geoid = lookuppow.geoid
#   WHERE sumlevel = 'C31' AND
#   st = '48'"))




# Get "lineno" information on both tables from the table shell
res_mode_lookup <- dbGetQuery(
  con, "
  SELECT * FROM tableshell 
  WHERE tblid = 'B102201'")

pow_mode_lookup <- dbGetQuery(
  con, "
  SELECT * FROM tableshell 
  WHERE tblid = 'B202200'")

# Extract a da alone flow matrix because we want mode-specific tables
flows_da <- flows_mode_tr %>%
  select(origin, destination, da) %>%
  filter(da != 0)

# Create a traditional OD flow matrix for drive alone from Part 3 data
# This will also be the stack control
flows_da_df <- spread(flows_da, destination, da, fill = 0)
flows_da_mtx <- data.matrix(flows_da_df[, -1])

# Quality assurance/Quality control
# Part 3 total trips within CBSA
sum(flows_mode_tr$da)

# Part 1 total trips originating within CBSA
res_mode_minority %>%
  filter(lineno == 2) %>%
  summarize(total = sum(est))

# Part 2 total trips destined within CBSA
pow_mode_minority %>%
  filter(lineno == 2) %>%
  summarize(total = sum(est))


# Extract a transit alone flow matrix because we want mode-specific tables
flows_tran <- flows_mode_tr %>%
  select(origin, destination, transit) %>%
  filter(transit != 0)

# Create a traditional OD flow matrix for transit from Part 3 data
flows_tran_df <- spread(flows_tran, destination, transit, fill = 0)
flows_tran_mtx <- data.matrix(flows_tran_df[, -1])

# Quality assurance/Quality control
# Part 3 total trips within CBSA
sum(flows_mode_tr$transit)

# Part 1 total trips originating within CBSA
res_mode_minority %>%
  filter(lineno %in% c(5, 6)) %>%
  summarize(total = sum(est))

# Part 2 total trips destined within CBSA
pow_mode_minority %>%
  filter(lineno %in% c(5, 6)) %>%
  summarize(total = sum(est))



# Do I actually have to create a new flow table using the controls from Part 1 and 2?
# If yes, this would just be a 2D contingency table...
# Wouldn't I still want to control to the total number of trips...?
# No, because each individual trip total would be controlled. 

# Create control totals
# Some of these might have no rows returned so they'll need to be populated
# with zeros -- use fill = 0 in spread()

# Summing over rows gives you control totals for the origins
row_control <- res_mode_minority %>%
  filter(lineno %in% c(13, 24) & geoid10 %in% flows_da$origin) %>%
  select(geoid10, lineno, est) %>%
  spread(lineno, est, fill = 0)

# The flow matrix contains 2,175 trips, but the column control contains
# 55,075 because it's looking at all destinations, regardless of origin. 
# Multiply the control matrix by the total trips we have over the total trips
# in the column control matrix 2175 / 55070 = 0.03949519.
# Presumably a less severe adjustment would be needed if a regional scale
# was being used. 
# row_control_mtx <- t(data.matrix(row_control[, -1])) * 2175 / 55070

# Summing over columns gives you control totals for the destinations
col_control <- pow_mode_minority %>%
  filter(lineno %in% c(13, 24) & geoid10 %in% flows_da$destination) %>%
  select(geoid10, lineno, est) %>%
  spread(lineno, est, fill = 0)





col_control_mtx <- t(data.matrix(row_control[, -1])) * 2175 / 2220

sum(col_control$`13`)
sum(col_control$`24`)
sum(row_control$`13`)
sum(row_control$`24`)



seed <- array(1, c(39, 6, 2))

# list of dimensions of each marginal constrain
tgt.data.3d <- list(row_control_dummy, col_control_dummy, flows_da_mtx)
# storing the description of target data in a list
tgt.list.3d <- list( 1, 2, 3)
# calling the Ipfp function
res.3d <- Ipfp(seed, tgt.list.3d, tgt.data.3d, iter=50, print=TRUE, tol=1e-5)


dbDisconnect(con, shutdown = TRUE)
