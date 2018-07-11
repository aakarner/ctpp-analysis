library(tidyr)

# Goal of this section is to create synthetic journey-to-work flows 
# by mode cross-tabulated with race/ethnicity

dbdir <- "monet_ctpp"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

# Get flows only for destinations within a single county
flows_mode_tr <- dbGetQuery(con, "SELECT * FROM a302103_tract") %>%
  select(geoid, origin, destination, lineno, est) %>%
  spread(lineno, est, fill = 0) %>%
 `colnames<-`(c("geoid", "origin", "destination", 
                "total", "da", "cp2", "cp3", "cp4", "cp56", "cp7p", 
                "bus", "streetcar", "subway", "railroad", "ferry", "bike", 
                "walk", "taxi", "motorcycle", "other", "telecommute")) %>%
  mutate(destcty = substr(destination, 3, 5),
         carpool = cp2 + cp3 + cp4 + cp56 + cp7p,
         transit = bus + streetcar + subway,
         nonmotorized = bike + walk,
         other = railroad + ferry + taxi + motorcycle + other,
         qa = da + carpool + transit + nonmotorized + telecommute + other) %>%
  filter(destcty == "253") # 253 Jones County


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
# Note that Part 1 and Part 2 have different summary levels
res_mode_minority <- dbGetQuery(
  con, "
  SELECT lookupres.geoid,   st || cty || tr AS geoid10, 
  st, cty, tr, lineno, est, se 
  FROM b102201
  INNER JOIN lookupres ON
  b102201.geoid = lookupres.geoid
  WHERE sumlevel = 'C11' AND
  st = '48'") # No county condition here - want residents regarless of location

pow_mode_minority <- dbGetQuery(
  con, "
  SELECT lookuppow.geoid, st || cty || tr AS geoid10, 
  st, cty, tr, lineno, est, se
  FROM b202200
  INNER JOIN lookuppow ON
  b202200.geoid = lookuppow.geoid
  WHERE sumlevel = 'C31' AND
  st = '48' AND 
  cty = '253'") 

# Get "lineno" information on both tables from the table shell
res_mode_lookup <- dbGetQuery(
  con, "
  SELECT * FROM tableshell 
  WHERE tblid = 'B102201'")

pow_mode_lookup <- dbGetQuery(
  con, "
  SELECT * FROM tableshell 
  WHERE tblid = 'B202200'")

# Extract a drive alone flow matrix
flows_da <- flows_mode_tr %>%
  select(origin, destination, da) %>%
  filter(da != 0)

# Create a traditional OD flow matrix for drive alone
# This will also be the stack control
flows_da_df <- spread(flows_da, destination, da, fill = 0)
flows_da_mtx <- t(data.matrix(flows_da_df[, -1]))

# Create control totals
# Some of these might have no rows returned so they'll need to be populated
# with zeros -- use fill = 0 in spread()

col_control <- res_mode_minority %>%
  filter(lineno %in% c(13, 24) & geoid10 %in% flows_da$origin) %>%
  select(geoid10, lineno, est) %>%
  spread(lineno, est, fill = 0)

# The flow matrix contains 2,175 trips, but the column control contains
# 55,075 because it's looking at all destinations, regardless of origin. 
# Multiply the control matrix by the total trips we have over the total trips
# in the column control matrix 2175 / 55070 = 0.03949519.
# Presumably a less severe adjustment would be needed if a regional scale
# was being used. 
col_control_mtx <- t(data.matrix(col_control[, -1])) * 2175 / 55070

# Same adjustment is needed for the row_control matrix
row_control <- pow_mode_minority %>%
  filter(lineno %in% c(13, 24) & geoid10 %in% flows_da$destination) %>%
  select(geoid10, lineno, est) %>%
  spread(lineno, est, fill = 0)

row_control_mtx <- t(data.matrix(row_control[, -1])) * 2175 / 2220

# The seed matrix should just split the stack control by the overall pop. shares
shares <- row_control %>%
  group_by(lineno) %>%
  summarize(totpop = sum(est))

# White share is 1680 / (1680 + 540) = 76%

seed <- array(c(flows_da_mtx * 0.76,
                flows_da_mtx * 0.24), c(39, 6, 2))

source("00-function-definitions.R")

# Look at using mipfp instead.
# Examples here: https://spatial-microsim-book.robinlovelace.net/smsim-in-r

ipf <- ipf3df(row_control_mtx, col_control_mtx, flows_da_mtx, seed,
              maxiter = 5000)

write_csv(row_control, "rows.csv")
write_csv(col_control, "cols.csv")
write_csv(flows_da_df, "stack.csv")

dbDisconnect(con, shutdown = TRUE)
