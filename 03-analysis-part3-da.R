library(DBI)
library(dplyr)
library(tidyr)
library(abind)
library(mipfp)

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

# Harris County
houston_counties <- c('201')

# H-GAC
# houston_counties <- c('015', '039', '071', '089', '157', '167', '201', 
#                       '291', '321', '339', '471', '473', '481')
       
# CBSA
houston_counties <- c('015', '039', '071', '157', '167', '201', 
                      '291', '321', '339', '455', '471', '473', '477', '481')
               

# A302103 - Means of transportation
# Part 3
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
# Note that Part 1 and Part 2 define summary levels use different codes

# Part 1 - place of residence
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


## Drive alone

# Construct consistent (wide) data for Parts 1-3 so that we can generate
# a consistent set of Os and Ds
flows_da <- flows_mode_tr %>% 
  select(origin, destination, transit) %>%
  filter(transit != 0)

names(flows_da)[3] <- "da"

all_origins <- data.frame(origin = unique(flows_da$origin))
all_destns <- data.frame(destination = unique(flows_da$destination))

origins_da_w <- res_mode_white %>%
  select(origin, da) %>%
  right_join(all_origins)
origins_da_w$da[is.na(origins_da_w$da)] <- 0

origins_da_poc <- res_mode_poc %>%
  select(origin, da) %>%
  right_join(all_origins)
origins_da_poc$da[is.na(origins_da_poc$da)] <- 0

stopifnot(origins_da_w$origin == origins_da_poc$origin)

destinations_da_w <- pow_mode_white %>%
  select(destination, da) %>%
  right_join(all_destns) %>%
  arrange(destination)
destinations_da_w$da[is.na(destinations_da_w$da)] <- 0

destinations_da_poc <- pow_mode_minority %>%
  select(destination, da) %>%
  right_join(all_destns) %>%
  arrange(destination)
destinations_da_poc$da[is.na(destinations_da_poc$da)] <- 0


# QA/QC ------------------------------------------------------------------------

# QA/QC: Does every origin and destination in Part 3 have a corresponding 
# origin and destination in Parts 1 and 2
stopifnot(length(unique(flows_da$origin)) == 
            sum(res_mode_white$origin %in% flows_da$origin)

# Which flows exist that don't have a corresponding row in Part 1
# There are some tracts that have only white or only poc flows
# Proceed assuming that Part 3 is correct. 



# all Part 1 origins have a corresponding flow
res_mode_white[!res_mode_white$origin %in% flows_da$origin, ]

unique(flows_da[!flows_da$origin %in% res_mode_white$origin, "origin"])

# 29 origins
48201
48157

[1] "48157670101" "48157670102" "48201210800" "48201211000" "48201211100" "48201211200" "48201220100" "48201220800" "48201230100" "48201230400"
[11] "48201230600" "48201231000" "48201231200" "48201231500" "48201233600" "48201311400" "48201311600" "48201312800" "48201313300" "48201313600"
[21] "48201331602" "48201331900" "48201332100" "48201530400" "48201531900" "48201533300" "48201980000" "48201980100" "48473680400"


dbGetQuery(con, "SELECT * FROM b102201 
           INNER JOIN lookupres ON
           b102201.geoid = lookupres.geoid
           WHERE sumlevel = 'C11' AND
           cty = '157' AND
           tr = '670101'")

res_mode_white[res_mode_white$origin == '48157670101', ]
res_mode_poc[res_mode_poc$origin == '48157670101', ]

# There are some tracts that have only white/POC flows!!!!!

# Create OD flow matrices for mipfp procedure ----------------------------------

# Normalize Part 1 and 2 totals to Part 3 totals
sum(flows_da$da) # 2,058,379

sum(origins_da_poc$da)# 1118605
sum(destinations_da_poc$da) # 1100309

sum(origins_da_w$da) # 1015001
sum(destinations_da_w$da) # 1004026

# Create a traditional OD flow matrix for drive alone from Part 3 data
flows_da_df <- spread(flows_da, destination, da, fill = 0)
row.names(flows_da_df) <- flows_da_df$origin
flows_da_df <- flows_da_df[,-1]

# Ensure all control totals are in the right order
stopifnot(origins_da_poc$origin == row.names(flows_da_df))
stopifnot(origins_da_w$origin == row.names(flows_da_df))
stopifnot(destinations_da_poc$destination == names(flows_da_df))
stopifnot(destinations_da_w$destination == names(flows_da_df))


# Balance the input marginals
# Split the difference between origins and destinations 
tol <- abs(sum(origins_da_poc$da) - sum(destinations_da_poc$da))

while (tol > 10e-9) { 
  t1 <- (sum(origins_da_poc$da)  + sum(destinations_da_poc$da)) / 2
  origins_da_poc$da <- origins_da_poc$da * (t1 / sum(origins_da_poc$da))
  destinations_da_poc$da <- destinations_da_poc$da * 
    (t1 / sum(destinations_da_poc$da))
  
  t2 <- (sum(destinations_da_w$da) + sum(destinations_da_w$da)) / 2
  origins_da_w$da <- origins_da_w$da * (t2 / sum(origins_da_w$da))
  destinations_da_w$da <- destinations_da_w$da * 
    (t2  / sum(destinations_da_w$da))
  
  # Balance the input marginals to the control totals in part 3
  # Origins
  t3 <- rowSums(flows_da_df) / (origins_da_poc$da + origins_da_w$da)
  origins_da_poc$da <- origins_da_poc$da * t3
  origins_da_w$da <- origins_da_w$da * t3
  
  # Destinations
  t4 <- colSums(flows_da_df) / (destinations_da_poc$da + destinations_da_w$da)
  destinations_da_poc$da <- destinations_da_poc$da * t4
  destinations_da_w$da <- destinations_da_w$da * t4
  
  tol <- abs(sum(origins_da_poc$da) - sum(destinations_da_poc$da))
}
  
  
  
sum(origins_da_poc$da) + sum(origins_da_w$da)
sum(destinations_da_poc$da) + sum(destinations_da_w$da)


# mipfp algorithm

origins <- rowSums(flows_da_df)
destinations <- colSums(flows_da_df)
race <- c(poc = sum(origins_da_poc$da), white = sum(origins_da_w$da))

# Cross1 is the flow matrix, flows_da_df
cross1 <- as.matrix(flows_da_df)
rownames(cross1) <- names(origins)
colnames(cross1) <- names(destinations)

cross2 <- cbind(origins_da_poc$da, origins_da_w$da)
rownames(cross2) <- names(origins)
colnames(cross2) <- names(race)

cross3 <- cbind(destinations_da_poc$da, destinations_da_w$da)
rownames(cross3) <- names(destinations)
colnames(cross3) <- names(race)

target <- list(origins, destinations, race, cross1, cross2, cross3)
descript <- list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3))

# Define the seed matrix based on the total share of POC/white trips

flows_da_df_poc <- flows_da_df * sum(origins_da_poc$da) / sum(flows_da$da)
flows_da_df_w <- flows_da_df * (1 - sum(origins_da_poc$da) / sum(flows_da$da))


names <- list(names(origins), names(destinations), names(race))

seed_mtx <- abind(flows_da_df_poc, flows_da_df_w, along = 3, new.names = names)



# running the Ipfp function
result <- Ipfp(seed_mtx, descript, target, iter = 500, 
               print = TRUE, tol = 1e-5)

write.csv(result$x.hat[,,1], "pocslice.csv")
write.csv(result$x.hat[,,2], "whiteslice.csv")



dbDisconnect(con, shutdown = TRUE)
