# These two scripts (part3-1 and part3-2) create synthetic journey-to-work flows 
# for drive alone and transit cross-tabulated with race/ethnicity 
# using iterative proportional fitting. 
# These tables do not exist in the public distribution of the CTPP so must be 
# created. 
# part3-1 queries the CTPP database to extract all required control totals
# and flows. part 3-2 makes all marginals consistent and executes an iterative
# proportional fitting procedure to generate the required flow tables.
# Run part3-1 first, followed by part3-2.

## Synthetic drive alone flows by race/ethnicity -------------------------------

# Construct a set of consistent origins and destinations for Parts 1-3.
# This step is necessary because we have race/ethnicity specific information
# from Parts 1 and 2 but not from Part 3. There are likely to be origins and
# destinations in the Part 3 file that do not exist in either the Part 1 or 2
# file. 

# Extract long tract-tract flow data for drive alone
flows_da <- flows_mode_tr %>% 
  select(origin, destination, da) %>%
  filter(da != 0)

# All origins and destinations from Part 3
all_origins <- data.frame(origin = unique(flows_da$origin))
all_destns <- data.frame(destination = unique(flows_da$destination))

# Create a list of Part 1 origins for both white and poc
origins_da_w <- res_mode_white %>%
  select(origin, da) %>%
  right_join(all_origins)
origins_da_w$da[is.na(origins_da_w$da)] <- 0

origins_da_poc <- res_mode_poc %>%
  select(origin, da) %>%
  right_join(all_origins)
origins_da_poc$da[is.na(origins_da_poc$da)] <- 0

# Make sure that the list of origins in each group is the same
stopifnot(origins_da_w$origin == origins_da_poc$origin)

# Create a list of Part 2 destinations for both white and poc
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

## Create OD flow matrices for mipfp procedure

# Normalize Part 1 and 2 totals to Part 3 totals
sum(flows_da$da) # 2,058,379

sum(origins_da_poc$da)# 1,118,605
sum(destinations_da_poc$da) # 1,100,309

sum(origins_da_w$da) # 1,015,001
sum(destinations_da_w$da) # 1,004,026

# Create a wide OD flow matrix for drive alone from Part 3 data
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
  
stopifnot((sum(origins_da_poc$da) + sum(origins_da_w$da)) ==
  sum(destinations_da_poc$da) + sum(destinations_da_w$da))

## Execute the iterative proportional fitting technique
# The Lovelace book on spatial microsimulation is very helpful here
# https://spatial-microsim-book.robinlovelace.net/smsimr.html#mipfp

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

# Execute the Ipfp function
result <- Ipfp(seed_mtx, descript, target, iter = 500, 
               print = TRUE, tol = 1e-5)

# Write the output
write.csv(result$x.hat[,,1], "output/pocslice_da.csv")
write.csv(result$x.hat[,,2], "output/whiteslice_da.csv")

## Synthetic public transit flows by race/ethnicity -------------------------------

# Construct a set of consistent origins and destinations for Parts 1-3.
# This step is necessary because we have race/ethnicity specific information
# from Parts 1 and 2 but not from Part 3. There are likely to be origins and
# destinations in the Part 3 file that do not exist in either the Part 1 or 2
# file. 

# Construct consistent (wide) data for Parts 1-3 so that we can generate
# a consistent set of Os and Ds
flows_tran <- flows_mode_tr %>% 
  select(origin, destination, transit) %>%
  filter(transit != 0)

all_origins <- data.frame(origin = unique(flows_tran$origin))
all_destns <- data.frame(destination = unique(flows_tran$destination))

origins_tran_w <- res_mode_white %>%
  select(origin, transit) %>%
  right_join(all_origins)
origins_tran_w$transit[is.na(origins_tran_w$transit)] <- 0

origins_tran_poc <- res_mode_poc %>%
  select(origin, transit) %>%
  right_join(all_origins)
origins_tran_poc$transit[is.na(origins_tran_poc$transit)] <- 0

stopifnot(origins_tran_w$origin == origins_tran_poc$origin)

destinations_tran_w <- pow_mode_white %>%
  select(destination, transit) %>%
  right_join(all_destns) %>%
  arrange(destination)
destinations_tran_w$transit[is.na(destinations_tran_w$transit)] <- 0

destinations_tran_poc <- pow_mode_minority %>%
  select(destination, transit) %>%
  right_join(all_destns) %>%
  arrange(destination)
destinations_tran_poc$transit[is.na(destinations_tran_poc$transit)] <- 0

# Create OD flow matrices for mipfp procedure ----------------------------------

# Normalize Part 1 and 2 totals to Part 3 totals
sum(flows_tran$transit) # 66,768

sum(origins_tran_poc$transit)# 50222
sum(destinations_tran_poc$transit) # 50133

sum(origins_tran_w$transit) # 17194
sum(destinations_tran_w$transit) # 17367

# Create a traditional OD flow matrix for drive alone from Part 3 data
flows_tran_df <- spread(flows_tran, destination, transit, fill = 0)
row.names(flows_tran_df) <- flows_tran_df$origin
flows_tran_df <- flows_tran_df[,-1]

# Ensure all control totals are in the right order
stopifnot(origins_tran_poc$origin == row.names(flows_tran_df))
stopifnot(origins_tran_w$origin == row.names(flows_tran_df))
stopifnot(destinations_tran_poc$destination == names(flows_tran_df))
stopifnot(destinations_tran_w$destination == names(flows_tran_df))

# Balance the input marginals
# Split the difference between origins and destinations 
tol <- abs(sum(origins_tran_poc$transit) - sum(destinations_tran_poc$transit))

while (tol > 10e-9) { 
  t1 <- (sum(origins_tran_poc$transit)  + sum(destinations_tran_poc$transit)) / 2
  origins_tran_poc$transit <- origins_tran_poc$transit * (t1 / sum(origins_tran_poc$transit))
  destinations_tran_poc$transit <- destinations_tran_poc$transit * 
    (t1 / sum(destinations_tran_poc$transit))
  
  t2 <- (sum(destinations_tran_w$transit) + sum(destinations_tran_w$transit)) / 2
  origins_tran_w$transit <- origins_tran_w$transit * (t2 / sum(origins_tran_w$transit))
  destinations_tran_w$transit <- destinations_tran_w$transit * 
    (t2  / sum(destinations_tran_w$transit))
  
  # Balance the input marginals to the control totals in part 3
  # Origins
  t3 <- rowSums(flows_tran_df) / (origins_tran_poc$transit + origins_tran_w$transit)
  origins_tran_poc$transit <- origins_tran_poc$transit * t3
  origins_tran_w$transit <- origins_tran_w$transit * t3
  
  # Destinations
  t4 <- colSums(flows_tran_df) / (destinations_tran_poc$transit + destinations_tran_w$transit)
  destinations_tran_poc$transit <- destinations_tran_poc$transit * t4
  destinations_tran_w$transit <- destinations_tran_w$transit * t4
  
  tol <- abs(sum(origins_tran_poc$transit) - sum(destinations_tran_poc$transit))
}

stopifnot((sum(origins_tran_poc$transit) + sum(origins_tran_w$transit)) == 
sum(destinations_tran_poc$transit) + sum(destinations_tran_w$transit))

# mipfp algorithm
origins <- rowSums(flows_tran_df)
destinations <- colSums(flows_tran_df)
race <- c(poc = sum(origins_tran_poc$transit), white = sum(origins_tran_w$transit))

# Cross1 is the flow matrix, flows_tran_df
cross1 <- as.matrix(flows_tran_df)
rownames(cross1) <- names(origins)
colnames(cross1) <- names(destinations)

cross2 <- cbind(origins_tran_poc$transit, origins_tran_w$transit)
rownames(cross2) <- names(origins)
colnames(cross2) <- names(race)

cross3 <- cbind(destinations_tran_poc$transit, destinations_tran_w$transit)
rownames(cross3) <- names(destinations)
colnames(cross3) <- names(race)

target <- list(origins, destinations, race, cross1, cross2, cross3)
descript <- list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3))

# Define the seed matrix based on the total share of POC/white trips

flows_tran_df_poc <- flows_tran_df * sum(origins_tran_poc$transit) / sum(flows_tran$transit)
flows_tran_df_w <- flows_tran_df * (1 - sum(origins_tran_poc$transit) / sum(flows_tran$transit))

names <- list(names(origins), names(destinations), names(race))

seed_mtx <- abind(flows_tran_df_poc, flows_tran_df_w, along = 3, new.names = names)

# running the Ipfp function
result <- Ipfp(seed_mtx, descript, target, iter = 500, 
               print = TRUE, tol = 1e-5)

write.csv(result$x.hat[,,1], "output/pocslice_transit.csv")
write.csv(result$x.hat[,,2], "output/whiteslice_transit.csv")

pocslice <- as.data.frame(result$x.hat[,,1]) %>%
  mutate(origin = row.names(.)) %>%
  gather(destination, trips, -origin) %>%
  filter(trips != 0)
pocslice$race <- "people of color"

whiteslice <- as.data.frame(result$x.hat[,,2]) %>%
  mutate(origin = row.names(.)) %>%
  gather(destination, trips, -origin) %>%
  filter(trips != 0)
whiteslice$race <- "white"

# Final trip table
race_trips <- rbind(pocslice, whiteslice)
