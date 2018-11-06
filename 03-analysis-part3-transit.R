## Drive alone

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
  
  
  
sum(origins_tran_poc$transit) + sum(origins_tran_w$transit)
sum(destinations_tran_poc$transit) + sum(destinations_tran_w$transit)


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

write.csv(result$x.hat[,,1], "output/mipfp_pocslice_transit.csv")
write.csv(result$x.hat[,,2], "output/mipfp_whiteslice_transit.csv")

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

# Look up before and after trip properties and join them to the table
trips_final <- 
  inner_join(
    race_trips,
    select(before_skims, durBef = duration, walkBef = walkTime, tranBef = transitTime,
           waitBef = waitingTime, transBef = transfers, geoid_o, geoid_d),
    by = c("origin" = "geoid_o", "destination" = "geoid_d")) %>%
  inner_join(
    select(after_skims, durAft = duration, walkAft = walkTime, tranAft = transitTime,
           waitAft = waitingTime, transAft = transfers, geoid_o, geoid_d),
    by = c("origin" = "geoid_o", "destination" = "geoid_d"))
  

# User benefits analysis
# TODO: Investigate why these trips aren't being skimmed
# Do they actually not exist? or are they outside of Metro's service area?
sum(is.na(trips_final$durAft)) #45
sum(is.na(trips_final$durBef)) #40

trips_final %>%
  group_by(race) %>%
  summarize(durBef = weighted.mean(durBef, trips, na.rm = TRUE),
            walkBef = weighted.mean(walkBef, trips, na.rm = TRUE),
            tranBef = weighted.mean(tranBef, trips, na.rm = TRUE),
            waitBef = weighted.mean(waitBef, trips, na.rm = TRUE),
            transBef = weighted.mean(transBef, trips, na.rm = TRUE),
            durAft = weighted.mean(durAft, trips, na.rm = TRUE),
            walkAft = weighted.mean(walkAft, trips, na.rm = TRUE),
            tranAft = weighted.mean(tranAft, trips, na.rm = TRUE),
            waitAft = weighted.mean(waitAft, trips, na.rm = TRUE),
            transAft = weighted.mean(transAft, trips, na.rm = TRUE))

trips_final %>% summarize(durBef = weighted.mean(durBef, trips, na.rm = TRUE),
            walkBef = weighted.mean(walkBef, trips, na.rm = TRUE),
            tranBef = weighted.mean(tranBef, trips, na.rm = TRUE),
            waitBef = weighted.mean(waitBef, trips, na.rm = TRUE),
            transBef = weighted.mean(transBef, trips, na.rm = TRUE),
            durAft = weighted.mean(durAft, trips, na.rm = TRUE),
            walkAft = weighted.mean(walkAft, trips, na.rm = TRUE),
            tranAft = weighted.mean(tranAft, trips, na.rm = TRUE),
            waitAft = weighted.mean(waitAft, trips, na.rm = TRUE),
            transAft = weighted.mean(transAft, trips, na.rm = TRUE))

# This was not really fruitful
# It's figures of origins and destinations
poc_toPlot <- 
    pocslice %>%
    group_by(origin) %>%
    summarize(trips = sum(trips), type_ = "origins")

poc_toPlot <- left_join(harris_acs, poc_toPlot, by = c("GEOID" = "origin"))
poc_toPlot$race <- "people of color"

white_toPlot <- 
    whiteslice %>%
    group_by(origin) %>%
    summarize(trips = sum(trips), type_ = "origins")

white_toPlot <- left_join(harris_acs, white_toPlot, by = c("GEOID" = "origin"))
white_toPlot$race <- "white"

raceToPlot <- rbind(poc_toPlot, white_toPlot)


ggplot() + geom_sf(data = raceToPlot, aes(col = trips, fill = trips)) + 
  facet_wrap(~race) + 
  scale_color_viridis() + scale_fill_viridis(name = "total commute trip origins") + 
  guides(color = FALSE) + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        strip.background = element_rect(colour = "black", fill = "grey75"),
        strip.text.x = element_text(color = "black", face = "bold", size = 12),
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank(),
        legend.position = "bottom") 

ggsave("output/CommuteOrigins.png", width = 8, height = 5)

race_toPlot <- left_join(harris_acs, race_toPlot, by = c("GEOID" = "destination"))


ggplot() + geom_sf(data = race_toPlot, aes(col = trips, fill = trips)) + 
  facet_wrap(~type_) + 
  scale_color_viridis() + scale_fill_viridis()


ggplot() + geom_sf(data = harris_acs, aes(fill = total))


# Combine accessibility measures with revealed behavior


dbDisconnect(con, shutdown = TRUE)
