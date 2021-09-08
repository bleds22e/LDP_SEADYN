### Converting coordinates from degrees-minutes-seconds to decimal degrees
### Also, adding data inventory information

library(tidyverse)

stand_coords <- read.csv("./Hondo/StandInfo/raw_data/Hondo_Stand_Coordinates.csv",
                         fileEncoding = "macintosh", # need to specify encoding to read in degree symbol
                         col.names = c("stand","corner","latitude_N","longitude_W","elevation_ft","comments"))

to_degrees <- stand_coords %>% 
  # remove N/W designation
  mutate(latitude_N = str_remove_all(latitude_N, "N"),
         longitude_W = str_remove_all(longitude_W, "W")) %>% 
  # separate into coordinate components
  separate(latitude_N, into = paste("lat", c("d", "m", "s"), sep = "_"), sep = "[°.]") %>% 
  separate(longitude_W, into = paste("long", c("d", "m", "s"), sep = "_"), sep = "[°.]") %>% 
  # convert from character to numeric data
  mutate_at(3:8, as.numeric) %>% 
  # calculate the decimal degree coordinates from the incorrect format
  mutate(latitude_degrees = lat_d + lat_m/60 + lat_s/3600,
            longitude_degrees = -1*(long_d + long_m/60 + long_s/3600), .keep = "unused") %>% 
  # this comments column doesn't include any useful information, so remove that
  select(-comments) %>% 
  mutate(elevation_m = elevation_ft*0.3048) %>% select(-elevation_ft) %>% 
  rename(stand_code = stand)
  
#write_csv(to_degrees, "./Hondo/StandInfo/clean_data/Hondo_StandLocation.csv")

means_only <- to_degrees %>% filter(corner == "Means") %>% select(-corner) %>% mutate(vascular_cover = rep(1, times = 8),
                                                                                      bryoid_cover = rep(1, times = 8),
                                                                                      tree_dynamics = rep(1, times = 8),
                                                                                      saplings = c(1,1,1,1,1,1,1,0),
                                                                                      dendrochronology = c(1,1,1,0,0,0,0,0),
                                                                                      litter = c(1,1,1,0,0,0,0,0),
                                                                                      probe_temp = rep(1, times = 8),
                                                                                      soil_conditions = c(1,1,1,0,0,0,0,0),
                                                                                      tree_cover = rep(1, times = 8),
                                                                                      dates_burned = c(NA, NA, 2001, 2001, NA, 2001, NA, 2001))
#write_csv(means_only, "./Hondo/StandInfo/clean_data/Hondo_StandInformation1.csv")

#QC contour data... 

qc_contours <- read_csv("./Hondo/StandInfo/clean_data/Hondo_StandContours.csv")
qc_contours2 <- qc_contours %>% 
  mutate(contour_value = contour_value - 10) %>% 
  rename(relative_elevation_m = contour_value,
         WE_marker = east_west,
         SN_marker = north_south)
summary(qc_contours2)

levels(as.factor(qc_contours2$SN_marker))

write_csv(qc_contours2, "./Hondo/StandInfo/clean_data/Hondo_StandContours.csv" )

