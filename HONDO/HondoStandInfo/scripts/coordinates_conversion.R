### Converting coordinates from degrees-minutes-seconds to decimal degrees
### Also, adding data inventory information

library(tidyverse)

stand_coords <- read.csv("./HONDO/HondoStandInfo/raw_data/Hondo_Stand_Coordinates.csv",
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
  mutate(lat = lat_d + lat_m/60 + lat_s/3600,
            long= -1*(long_d + long_m/60 + long_s/3600), .keep = "unused") %>% 
  # this comments column doesn't include any useful information, so remove that
  select(-comments)

#write_csv(to_degrees, "./HONDO/HondoStandInfo/clean_data/SEADYN_Hondo_StandCoords.csv")

means_only <- to_degrees %>% filter(corner == "Means") %>% select(-corner) %>% mutate(bryoid_cover = rep(1, times = 8),
                                                                                      vascular_cover = rep(1, times = 8),
                                                                                      litter = c(1,1,1,0,0,0,0,0),
                                                                                      soil = c(1,1,1,0,0,0,0,0),
                                                                                      saplings = c(1,1,1,1,1,1,1,0),
                                                                                      densiometer = rep(1, times = 8),
                                                                                      tree_dynamics = rep(1, times = 8),
                                                                                      dates_burned = c(NA, NA, 2001, 2001, NA, 2001, NA, 2001))
#write_csv(means_only, "./HONDO/HondoStandInfo/clean_data/SEADYN_Hondo_StandInfo.csv")
