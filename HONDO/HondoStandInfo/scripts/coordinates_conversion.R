### converting coordinates from degrees-minutes-seconds to decimal degrees

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
  mutate(lat_dec = lat_d + lat_m/60 + lat_s/3600,
            long_dec= -1*(long_d + long_m/60 + long_s/3600), .keep = "unused") %>% 
  # this comments column doesn't include any useful information, so remove that
  select(-comments)

write.csv(to_degrees, "./HONDO/HondoStandInfo/clean_data/Hondo_Stand_Coordinates.csv")
