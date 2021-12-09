### Extracting 2010 sapling data and tidying for depositing
### AVH November 2021

library(tidyverse)
library(assertr)

data <- read_csv("./Hondo/Saplings/raw_data/2010_data/Hondo_Saplings_Stand7_2010.csv",
                 col_names = c("stand", "quad","tree_tag","species_code","base_coord_N_m",
                               "base_coord_S_m","base_coord_E_m","base_coord_W_m","DBH_cm",
                               "stem_height_m","tree_code","stem_lean_amt","stem_lean_direction", "comments"),
                 skip = 1, col_types = c("f","f","f", "n","n",
                                         "n","n","n","n","c","n",
                                         "c","c")) %>% 
  mutate_all(~na_if(., ".")) %>% 
  mutate_at(c("base_coord_N_m", "base_coord_W_m",
              "base_coord_S_m", "base_coord_E_m"), as.numeric) %>% 
  mutate(base_coord_S_m = if_else(is.na(base_coord_N_m), base_coord_S_m,
                                  5-base_coord_N_m),
         base_coord_W_m = if_else(is.na(base_coord_E_m), base_coord_W_m,
                                  5-base_coord_E_m)) %>% 
  select(-base_coord_N_m, -base_coord_E_m)

levels(as.factor(data$species_code))

# QC validation
glimpse(data)
data %>% verify(stand == 7) %>% verify(substr(quad, 1,1) %in% 0:9) %>% 
  verify(substr(quad,2,2) %in% c("A","B","C","D","E","F",
                                 "G","H","I","J")) %>% 
  verify(is.numeric(c(tree_tag, base_coord_S_m, base_coord_W_m))) %>% 
           verify(species_code %in% c("PIMA","LALA")) %>% 
  verify(base_coord_S_m > 0 & base_coord_S_m < 5) %>% 
  verify(base_coord_W_m > 0 & base_coord_W_m < 5) %>% 
  verify(DBH_cm > 0) %>% verify(stem_height_m > 0) %>% 
  verify(tree_code %in% c("LL","LD","DD")) %>% 
  verify(stem_lean_amt > 0 & stem_lean_amt < 90) %>% 
  verify(stem_lean_direction %in% c("N","S","E","W","NW","SW","NE","SE"))

## confusion: are these saplings (DBH was measured, which seems like not saplings, but data are titled saplings...)

         