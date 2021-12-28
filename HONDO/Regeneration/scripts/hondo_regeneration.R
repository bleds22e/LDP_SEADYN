### Regeneration data
## Amelia Hesketh 
## November 2021

library(tidyverse)

# create a small df that converts shorthand tree species codes to unified four-letter taxonomic code

species <- c("PB","AW","BW","SW","SB","PJ","LT")
species_code <- c("POBA","POTR","BEPA","PIGL","PIMA","PIBA", "LALA")
sp_converter <- as.data.frame(cbind(species, species_code))

# read in regeneration data
regen <- read_csv("./Hondo/Regeneration/raw_data/Hondo_Regeneration_2010.csv", skip = 9) %>%
  rename(quad = plot) %>% left_join(sp_converter) %>% select(-species) %>%
  relocate(species_code, .after = quad)

# separate data into two dataframes: one for count data, the other for mensuration data
count_data <- regen %>% select(stand,quad,species_code, count)

# mensuration files are chunked into four small dfs: two for the two tallest trees, two for two representative trees 
mens_tall <- regen %>% select(stand, quad, species_code, dbh1,
                                  ht1, rcd1) %>% 
  rename(DBH_cm = dbh1, stem_height_m = ht1, RCD_cm = rcd1) %>% # rename columns
  mutate(tree_type = "T")

mens_tall_2 <- regen %>% select(stand, quad, species_code, dbh2,
                                ht2, rcd2) %>% 
  rename(DBH_cm = dbh2, stem_height_m = ht2, RCD_cm = rcd2) %>% 
  mutate(tree_type = "T")

mens_rep <- regen %>% select(stand, quad, species_code, avgdbh1, 
                                  avght1, avgrcd1) %>% 
  rename(DBH_cm = avgdbh1, stem_height_m = avght1, RCD_cm = avgrcd1) %>% 
  mutate(tree_type = "R")

mens_rep2 <- regen %>% select(stand, quad, species_code, avgdbh2, 
                             avght2, avgrcd2) %>% 
  rename(DBH_cm = avgdbh2, stem_height_m = avght2, RCD_cm = avgrcd2) %>% 
  mutate(tree_type = "R")

# then these are bound together
regen_mens <- rbind(mens_rep, mens_rep2, mens_tall, mens_tall_2)

regen_mens <- regen_mens %>% 
  mutate_all(~na_if(., ".")) %>%  # replace decimals with NA
  filter(is.na(stem_height_m) == F) # only retain non-NA data

# add in implicit zero data for all species counted
regen_count <- count_data %>% 
  complete(species_code, nesting(stand, quad), fill = list(count = 0)) %>% 
  relocate(species_code, .after = quad)

# save data frames

write_csv(regen_count, "./Hondo/Regeneration/clean_data/Hondo_RegenerationCounts_2010.csv")
write_csv(regen_mens, "./Hondo/Regeneration/clean_data/Hondo_RegenerationMensuration_2010.csv")
