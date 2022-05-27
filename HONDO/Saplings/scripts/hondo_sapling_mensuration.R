## Small tree ages, DBH, height
## AVH: June 2021

# The following code assembles all manually input sapling data for Hondo stands 1-8
# collected annually from ~1982-1985.

library(tidyverse)

# read in all individual data files
file_list <- list.files("./Hondo/Saplings/raw_data/size_data")

all_data <- data.frame()

# join together the data therein

for (i in 1:length(file_list)){
  filename = paste("./Hondo/Saplings/raw_data/size_data/", file_list[i], sep = "")
  df <- read_csv(filename) %>% mutate(browsed = as.character(browsed))
  if (i == 1){
    all_data <- df
  }
  else {
    all_data <- all_data %>% full_join(df)
  }
}

# need to extract all the years browsed for each tree and move this info to the tree_id file

years_browsed <- vector()
id_browsed <- vector()

for (tree in 1:length(all_data$id)){
  tree_data <- all_data %>% filter(id == all_data$id[tree]) # extract all annual surveys for an individual tree
  year_string <- c()
  for (survey in 1:length(tree_data$browsed)){
    year_string <- paste(year_string, tree_data$browsed[survey], sep = ", ") # paste together all information about years browsed
  }
  x <- unlist(str_split(year_string, ", ")) # unlist the string into a vector
  y <- paste(unique(x[x != "" & x != "NA"]), collapse = " ") # extract unique years browsed, and collapse this into a spaced string
  years_browsed[tree] <- y
  id_browsed[tree] <- all_data$id[tree]
}

# now, bind these years browsed and associated tree id into a dataframe that can be joined with the
# tree id position/substratum information

browsing_time <- unique(cbind(years_browsed, id_browsed))

browsing_time <- as.data.frame(browsing_time) %>% rename(id = id_browsed) %>% 
  mutate(id = as.factor(id), years_browsed = na_if(years_browsed, "")) %>% 
  rename(tree_tag = id)# if tree was not browsed, put in explicit NA for this column

# read in tree info to add browsing observations

tree_info <- read_csv("./Hondo/Saplings/raw_data/tree_info/Hondo_SaplingID_1983_1985.csv") %>% mutate(tree_tag = as.factor(tree_tag)) %>% 
  left_join(browsing_time)

# and remove from age data any trees that don't have an established position/identifier

id_valid <- as.numeric(tree_info$tree_tag)

valid_data <- data.frame()

for (row in 1:length(all_data$stand)){
  if (all_data$id[row] %in% id_valid){
    valid_data <- rbind(valid_data, all_data[row,])
  }
}

valid_data2 <- valid_data %>% select(-browsed, -notes) %>% rename(basal_diameter_mm = diameter_mm, tree_tag = id)

saps_1983 <- valid_data2 %>% filter(year == 1983)
saps_1983$tree_tag[which(duplicated(saps_1983$tree_tag))]
saps_1983$stand[which(duplicated(saps_1983$tree_tag))]

saps_1984 <- valid_data2 %>% filter(year == 1984)
saps_1984$tree_tag[which(duplicated(saps_1984$tree_tag))]
saps_1984$stand[which(duplicated(saps_1984$tree_tag))]

saps_1985 <- valid_data2 %>% filter(year == 1985)
saps_1985$tree_tag[which(duplicated(saps_1985$tree_tag))]
saps_1985$stand[which(duplicated(saps_1985$tree_tag))]

# double checked duplicates, and corrected a couple of mistakes (1985 data for stand 6 hidden in 1984 dataset)
# duplicate tree tags remaining are real, or else an artifact of the recorder (not me!)

# now to QC the data

library(assertr)
saplings <- valid_data2 %>% unite("date",c(year,month,day), sep = "-", remove = F) %>% 
  mutate(date = ymd(date)) %>% 
  select(-day) %>% relocate(year, .before = month) %>% relocate(date, .after = month)

saplings %>% assert(within_bounds(1,8), stand) %>% assert(within_bounds(1,12), month) %>% 
  assert(within_bounds(1980,1985), year) %>% verify(tree_tag > 0) %>% verify(age > 0 | is.na(age)) %>% 
  verify(height_cm > 0 | is.na(height_cm)) %>% verify(basal_diameter_mm > 0 | is.na(basal_diameter_mm)) %>% 
  verify(basal_diameter_mm < 80 | is.na(basal_diameter_mm)) # roughly less than 3 inches DBH
# one exception, which seems off by a factor of 10 (id 634, year 1985)

#write_csv(saplings, "./Hondo/Saplings/clean_data/Hondo_SaplingMensuration_1983_1985.csv")

sap_info <- tree_info
summary(sap_info)
levels(as.factor(sap_info$substratum))
range(saplings$date, na.rm = T)
sap_info <- sap_info %>% rename(base_coord_S_m = location_south_m,
                                base_coord_W_m = location_west_m,
                                tree_tag = id,
                                species_code = species,
                                quad = quadrat)

#write_csv(sap_info, "./Hondo/Saplings/clean_data/tree_info/Hondo_SaplingID_1983_1985.csv")

# Figure showing location of saplings in stands

grid_theme <- theme_classic() +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    legend.position = "top",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 9),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = 7, face = "italic"),
    panel.spacing = unit(0.7, "lines"))

letter_to_m <- cbind(LETTERS[1:10], c(0:9)) %>% as.data.frame() %>% rename(row = 1, metres_WE = 2) %>% 
  mutate(metres_WE = as.numeric(metres_WE)*5)

sapling_measurements <- read_csv("./Hondo/Saplings/clean_data/Hondo_SaplingMensuration_1983_1985.csv") %>% 
  select(tree_tag, year) %>% unique()

sapling_locations <- read_csv("./Hondo/Saplings/clean_data/Hondo_SaplingID_1983_1985.csv") %>% 
  select(species_code, stand, quad, tree_tag, base_coord_S_m, base_coord_W_m) %>%
  left_join(sapling_measurements) %>% 
  separate(quad, into = c("col","row"), 1,1) %>% 
  left_join(letter_to_m) %>% 
  mutate(metres_SN = as.numeric(col)*5 + base_coord_S_m, metres_WE = metres_WE + base_coord_W_m) %>% 
  filter(is.na(year) == F) %>% 
  mutate(species = case_when(species_code == "ABBA" ~ "A. balsamea",
                             species_code == "BEPA" ~ "B. papyrifera",
                             species_code == "POTR" ~ "P. tremuloides",
                             species_code == "PIBA" ~ "P. banksiana",
                             species_code == "PIGL" ~ "P. glauca",
                             species_code == "PIMA" ~ "P. marina",
                             species_code == "LALA" ~ "L. laricina"))


sapling_time <- ggplot(aes(x = metres_WE, y = metres_SN, fill = species), data = sapling_locations) +
  geom_point(alpha = 0.8, pch = 21, stroke = 0.1, size = 1, col = "black") +
  facet_grid(rows = vars(year), cols = vars(stand)) +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       col = "Species") +
  grid_theme +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F) +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  guides(fill = guide_legend(override.aes = list(size=4)))
  
png("./sapling_positions.png", res = 600, width = 8, height = 5, units = "in")
sapling_time
dev.off()

