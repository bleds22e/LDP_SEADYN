## Small tree ages, DBH, height
## AVH: June 2021

# The following code assembles all manually input sapling data for Hondo stands 1-8
# collected annually from ~1982-1985.

library(tidyverse)

# read in all individual data files
file_list <- list.files("./HONDO/TreeAge/raw_data/size_data")

all_data <- data.frame()

# join together the data therein

for (i in 1:length(file_list)){
  filename = paste("/HONDO/TreeAge/raw_data/size_data/", file_list[i], sep = "")
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
  mutate(id = as.factor(id), years_browsed = na_if(years_browsed, "")) # if tree was not browsed, put in explicit NA for this column

# read in tree info to add browsing observations

tree_info <- read_csv("./HONDO/TreeAge/raw_data/tree_info/tree_id.csv") %>% mutate(id = as.factor(id)) %>% 
  left_join(browsing_time)

# and remove from age data any trees that don't have an established position/identifier

id_valid <- as.numeric(tree_info$id)

valid_data <- data.frame()

for (row in 1:length(all_data$stand)){
  if (all_data$id[row] %in% id_valid){
    valid_data <- rbind(valid_data, all_data[row,])
  }
}

#write_csv(valid_data, "./HONDO/TreeAge/clean_data/SEADYN_saplings.csv")
