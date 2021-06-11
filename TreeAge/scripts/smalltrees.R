library(tidyverse)

setwd("./TreeAge/raw_data/size_data")

file_list <- list.files()

all_data <- data.frame()

# join together all the data

for (i in 1:length(file_list)){
  filename = paste("./", file_list[i], sep = "")
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
  tree_data <- all_data %>% filter(id == all_data$id[tree])
  year_string <- c()
  for (survey in 1:length(tree_data$browsed)){
    year_string <- paste(year_string, tree_data$browsed[survey], sep = ", ")
  }
  x <- unlist(str_split(year_string, ", "))
  y <- paste(unique(x[x != "" & x != "NA"]), collapse = " ")
  years_browsed[tree] <- y
  id_browsed[tree] <- all_data$id[tree]
}

browsing_time <- unique(cbind(years_browsed, id_browsed))
browsing_time <- as.data.frame(browsing_time) %>% rename(id = id_browsed) %>% 
  mutate(id = as.factor(id), years_browsed = na_if(years_browsed, ""))

# read in tree info to add browsing observations

setwd("../../../")

tree_info <- read_csv("./TreeAge/raw_data/tree_info/tree_id.csv") %>% mutate(id = as.factor(id)) %>% 
  left_join(browsing_time)

# and remove from age data any trees that don't have an established position/identifier

id_valid <- as.numeric(tree_info$id)

valid_data <- data.frame()

for (row in 1:length(all_data$stand)){
  if (all_data$id[row] %in% id_valid){
    valid_data <- rbind(valid_data, all_data[row,])
  }
}

write_csv(valid_data, "./TreeAge/clean_data/SEADYN_saplings.csv")
