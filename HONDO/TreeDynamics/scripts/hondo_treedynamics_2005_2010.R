### Extracting 2005 and 2010 tree data from messy dataframes
### AVH September 2021

library(tidyverse)

## First, read in the relevant files

file.list <- list.files("./Hondo/TreeDynamics/2005_2010_raw/")

## Loop will trim away the unnecessary variables,
## rename columns,
## add stand information
tree_codes <- cbind(c("A","B","F","J","L","P","S","W"),
                    c("POTR","BEPA","ABBA","PIBA","LALA","POTR","PIMA","PIGL"))
colnames(tree_codes) <- c("species", "species_code")
tree_codes <- as.data.frame(tree_codes)


for (file in 1:length(file.list)){
  file.name <- paste("./Hondo/TreeDynamics/2005_2010_raw/", file.list[file],
                     sep = "")
  raw_file <- read_csv(file.name, col_names = F) %>% select(1:4, 38:49) %>% slice(-c(1:4))
  rename_file <- raw_file %>% rename(stand = 1, quad = 2, tree_tag = 3,
                                     species = 4, DBH_2005_cm = 5, height_2005_m = 6,
                                     code_2005 = 7, lean_amount_2005_degrees = 8,
                                     lean_direction_2005 = 9, comments_2005 = 10,
                                     DBH_2010_cm = 11, height_2010_m = 12,
                                     code_2010 = 13, lean_amount_2010_degrees = 14,
                                     lean_direction_2010 = 15, comments_2010 = 16) %>% slice(-1)
  tidy_file <- rename_file %>% left_join(tree_codes) %>% select(-species) 
  is.na(tidy_file) <- tidy_file == "."
  if (file == 1){
    full_file <- tidy_file
  }
  if (file > 1){
    full_file <- full_file %>% full_join(tidy_file)
  }
}

tidy_file2 <- full_file %>% 
  mutate_at(c(1,2,3,6,8,12,14,16), as.factor) %>% mutate_at(c(4,5,7,10,11,13), as.numeric)

tidy_file3 <- tidy_file2 %>% mutate(comments_2005 = tolower(str_replace_all(comments_2005, "_", " ")),
                                    comments_2010 = str_replace_all(comments_2010, "_", " "))

tidy_file3 <- as_tibble(tidy_file3)

## Then, join these objects into one dataframe to join with -2001 data

treedyn_1980_2001 <- read_csv("./Hondo/TreeDynamics/clean_data/Hondo_TreeDynamics_1980_2001.csv") %>% 
  mutate(stand = as.factor(stand), tree_tag = as.factor(tree_tag))

treedyn_1980_2010 <- treedyn_1980_2001 %>% full_join(tidy_file3)

# need to qc, but all joined together for now.

write_csv(treedyn_1980_2010, "./Hondo/TreeDynamics/clean_data/Hondo_TreeDynamics_1980_2010.csv")

