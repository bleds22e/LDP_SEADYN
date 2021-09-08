### STEM DATA FOR AOS TREE DYNAMICS ###
## AVH June 2021 ##

con <- file("./AOS/TreeDynamics/raw_data/aos.stemraw.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con) # read in the single stem data file line by line

# create a function to remove all whitespace and collapse string (str_squish isn't working for some reason)
replace_ws <- function(i){
  str_replace_all(i, pattern = " ", replacement = "")
}

# get rid of whitespace to make single string with all information
results_list2 <- lapply(results_list, str_trim, side = "right")  #trim whitespace


# create blank dataframe to fill in
stem_data <- as.data.frame(matrix(ncol = 8))
col_names <- c("stand","quadrat","tree_tag","tree_sp","dbh_units","dead", "height_m",
               "age")
colnames(stem_data) <- col_names


for (row in 1:length(results_list2)){ # for each row of original text file
  empty_row <- c() # start an empty row to hold data
  data_string <- as.character(results_list2[[row]]) # read in line
  char.rem = nchar(data_string) # figure out how long the string is
  stand <- substr(data_string, 1, 2) # subset first position to get stand number
  empty_row[1] <- as.numeric(stand)
  data_string <- substr(data_string, 3, char.rem) # and trim this off data string
  data_string <- replace_ws(data_string)
  
  char.rem = nchar(data_string)
  quadrat <- substr(data_string, 1, 2) # scrub 2-character quadrat code 
  empty_row[2] <- toupper(quadrat)
  data_string <- substr(data_string, 3, char.rem) # remove this quadrat code from string
  
  char.rem = nchar(data_string)
  for (char in 1:char.rem){ # now figure out the species and what position this is in
    if (substr(data_string, char, char) %in% c("j","s","w","a")){
      trim.pos = char - 1
      sp.pos = char
    }
  }
  tree_tag <- as.numeric(substr(data_string, 1, trim.pos)) # trim the tree tag
  tree_sp <- as.character(substr(data_string, sp.pos, sp.pos)) # trim the species code
  
  empty_row[3] <- tree_tag
  empty_row[4] <- tree_sp
  
  trim.pos = sp.pos + 1
  
  data_string <- substr(data_string, trim.pos, char.rem) # trim the data string
  
  char.rem = nchar(data_string)
  
  dbh_units <- substr(data_string, 1, 2) # get the dbh (always 2 digits)
  empty_row[5] <- dbh_units
  data_string <- substr(data_string, 3, char.rem) # trim data string
  
  dead <- if_else(grepl("[d]", substr(data_string, 1, 1)), "d", "l")  # if d is present, tree is dead, otherwise it is live
  empty_row[6] <- dead
  data_string <- sub(pattern = dead, "", data_string) # trim data string
  
  if (nchar(data_string) > 0){ # if there is stil data remaining, it is for both the height and age
    char.rem = nchar(data_string)
    trim.position = char.rem - 2 # age is always the last two digits, so extract that
    height_m <- substr(data_string, 1, trim.position) # and whatever is before this position is the height
    age <- substr(data_string, (trim.position + 1), char.rem)
  }
  if (nchar(data_string) == 0) { # if string is empty, data is NA
    height_m <- NA
    age <- NA
  }
  empty_row[7] <- height_m
  empty_row[8] <- age
  stem_data <- rbind(stem_data, empty_row) # bind assembled row to full data
}

stem_data2 <- stem_data[which(stem_data$stand != ""),] # get rid of first row (blank)

stem_data3 <- stem_data2 %>% 
  mutate_at(c(5, 7, 8), as.numeric) %>% 
  mutate_at(c(1:4, 6), as.factor) %>% 
  mutate(dbh_m = (dbh_units*10)*(2.54)/100) %>% # convert from 0.1 inch units to m
  select(-dbh_units)

# create species code that is consistent with other data
sp_codes <- c("POTR", "PIGL", "PIMA", "PIBA")
tree_sp <- c("a", "w", "s", "j")
sp_conversion <- as.data.frame(cbind(sp_codes, tree_sp))

stem_data4 <- stem_data3 %>% full_join(sp_conversion) %>% select(-tree_sp)

#convert numeric codes to two-letter conventional code for stand

stand_info <- read_csv("./AOS/StandInfo/AOS_StandInformation.csv") %>% 
  select(plot_number, stand_code) %>% rename(stand = plot_number)

stem_data5 <- stem_data4 %>% left_join(stand_info) %>% select(-stand) %>% 
  rename(stand = stand_code, species_code = sp_codes)

#write_csv(stem_data5, "./AOS/TreeDynamics/clean_data/AOS_TreeDynamics_1983.csv")

stems <- read_csv("./AOS/TreeDynamics/clean_data/AOS_TreeDynamics_1983.csv") %>% 
  mutate(DBH_1983_cm = dbh_m,
         tree_code_1983 = toupper(dead)) %>% 
  rename(
         quad = quadrat,
         stem_height_1983_m = height_m,
         age_1983 = age
         ) %>% 
  select(-dbh_m, -dead)

# check on stand numbers - are these really AOS 1-8, or do they encode other things?

levels(as.factor(stems$species_codes))
levels(as.factor(stems$tree_code_1983))

stems <- stems %>% relocate(stand, quad, tree_tag, species_code, 
                            tree_code_1983, DBH_1983_cm, stem_height_1983_m, age_1983)

write_csv(stems, "./AOS/TreeDynamics/clean_data/AOS_TreeDynamics_1983.csv")



