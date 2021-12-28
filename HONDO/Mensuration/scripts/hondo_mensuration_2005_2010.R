### Extracting 2005 and 2010 tree data from messy dataframes
### AVH September 2021

# read in packages
pkgs <- c("dplyr","stringr","assertr","skimr","readr")

lapply(pkgs, library, character.only = T)

## First, read in the relevant files

file.list <- list.files("./Hondo/Mensuration/2005_2010_raw/")

## Loop will trim away the unnecessary variables,
## rename columns,
## add stand information

# Create small df to convert between the shorthand species codes and unified taxonomic codes
tree_codes <- cbind(c("A","B","F","J","L","P","S","W"),
                    c("POTR","BEPA","ABBA","PIBA","LALA","POTR","PIMA","PIGL"))
colnames(tree_codes) <- c("species", "species_code")
tree_codes <- as.data.frame(tree_codes)

# for each file
for (file in 1:length(file.list)){
  file.name <- paste("./Hondo/Mensuration/2005_2010_raw/", file.list[file],
                     sep = "") # read in the file, select only columns with data and remove weird header information
  raw_file <- read_csv(file.name, col_names = F) %>% select(1:4, 38:49) %>% slice(-c(1:4))
  rename_file <- raw_file %>% rename(stand = 1, quad = 2, tree_tag = 3,
                                     species = 4, DBH_2005_cm = 5, height_2005_m = 6,
                                     code_2005 = 7, lean_amount_2005 = 8,
                                     lean_direction_2005 = 9, comments_2005 = 10,
                                     DBH_2010_cm = 11, height_2010_m = 12,
                                     code_2010 = 13, lean_amount_2010 = 14,
                                     lean_direction_2010 = 15, comments_2010 = 16) %>% slice(-1)
  # rename all the columns with consistent terminology
  tidy_file <- rename_file %>% left_join(tree_codes) %>% select(-species) 
  is.na(tidy_file) <- tidy_file == "." # replace periods with NA
  if (file == 1){
    full_file <- tidy_file
  }
  if (file > 1){
    full_file <- full_file %>% full_join(tidy_file) # join all the read-in files together
  }
}

tidy_file2 <- full_file %>% 
  mutate_at(c(1,2,3,6,8,12,14,16), as.factor) %>% mutate_at(c(4,5,7,10,11,13), as.numeric)

tidy_file3 <- tidy_file2 %>% mutate(comments_2005 = tolower(str_replace_all(comments_2005, "_", " ")),
                                    comments_2010 = str_replace_all(comments_2010, "_", " "))

tidy_file3 <- as_tibble(tidy_file3)

## Then, join these objects into one dataframe to join with -2001 data

treedyn_1980_2001 <- read_csv("./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2001.csv") %>% 
  mutate(stand = as.factor(stand), tree_tag = as.factor(tree_tag))

treedyn_1980_2010 <- treedyn_1980_2001 %>% full_join(tidy_file3)

# need to qc, but all joined together for now.

# QC data

# 1) tree species should be in the list of valid tree species codes
# 2) quadrats can only have certain specified values as well (1st character 0-9, 2nd letter A-J)
# 3) stand must be in 1:8
# 4) tree tag must be numeric
# 5) DBH and height for 2005/2010 must be greater than zero
# 6) Lean amount must be 0-90
# 7) Lean direction must be a compass direction in c(N, S, E, W, NE, NW, SE, SW)
# 8) Codes must be in list already described in metadata


treedyn_1980_2010 %>% assert(in_set(1:8), stand) %>% 
  mutate(quad_number = as.numeric(substr(quad,1,1)), quad_letter = substr(quad,2,2)) %>% 
  assert(in_set(0:9), quad_number) %>% 
  assert(in_set(c("A","B","C","D","E","F","G","H","I","J")), quad_letter) %>% 
  assert(in_set(c("PIBA","PIGL", "POTR","PIMA","LALA", "BEPA","ABBA")), species_code) %>% 
  verify(DBH_2005_cm > 0 | is.na(DBH_2005_cm)) %>% 
  verify(DBH_2010_cm > 0 | is.na(DBH_2010_cm)) %>% 
  verify(height_2005_m > 0 | is.na(height_2005_m)) %>% 
  verify(height_2010_m > 0 | is.na(height_2010_m)) %>% 
  verify(lean_amount_2005 >= 0 & lean_amount_2005 < 90 | is.na(lean_amount_2005)) %>% 
  verify(lean_amount_2010 >= 0 & lean_amount_2010 < 90 | is.na(lean_amount_2010)) %>% 
  assert(in_set(c("N","NE","E","SE","S","SW","W","NW")), c(lean_direction_2005,lean_direction_2010)) %>% 
  assert(in_set(c("DD","DL","DS","LB","LL","DB1","DB2", "LB_LL")), c(code_2005, code_2010))
  
skim(treedyn_1980_2010 %>% select(DBH_2005_cm,DBH_2010_cm,height_2005_m,height_2010_m))
# no outliers, looks good.

qc_treedyn <- treedyn_1980_2010 %>% rename(tree_code_2005 = code_2005,
                                           tree_code_2010 = code_2010,
                                           comments_2000 = comments) %>% 
  mutate(comments_2000 = str_replace_all(comments_2000, "_", " "))

# save QCed file

#write_csv(qc_treedyn, "./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2010.csv")
