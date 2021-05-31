# Reading vascular data
# EKB; January 2021

### LIBRARIES ###

#install.packages("groundhog")
library(groundhog)
library(tidyverse)
library(janitor)
groundhog.library(tidyverse, "2021-01-01")
groundhog.library(janitor, "2021-01-01")
source("functions.R") # read in specific functions

### FILES ###

stands <- c("Stand1", "Stand2", "Stand3", 
            "Stand4", "Stand5", "Stand6", "Stand7", "Stand8")
list_names <- c("Stand_1", "Stand_2", "Stand_3", "Stand_4", 
                "Stand_5", "Stand_6", "Stand_7", "Stand_8")
df_list <- sapply(list_names, function(x) NULL)

for (s in 1:length(stands)) {
  
  ## PREP DATA ##
  
  # make df with .txt file paths
  file_path <- paste0("./VascularPlantSurveys/Hondo_raw/", stands[s])
  myfiles <- as.list(list.files(file_path, full.names = TRUE, pattern = "*.txt"))
  
  # make df with metadata pulled from file names (see 'functions.R' script)
  metadata <- filename_to_metadata(file_path)
  
  ## PROCESS FILES ##
  
  # empty list for processed dataframes
  cleaned_list <- list()
  
  for (f in 1:length(myfiles)) {
    
    # read in file, clean, and turn into long df
    f2 <- myfiles[[f]]          # pull out one file at a time
    f3 <- read_in_txt_file(f2)  # see 'functions.R' script
    f4 <- txt_file_to_df(f3)    # see 'functions.R' script
    
    # add metadata from file name
    f4$Month <- metadata$month[f]
    f4$Year <- metadata$year[f]
    f4$Stand <- metadata$stand[f]
    
    # add the dataframe to a list
    cleaned_list[[f]] <- f4
    
  }
  
  # make one big dataframe from all the files by binding all the rows together
  long_data <- do.call(rbind, cleaned_list) 
  
  # pull out temp data and put in its own df
  temp_data <- long_data %>% 
    mutate(Cover = as.numeric(Cover)) %>% 
    filter(Species == 'TEMP', Cover > 0) %>%  # if temp is 0, means no data
    rename("Temp_F" = "Cover") %>% 
    select(-Species)
  
  # make species data wide
  cover_data <- long_data %>% 
    mutate(Cover = as.numeric(Cover)) %>% 
    filter(Species != 'TEMP',
           # some df have extra rows of 0.0, so remove those here
           Species != 0.0,
           Species != '0.0')
  
  # deal with any stand-specific issues causing duplicate rows
  if (s %in% c(5, 8)) {
    
    # in stands 5 and 8, the data from Aug 2015 has at least one species row 
    # repeated; the repeat row has cover values that are all 0 while the correct
    # row has some values which are above 0
    
    # remove any row duplicated in the Year, Month, Species, and Quad columns
    cover_data_clean <- cover_data %>% 
      group_by(Year, Month, Species, Quad) %>% 
      mutate(dups = n()>1) %>% 
      filter(dups != TRUE) %>% 
      select(-dups)
    # sum the values of the duplicate rows (either 0+0 or 0+correct_value)
    stand_dups <- cover_data %>% 
      group_by(Stand, Year, Month, Species, Quad) %>% 
      mutate(dups = n()>1) %>% 
      filter(dups == TRUE) %>% 
      summarise(Cover = sum(Cover)) %>% 
      select(Species, Quad, Cover, Month, Year, Stand)
    # bind the df without any duplicates and with summed duplicates together
    cover_data <- bind_rows(cover_data_clean, stand_dups)
    
  } 
  
  # make the data into crosstab with Species as the columns
  cover_data <- cover_data %>% 
    select(Month, Year, Quad, Stand, Species, Cover) %>% 
    pivot_wider(id_cols = c("Month", "Year", "Stand", "Quad", "Species"),
                names_from = Species, 
                values_from = Cover, 
                values_fill = NA)
  
  # make list for export
  df_list[[s]] <- list(cover_data, temp_data)
  
}

# give a descriptive name (stand plus cover/temp) to each df
for (l in 1:length(df_list)){
  
  stand_names <- names(df_list)
  stand <- stand_names[l]
  names(df_list[[l]]) <- c(paste(stand, "Cover", sep = "_"),
                        paste(stand, "Temp", sep = "_"))
  
}

# write each df to a csv file with a descriptive name
#for (l in 1:length(df_list)) {
  
  #stand <- df_list[[l]]
  #df_names <- names(stand)
  
  #for (d in 1:length(stand)){
   # write_csv(stand[[d]],
              #paste("./VascularPlantSurveys/HONDO_compiled/", df_names[d], ".csv", sep = ""))
 # }
  
#}  
