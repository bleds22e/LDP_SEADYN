# Read in weird tree files
# EKB; Dec 21, 2020

### LIBRARIES ###
library(tidyverse)
library(janitor)

### NOTES ###

# Right now, this script does the following:
# 1. Reads in a text file. We lose the metadata located in the file name.
# 2. Pulls out the first line of the file and saves it as metadata.
# 3. Removes extra white-space and makes each list the same length (26 here)
# 4. Binds the lists together to be rows in one dataframe, names the first row
#     into column names, drops empty rows and any rows that repeat the column names.
# 5. Makes a list that contains both the cleaned dataframe and the metadata

# Ideally:
# 1. We either convert to text files in R or figure out how to do this with 
#    whatever files they currently are.
# 2. Fix some of the hard-coding.
# 3. Retain the metadata from the file names.
# 4. Maybe we want to restructure the files once we've read them all in?
#     a. read in a file and save the metadata from in (site, stand, year, month)
#     b. pull out metadata row
#     c. make the cross-tab data as above
#     d. save all above in 3 big lists then rotate through and make 1 giant df?
#         - columns for site, stand, year, month, quad, species, value, temp?, 
#           and a notes column for the metadata row 
#         - or keep some of the cross-tab part? or one df for each stand? 

### READ IN FILE ###
## this attempt using a txt file

# open a connection to the file we want to read in
con <- file('./HONDO/VascularPlantSurveys/raw_data/Stand1/HONDO181.AUG.txt') 
open(con)

# make a list to put the results into
results.list <- list()

# start with the first line in the file and cycle through all of them
current.line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results.list[[current.line]] <- line
  current.line <- current.line + 1
} 

# close the connection to the file
close(con)

### TURN INTO DATAFRAME ###

# remove remaining white spaces
results.list <- lapply(results.list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# save first list as metadata to be attached later
# are all the first rows metadata?
metadata <- results.list[[1]]
  
# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results.list[2:length(results.list)], 
                     unlist(str_split), 
                     pattern = " ")
  
# in order to bind the list together as rows, they need to be the same length.
# it would probably be better to not hard-code the 26 and allow flexiblity
# for files that might have different row lengths
for (i in 1:length(split_list)){
    
  # for lists that start with "QUAD," remove repeats and only take
  # the first 26 objects in the list
  # should fix to remove any objects with lowercase letters specifically
  if (split_list[[i]][[1]][1] == 'QUAD'){
     split_list[[i]] = split_list[[i]][[1]][1:26]
      
  # for lists that are empty, put 26 NAs in them  
  } else if (split_list[[i]][[1]][1] == "") {
    split_list[[i]][[1]][1:26] <- NA
  }
    
}
  
# stitch lists together to act as rows in a dataframe
data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T)) %>% 
  # remove the empty rows
  drop_na() %>% 
  # make the first row ("QUAD") into the column names
  janitor::row_to_names(., row_number = 1) %>% 
  # remove any remaining "QUAD" rows
  filter(QUAD != "QUAD")

# make a list to keep metadata together
data_and_metadata <- list(data, metadata)

