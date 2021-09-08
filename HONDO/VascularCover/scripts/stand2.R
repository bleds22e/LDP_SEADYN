# Prep Stand 2 Data for missing files
# January 2021

## What this script does:
# reads in the older versions of the stand 2 data that we don't have in "hondo" format (the s2YY [YY is year] files for 1981-1985), fixes the issues in them, and binds them all together. Note: The 1980 data was reentered from the field copy.

# LIBRARIES #
library(tidyverse)
library(janitor)

# READ IN THE FILE #

## 1981:
# open a connection to the file we want to read in
con <- file('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/s281.b.st25.txt') 
open(con)

# make a list to put the results into
results_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

# CLEAN FILE #

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

#save first 6 rows as metadata:
metadata1981 <- results_list[1:6]

#gsub finds the 0.01's and replaces them with 0 and .01. The next step will recognize that they're separate because of the space. It saves as a "value", but after the next line it ends up as "data" again.
results_list <- gsub(".01", " .01", results_list)

# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[7:length(results_list)], 
                     str_split, 
                     pattern = " ")

# in order to bind the list together as rows, they need to be the same length.
for (i in 1:length(split_list)){
  
  # get length of row
  # 31 is assuming only 1 sub-stand in ever surveyed twice
  # if 31 is not the maximum length, we'll need to edit this code
  max_length <- 26
  row_length <- length(split_list[[i]][[1]])
  
  # for lists that start with "QUAD," find length of "row" (actually a list)
  if (row_length > 1){
    
    # this code adds NAs to the row to match max_length
    if (row_length < max_length) {
      add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
        na_if("")
      split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
    }
    
    # for lists that are empty, put the appropriate number of NAs in, based on the
    # length of the corrected "QUAD"  list  
  } else if (row_length <= 1) {
    split_list[[i]][[1]][1:max_length] <- NA
    
  } else {
    print("ERROR: value greater than 26 possibly detected")
    # ideally, would want to print which file once this is written for multiple
  }
  
}

data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T))
#Row 43: something weird going on here (quad is 6c but probably should be 8c and one value is 510 - going to need to refer back to data sheets, I think) - this issue is fixed below

# Now to get 2nd rows to tack onto the first row
# Tidyverse: filter based on odd/even rows 

# starting at row 1, select evey second row (odds)
data_pt1 <- data %>% filter(row_number() %% 2 == 1)

# same for evens:
data_pt2 <- data %>% filter(row_number() %% 2 == 0)

# cbind to get everything together:
data1981 <- cbind(data_pt1, data_pt2)

# de-select the NA rows I added to make it a data frame:
data1981 <- data1981 %>% remove_empty(which = c("cols"))

# To add columns, will get rows from the corresponding file that has the column names:
con <- file('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/s281.v.sp25.txt') 
open(con)

# make a list to put the results into
column_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  column_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

#we need 13-18:
column <- column_list[13:18]

# remove remaining white spaces
column <- lapply(column, str_trim, side = "both") %>% 
  lapply(., str_squish)

# Seperating based on the spaces:
column <- lapply(column, 
                     str_split, 
                     pattern = " ")


# Collapse these lists into one big list
species <- unlist(column, recursive = T)

# deselect the first 2
species <- species[-c(1, 2)]

# add onto data1981
data1981 <- rbind(species, data1981) # it works!!

# make 1st row column names:
data1981 <- data1981 %>%
  row_to_names(row_number = 1)

# make column for month, and a year column:
data1981$month <- NA
data1981$year <- 1981
data1981$stand <- 2

# add months based on row numbers:
data1981[c(1:25), 45] = "5"
data1981[c(26:50), 45] = "6"
data1981[c(51:75), 45] = "7"
data1981[c(76:100), 45] = "8"
data1981[c(101:125), 45] = "9"
data1981[c(126:150), 45] = "10"

# The weird thing with row 23: It was just entered wrong, so fixing it based on the field copy data sheet:
data1981_clean <- data1981
data1981_clean[22, 1] = "8C" # quad name is 6c, should be 8c
data1981_clean[22, 5] = "5" #ALCR cover should be 5

#reorder:
data1981_clean <- data1981_clean %>% 
  select(stand, year, month, everything())

#######

## 1982
# open a connection to the file we want to read in
con <- file('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/s282.v.st25.txt') 
open(con)

# make a list to put the results into
results_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

# CLEAN FILE #

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

#save first 6 rows as metadata:
metadata1982 <- results_list[1:6]

#gsub finds the 0.01's and replaces them with 0 and .01. The next step will recognize that they're separate because of the space. It saves as a "value", but after the next line it ends up as "data" again.
results_list <- gsub(".01", " .01", results_list)

# there are two blank spaces for May 8E and 8D ([46] and [48]) where .01 was at the beginning of a row, removing them:
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[7:length(results_list)], 
                     str_split, 
                     pattern = " ") 

# in order to bind the list together as rows, they need to be the same length.
for (i in 1:length(split_list)){
  
  # get length of row
  # 31 is assuming only 1 sub-stand in ever surveyed twice
  # if 31 is not the maximum length, we'll need to edit this code
  max_length <- 26
  row_length <- length(split_list[[i]][[1]])
  
  # for lists that start with "QUAD," find length of "row" (actually a list)
  if (row_length > 1){
    
    # this code adds NAs to the row to match max_length
    if (row_length < max_length) {
      add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
        na_if("")
      split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
    }
    
    # for lists that are empty, put the appropriate number of NAs in, based on the
    # length of the corrected "QUAD"  list  
  } else if (row_length <= 1) {
    split_list[[i]][[1]][1:max_length] <- NA
    
  } else {
    print("ERROR: value greater than 26 possibly detected")
    # ideally, would want to print which file once this is written for multiple
  }
  
}

data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T))


# Now to get 2nd rows to tack onto the first row
# Tidyverse: filter based on odd/even rows 

# starting at row 1, select evey second row (odds)
data_pt1 <- data %>% filter(row_number() %% 2 == 1)

# same for evens:
data_pt2 <- data %>% filter(row_number() %% 2 == 0)

# cbind to get everything together:
data1982 <- cbind(data_pt1, data_pt2)

# de-select the NA rows I added to make it a data frame:
data1982 <- data1982[c(1:44)]

# add onto data1981
data1982 <- rbind(species, data1982) # it works!!

# make 1st row column names:
data1982 <- data1982 %>%
  row_to_names(row_number = 1)

# make column for month, and a year column:
data1982$month <- NA
data1982$year <- 1982
data1982$stand <- 2

# add months based on row numbers:
data1982[c(1:25), 45] = "5"
data1982[c(26:50), 45] = "6"
data1982[c(51:75), 45] = "7"
data1982[c(76:100), 45] = "8"
data1982[c(101:125), 45] = "9"
data1982[c(126:150), 45] = "10"

#reorder:
data1982_clean <- data1982 %>% 
  select(stand, year, month, everything())

###################################################################
## 1983

# open a connection to the file we want to read in
con <- file('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/s283.v.st25.txt') 
open(con)

# make a list to put the results into
results_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

# CLEAN FILE #

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# this one doesn't have metadata

#gsub finds the 0.01's and replaces them with 0 and .01. The next step will recognize that they're separate because of the space. It saves as a "value", but after the next line it ends up as "data" again.
results_list <- gsub(".01", " .01", results_list)

# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[1:length(results_list)], 
                     str_split, 
                     pattern = " ")

# in order to bind the list together as rows, they need to be the same length.
for (i in 1:length(split_list)){
  
  # get length of row
  # 31 is assuming only 1 sub-stand in ever surveyed twice
  # if 31 is not the maximum length, we'll need to edit this code
  max_length <- 26
  row_length <- length(split_list[[i]][[1]])
  
  # for lists that start with "QUAD," find length of "row" (actually a list)
  if (row_length > 1){
    
    # this code adds NAs to the row to match max_length
    if (row_length < max_length) {
      add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
        na_if("")
      split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
    }
    
    # for lists that are empty, put the appropriate number of NAs in, based on the
    # length of the corrected "QUAD"  list  
  } else if (row_length <= 1) {
    split_list[[i]][[1]][1:max_length] <- NA
    
  } else {
    print("ERROR: value greater than 27 possibly detected")
    # ideally, would want to print which file once this is written for multiple
  }
  
}

data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T))

# Now to get 2nd rows to tack onto the first row
# Tidyverse: filter based on odd/even rows 

# starting at row 1, select evey second row (odds)
data_pt1 <- data %>% filter(row_number() %% 2 == 1)

# same for evens:
data_pt2 <- data %>% filter(row_number() %% 2 == 0)

# cbind to get everything together:
data1983 <- cbind(data_pt1, data_pt2)

# de-select the NA rows I added to make it a data frame:
data1983 <- data1983[c(1:45)]

#get rid of the 2nd half quadrat names:
data1983 <- data1983[-(27)]

# add columns
data1983 <- rbind(species, data1983) # it works!!

# make 1st row column names:
data1983 <- data1983 %>%
  row_to_names(row_number = 1)

# make column for month, and a year column:
data1983$month <- NA
data1983$year <- 1983
data1983$stand <- 2

# add months based on row numbers:
data1983[c(1:25), 45] = "5"
data1983[c(26:50), 45] = "6"
data1983[c(51:75), 45] = "7"
data1983[c(76:100), 45] = "8"
data1983[c(101:125), 45] = "9"
data1983[c(126:150), 45] = "10"

## the plot no.'s have a number at the end - taking it off
quad <- select(data1981, (1))
data1983 <- cbind(data1983, quad)
data1983 <- data1983[-(1)]

#reorder:
data1983_clean <- data1983 %>% 
  select(stand, year, month,PLOT, everything())


#########################################
## 1984:
# open a connection to the file we want to read in
con <- file('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/s284.v.st25.txt') 
open(con)

# make a list to put the results into
results_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

# CLEAN FILE #

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# this one doesn't have metadata

#gsub finds the 0.01's and replaces them with 0 and .01. The next step will recognize that they're separate because of the space. It saves as a "value", but after the next line it ends up as "data" again.
results_list <- gsub(".01", " .01", results_list)

# for 1984, looks like they started doing quads 4 twice, and entering the mean value between them. So, let's deal with these:

## there's a big problem with these since the mean numbers have 3 digits: fixing them: 

results_list <- gsub(".06", " .06", results_list, fixed = TRUE)
results_list <- gsub("1.3", " 1.3", results_list, fixed = TRUE)
results_list <- gsub("1.5", " 1.5", results_list, fixed = TRUE)
results_list <- gsub("2.5", " 2.5", results_list, fixed = TRUE)
results_list <- gsub("4.5", " 4.5", results_list, fixed = TRUE)
results_list <- gsub("5.5", " 5.5", results_list, fixed = TRUE)
results_list <- gsub(".55", " .55", results_list, fixed = TRUE)
results_list <- gsub(".75", " .75", results_list, fixed = TRUE)
results_list <- gsub("1.2", " 1.2", results_list, fixed = TRUE)
results_list <- gsub("6.5", " 6.5", results_list, fixed = TRUE)
results_list <- gsub("3.5", " 3.5", results_list, fixed = TRUE)
results_list <- gsub(".05", " .05", results_list, fixed = TRUE)
results_list <- gsub("7.5", " 7.5", results_list, fixed = TRUE)
results_list <- gsub(".26", " .26", results_list, fixed = TRUE)
results_list <- gsub("9.5", " 9.5", results_list, fixed = TRUE)
results_list <- gsub("8.5", " 8.5", results_list, fixed = TRUE)

# take out any extra spaces that those changes put in:
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[1:length(results_list)], 
                     str_split, 
                     pattern = " ")

# even after that, this row has an extra space:
split_list[split_list[275] != ""]


# in order to bind the list together as rows, they need to be the same length.
for (i in 1:length(split_list)){
  
  # get length of row
  # 31 is assuming only 1 sub-stand in ever surveyed twice
  # if 31 is not the maximum length, we'll need to edit this code
  max_length <- 26
  row_length <- length(split_list[[i]][[1]])
  
  # for lists that start with "QUAD," find length of "row" (actually a list)
  if (row_length > 1){
    
    # this code adds NAs to the row to match max_length
    if (row_length < max_length) {
      add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
        na_if("")
      split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
    }
    
    # for lists that are empty, put the appropriate number of NAs in, based on the
    # length of the corrected "QUAD"  list  
  } else if (row_length <= 1) {
    split_list[[i]][[1]][1:max_length] <- NA
    
  } else {
    print("ERROR: value greater than 26 possibly detected")
    # ideally, would want to print which file once this is written for multiple
  }
  
}


data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T))

# Now to get 2nd rows to tack onto the first row
# Tidyverse: filter based on odd/even rows 

# starting at row 1, select evey second row (odds)
data_pt1 <- data %>% filter(row_number() %% 2 == 1)

# same for evens:
data_pt2 <- data %>% filter(row_number() %% 2 == 0)

# cbind to get everything together:
data1984 <- cbind(data_pt1, data_pt2)

# de-select the NA rows I added to make it a data frame:
data1984 <- data1984[c(1:45)]

#get rid of the 2nd half quadrat names:
data1984 <- data1984[-(27)]

# add columns
data1984 <- rbind(species, data1984) # it works!!

# make 1st row column names:
data1984 <- data1984 %>%
  row_to_names(row_number = 1)

# make column for month, and a year column:
data1984$month <- NA
data1984$year <- 1984
data1984$stand <- 2

# add months based on row numbers:
data1984[c(1:25), 45] = "5"
data1984[c(26:50), 45] = "6"
data1984[c(51:75), 45] = "7"
data1984[c(76:100), 45] = "8"
data1984[c(101:125), 45] = "9"
data1984[c(126:150), 45] = "10"

## the plot no.'s have a number at the end - taking it off
quad <- select(data1981, (1))
data1984 <- cbind(data1984, quad)
data1984 <- data1984[-(1)]

#reorder and change small number codes:
data1984_clean <- data1984 %>% 
  select(stand, year, month, PLOT, everything())
data1984_clean[data1984_clean == "."] <- ".01"
data1984_clean[data1984_clean == "R"] <- ".1"
data1984_clean[data1984_clean == "+"] <- ".5"

#################################################################################################
# 1985:
# open a connection to the file we want to read in
con <- file('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/s285.v_data.txt') 
open(con)

# make a list to put the results into
results_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

# CLEAN FILE #

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# this one doesn't have metadata

#gsub finds the 0.01's and replaces them with 0 and .01. The next step will recognize that they're separate because of the space. It saves as a "value", but after the next line it ends up as "data" again.
results_list <- gsub(".01", " .01", results_list, fixed = TRUE)

# for 1985, looks like they started doing quads 4 twice, and entering the mean value between them. So, let's deal with these:

#first, seperate temperatures from the long string of stuck together numbers
results_list <- gsub("52", "52 ", results_list, fixed = TRUE)
results_list <- gsub("62", "62 ", results_list, fixed = TRUE)
results_list <- gsub("61", "61 ", results_list, fixed = TRUE)
results_list <- gsub(".55", " .55", results_list, fixed = TRUE)
results_list <- gsub(".06", " .06", results_list, fixed = TRUE)
results_list <- gsub("1.3", " 1.3", results_list, fixed = TRUE)
results_list <- gsub("1.5", " 1.5", results_list, fixed = TRUE)
results_list <- gsub("2.5", " 2.5", results_list, fixed = TRUE)
results_list <- gsub("4.5", " 4.5", results_list, fixed = TRUE)
results_list <- gsub("5.5", " 5.5", results_list, fixed = TRUE)

results_list <- gsub(".75", " .75", results_list, fixed = TRUE)
results_list <- gsub("1.2", " 1.2", results_list, fixed = TRUE)
results_list <- gsub("6.5", " 6.5", results_list, fixed = TRUE)
results_list <- gsub("3.5", " 3.5", results_list, fixed = TRUE)
results_list <- gsub(".05", " .05", results_list, fixed = TRUE)
results_list <- gsub("7.5", " 7.5", results_list, fixed = TRUE)
results_list <- gsub(".26", " .26", results_list, fixed = TRUE)
results_list <- gsub("9.5", " 9.5", results_list, fixed = TRUE)
results_list <- gsub("8.5", " 8.5", results_list, fixed = TRUE)
results_list <- gsub("0.3", " 0.3", results_list, fixed = TRUE)
results_list <- gsub("2.6", " 2.6", results_list, fixed = TRUE)
results_list <- gsub("1.8", " 1.8", results_list, fixed = TRUE)
# take out any extra spaces that those changes put in:
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[1:length(results_list)], 
                     str_split, 
                     pattern = " ")

# in order to bind the list together as rows, they need to be the same length.
for (i in 1:length(split_list)){
  
  # get length of row
  # 31 is assuming only 1 sub-stand in ever surveyed twice
  # if 31 is not the maximum length, we'll need to edit this code
  max_length <- 26
  row_length <- length(split_list[[i]][[1]])
  
  # for lists that start with "QUAD," find length of "row" (actually a list)
  if (row_length > 1){
    
    # this code adds NAs to the row to match max_length
    if (row_length < max_length) {
      add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
        na_if("")
      split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
    }
    
    # for lists that are empty, put the appropriate number of NAs in, based on the
    # length of the corrected "QUAD"  list  
  } else if (row_length <= 1) {
    split_list[[i]][[1]][1:max_length] <- NA
    
  } else {
    print("ERROR: value greater than 26 possibly detected")
    # ideally, would want to print which file once this is written for multiple
  }
  
}


data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T))

# Now to get 2nd rows to tack onto the first row
# Tidyverse: filter based on odd/even rows 

# starting at row 1, select evey second row (odds)
data_pt1 <- data %>% filter(row_number() %% 2 == 1)

# same for evens:
data_pt2 <- data %>% filter(row_number() %% 2 == 0)

# cbind to get everything together:
data1985 <- cbind(data_pt1, data_pt2)

### there is an extra .1 for Aug E2, it looks like it was an enter error, so ignoring it

# de-select the NA rows I added to make it a data frame:
data1985 <- data1985[c(1:45)]

#get rid of the 2nd half quadrat names:
data1985 <- data1985[-(27)]

# add columns
data1985 <- rbind(species, data1985) # it works!!

# make 1st row column names:
data1985 <- data1985 %>%
  row_to_names(row_number = 1)

# make column for month, and a year column:
data1985$month <- NA
data1985$year <- 1985
data1985$stand <- 2

# add months based on row numbers:
data1985[c(1:25), 45] = "5"
data1985[c(26:50), 45] = "6"
data1985[c(51:75), 45] = "7"
data1985[c(76:100), 45] = "8"
data1985[c(101:125), 45] = "9"
data1985[c(126:150), 45] = "10"

## the plot no.'s have a number at the end - taking it off by just adding quadrat column from 1981
quad <- select(data1981, (1))
data1985 <- cbind(data1985, quad)
data1985 <- data1985[-(1)]

#reorder:
data1985_clean <- data1985 %>% 
  select(stand, year, month, PLOT, everything())

############################################################################################
## 1980

# Addding in 1980 data that was re-entered from field copies:
data1980 <- read.csv('./Hondo/VascularPlantSurvey/raw_data/Stand2/old_v_missing/Hondo_VascularSurvey_Stand2_1980_08.csv')
# what needs to be done here:
# -  either reformat to look like others, or wait until later
# - rename quadrats at some point (can't start with a number)
# - SODE species
data1980 <- data1980 %>% 
  select(-c(species.code, Comments, year, month))

# switch columns and rows to go with the other data
data1980_long <- data1980 %>% 
  pivot_longer(cols = starts_with("X"), names_to = "PLOT", values_to = "cover")
data1980_wide <- data1980_long %>% 
  pivot_wider(names_from = "species", values_from = "cover")


#add in one month of quadrat names:
quad2 <- quad %>% slice(1:25)
data1980_clean <- cbind(data1980_wide, quad2)
data1980_clean <- data1980_clean[-(1)]

#add misc info
data1980_clean$stand <- 2
data1980_clean$year <- 1980
data1980_clean$month <- 8

# Dealing with the different species problems:
# RUST = listed in data sheets as RUST (american raspberry) in 1980 and 1981, but the entered 1981 data has it as RUID (european raspberry). I'm trusting the 1981 data sheet, so changing it in 1980.
# SODE = same thing as above, in 1981 entered data it was changed to SOSP
# LYTR should be LYCO
# PYSE and PYVI were also changed to be the same species in subsequent years as PYCH, adding them together
data1980_clean$PYCH <- data1980_clean$PYSE + data1980_clean$PYVI
data1980_clean<- data1980_clean %>% 
  rename(SOSP = SODE,
         RUID = RUST,
         LYCO = LYTR) %>% 
  select(-c(PYSE, PYVI))

#################################################################################################################################################################################################
########################################################################################################################################################################################

# Add it all together:
stand2 <- bind_rows(data1981_clean, data1982_clean, data1983_clean, data1984_clean, data1985_clean)

# need to change characters to numeric to go with 1980 data
stand2[5:47] <- lapply(stand2[5:47], as.numeric)
stand2$month <- as.numeric(stand2$month)

stand2 <- bind_rows(stand2, data1980_clean)

# where temps are zero, they weren't recorded. Changing them to NA
stand2$TEMP[stand2$TEMP ==0] <- NA
stand2 <- stand2 %>% 
  arrange(year)

# are any of the cells empty?
which (is.null(stand2)) #nope!

#write_csv(stand2, 'VascularPlantSurveys/Hondo_VascularSurvey_Stand2_missingfiles.csv')

## Add this into the other cleaned data for stand 2:
# change the format to match Ellen's files
stand2 <- stand2 %>% 
  rename(Stand = stand,
         Year = year,
         Month = month,
         Quad = PLOT,
         Temp_F = TEMP)
Stand_2_Cover_80s <- stand2 %>% select(-Temp_F)
Stand_2_Temp_80s <- stand2 %>% select(c(Quad, Temp_F, Month, Year, Stand))

#read in Ellen's stand 2 files
Stand_2_Cover_recent <- read.csv('./Hondo/VascularPlantSurvey/Hondo_compiled/Stand_2_Cover.csv') 
Stand_2_Temp_recent <- read.csv('./Hondo/VascularPlantSurvey/Hondo_compiled/Stand_2_Temp.csv') 
Stand_2_Cover <- bind_rows(Stand_2_Cover_80s, Stand_2_Cover_recent)

Stand_2_Temp <- bind_rows(Stand_2_Temp_80s, Stand_2_Temp_recent)

#add these to the cleaned data file
#write_csv(Stand_2_Cover, './Hondo/VascularPlantSurvey/Hondo_compiled/Stand_2_Cover_complete.csv')
#write_csv(Stand_2_Temp, './Hondo/VascularPlantSurvey/Hondo_compiled/Stand_2_Temp_complete.csv')
