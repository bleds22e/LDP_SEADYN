# cleaning bryoid data

# get cover data sorted out

library(tidyverse)
library(janitor)

setwd("./BryoidSurvey/txt_files/")
file.list <- list.files(path = "./cover_data_raw/single_entry", pattern = "*.txt")

# for quadrats where name of quad was entered only once w/ data split over multiple lines

for (file in 1:length(file.list)){
  filename = file.list[file]
  rawfileloc = paste("./cover_data_raw/single_entry/", filename, sep = "")
  file <- read.csv2(rawfileloc, header = FALSE, sep = "")
  trim.positions <- grep("[AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz]", file$V1)
  empty_data <- data.frame()
  for (pos in 1:length(trim.positions)){
    if (pos < length(trim.positions)){
      start.trim = trim.positions[pos]
      end.trim = trim.positions[pos+1] - 1
    }
    else {
      start.trim = trim.positions[pos]
      end.trim = length(file$V1)
    }
    quadrat_df <- file[start.trim:end.trim,]
    if (length(quadrat_df$V1) == 2){
      row_1 <- quadrat_df[1, ]
      row_2 <- quadrat_df[2, ]
      row_1_clean <- vector()
      row_2_clean <- vector()
      for (col in 1:length(row_1)){
        if (row_1[,col] != "" & is.na(row_1[,col]) == FALSE){
          row_1_clean <- cbind(row_1_clean, row_1[,col])
        }
      }
      for (col in 1:length(row_2)){
        if (row_2[,col] != "" & is.na(row_2[,col]) == FALSE){
          row_2_clean <- cbind(row_2_clean, row_2[,col])
        }
      }
      quad_row <- cbind(row_1_clean, row_2_clean)
    }
    else if (length(quadrat_df$V1)==3) {
      row_1 <- quadrat_df[1, ]
      row_2 <- quadrat_df[2, ]
      row_3 <- quadrat_df[3, ]
      row_1_clean <- vector()
      row_2_clean <- vector()
      row_3_clean <- vector()
      for (col in 1:length(row_1)){
        if (row_1[,col] != "" & is.na(row_1[,col]) == FALSE){
          row_1_clean <- cbind(row_1_clean, row_1[,col])
        }
      }
      for (col in 1:length(row_2)){
        if (row_2[,col] != "" & is.na(row_2[,col]) == FALSE){
          row_2_clean <- cbind(row_2_clean, row_2[,col])
        }
      }
      for (col in 1:length(row_3)){
        if (row_3[,col] != "" & is.na(row_3[,col]) == FALSE){
          row_3_clean <- cbind(row_3_clean, row_3[,col])
        }
      }
      quad_row <- as.data.frame(cbind(cbind(row_1_clean, row_2_clean),row_3_clean))
    }
      else if (length(quadrat_df$V1)==4){
        row_1 <- quadrat_df[1, ]
        row_2 <- quadrat_df[2, ]
        row_3 <- quadrat_df[3, ]
        row_4 <- quadrat_df[4, ]
        row_1_clean <- vector()
        row_2_clean <- vector()
        row_3_clean <- vector()
        row_4_clean <- vector()
        for (col in 1:length(row_1)){
          if (row_1[,col] != "" & is.na(row_1[,col]) == FALSE){
            row_1_clean <- cbind(row_1_clean, row_1[,col])
          }
        }
        for (col in 1:length(row_2)){
          if (row_2[,col] != "" & is.na(row_2[,col]) == FALSE){
            row_2_clean <- cbind(row_2_clean, row_2[,col])
          }
        }
        for (col in 1:length(row_3)){
          if (row_3[,col] != "" & is.na(row_3[,col]) == FALSE){
            row_3_clean <- cbind(row_3_clean, row_3[,col])
          }
        }
        for (col in 1:length(row_4)){
          if (row_4[,col] != "" & is.na(row_4[,col]) == FALSE){
            row_4_clean <- cbind(row_4_clean, row_4[,col])
          }
        }
      quad_row <- as.data.frame(cbind(cbind(cbind(row_1_clean, row_2_clean),row_3_clean),row_4_clean))
    }
    empty_data <- rbind(empty_data, quad_row)
    empty_data <- empty_data %>% 
      mutate(V1 = str_remove_all(V1, "[']")) %>% 
      mutate_at(-1, as.numeric)
  }
  csvname <- gsub(".txt", ".csv", filename)
  cleanfileloc <- paste("./cover_data_clean/", csvname, sep = "")
  write_csv(empty_data, cleanfileloc, col_names = F)
}

# second type of file = 'double entry'. every row has quadrat entered
# problem with this file = decimal cover entries run together with prior numbers. sep by "." and whitespace
# same method as for stand2 Vascular Plant Survey (readLines)

file.list <- list.files(path = "./cover_data_raw/double_entry", pattern = "*.txt")
file <- read.csv2(paste("./cover_data_raw/double_entry/",file.list[1], sep = ""), header = FALSE, sep = "[")

for (file in 1:length(file.list)){
  filename = file.list[file]
  rawfileloc = paste("./cover_data_raw/double_entry/", filename, sep = "")
  con <- file(rawfileloc)
  open(con)
  results_list <- list()
  current_line <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    results_list[current_line] <- line
    current_line <- current_line + 1
  } 
  close(con)
  results_list <- lapply(results_list, str_trim, side = "both") %>% 
    lapply(., str_squish)
  results_list <- gsub(".01", " .01", results_list)
  split_list <- lapply(results_list, 
                       str_split, 
                       pattern = " ")
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
  data_pt1 <- data %>% filter(row_number() %% 2 == 1)
  data_pt2 <- data %>% filter(row_number() %% 2 == 0) %>% select(-1)
  alldata <- cbind(data_pt1, data_pt2) %>% remove_empty(which = "cols")
  csvname <- gsub(".txt", ".csv", filename)
  cleanfileloc <- paste("./cover_data_clean/", csvname, sep = "")
  write_csv(alldata, cleanfileloc, col_names = F)
}


## now on to the metadata

file <- read.csv2("./species_codes/s181.b.sp.5.txt", sep = "", col.names = F)

start.trim <- min(grep("VARIABLE", file$EDIT, fixed = TRUE))
end.trim <- min(grep("INPUT MEDIUM", file$EDIT, fixed = TRUE)) - 1

column_names <- file[start.trim:end.trim,]

full_col_names <- vector()

for (i in 1:length(column_names)){
  chunk <- column_names[i]
  full_col_names <- paste(full_col_names, chunk)
}

full_col_names <- str_trim(full_col_names, side = "both")
full_col_names <- as.data.frame(str_split(full_col_names, pattern = " ", simplify = T))
full_col_names <- full_col_names[, which(full_col_names != "")]

# loop this for everything to get clean column names
# associate data with column names
# break down file names into informative columns
