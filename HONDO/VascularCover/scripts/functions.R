# FUNCTIONS #

filename_to_metadata <- function(file_path){
  
  # fxn to read in Hondo file names and pull out metadata (stand, month, year)
  myfiles <- as.data.frame(list.files(file_path, 
                                      full.names = FALSE, 
                                      pattern = "*.txt"))
  colnames(myfiles) <- c("file_name")
  
  myfiles <- myfiles %>% 
    # split the name into columns named hondo and month (aka Hondo189 and JUN)
    separate(file_name, c("hondo", "month", NA)) %>% 
    # make sure the month is capitalized
    # make a new column called year, fill with last 2 characters from hondo col
    # make a new column called stand, fill with character third from the end of hondo
    mutate("month" = str_to_upper(month),
           "year" = as.numeric(str_sub(hondo, start = -2)),
           "stand" = str_sub(hondo, start = -3, end = -3)) %>% 
    # for the year column, add 2000 if the value in the column created above is
    # less than 50; add 1900 if it is greater than 50
    mutate("year" = as.character(if_else(year < 50, year + 2000, year +1900))) %>% 
    # change the 3 letter months to numbers
    mutate("month" = ifelse(month == 'JAN', 1, 
                            ifelse(month == 'FEB', 2,
                                   ifelse(month == 'MAR', 3,
                                          ifelse(month == 'APR', 4, 
                                                 ifelse(month == 'MAY', 5,
                                                        ifelse(month == 'JUN', 6,
                                                               ifelse(month == 'JUL', 7,
                                                                      ifelse(month == 'AUG', 8,
                                                                             ifelse(month == 'SEP', 9, 
                                                                                    ifelse(month == 'OCT', 10, 
                                                                                           ifelse(month == 'NOV', 11,
                                                                                                  ifelse(month == 'DEC', 12, NA))))))))))))) %>% 
    # remove the hondo column (all data has been extracted from it)
    select(-hondo)
  
  return(myfiles)
  
}


read_in_txt_file <- function(file_path){
  
  ### READ IN FILE ###
  ## once all files have been made into txt files
  
  # open a connection to the file we want to read in
  con <- file(file_path) 
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
  
  return(results_list)
  
}


txt_file_to_df <- function(results_list){
  
  ### TURN INTO DATAFRAME ###
  
  # remove remaining white spaces and make everything uppercase
  results_list <- lapply(results_list, str_trim, side = "both") %>% 
    lapply(., str_squish) %>% 
    lapply(., str_to_upper)
  
  # get the rows after the metadata and unlist them so each value gets read 
  # separately; otherwise, each row is one big long character string
  split_list <- lapply(results_list[1:length(results_list)], 
                       str_split, 
                       pattern = " ")
  
  ## find first "quad" row and cut out the rest (remove metadata at top of file)
  # empty vector
  quad_rows <- vector()
  for (i in 1:length(split_list)){
    if (split_list[[i]][[1]][1] == 'QUAD') {
      #  add each row/list num that starts with 'QUAD' to the vector
      quad_rows <- c(quad_rows, i)
    }
  }
  # select only the rows/lists from the first 'QUAD' row through the end
  # this removes all of the lines of metadata from the top of the file
  split_list <- split_list[min(quad_rows):length(split_list)]
  
  ## in order to bind the list together as rows, they need to be the same length
  for (i in 1:length(split_list)){
    
    # get length of row first row (a 'QUAD' row)
    max_length <- 36
    row_length <- length(split_list[[i]][[1]])
    
    ## make each type of row the correct length
    if (row_length > 1){
      
      # this code adds NAs to the row to match max_length
      if (row_length < max_length) {
        
        # if the length of the row is less than the max length, make a vector
        # of NAs needed to match the max length
        add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
          na_if("")
        # append that vector of NAs to the row
        split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
      }
      
      # for lists that are empty, make a vector of NAs as long as max_length
    } else if (row_length <= 1) {
      split_list[[i]][[1]][1:max_length] <- NA
    } 
    
  }
  
  # stitch lists together to act as rows in a dataframe
  cover_df <- data.frame(matrix(unlist(split_list), 
                                nrow = length(split_list), 
                                byrow = T)) %>% 
    # remove the empty rows
    janitor::remove_empty("rows") %>% 
    # make the first row ("QUAD") into the column names
    janitor::row_to_names(., row_number = 1) %>% 
    # remove any remaining "QUAD" rows (filter throws an error, for some reason)
    .[.$QUAD != 'QUAD',]
  
  # make sure all columns have unique names
  colnames(cover_df) <- make.unique(colnames(cover_df))
  
  # if any columns are all NA, remove them
  not_any_na <- function(x) all(!is.na(x))
  cover_df <- cover_df %>% select(where(not_any_na))
  
  # make tidy 
  cover_df_long <- rename(cover_df, "Species" = "QUAD") %>% 
    pivot_longer(2:ncol(cover_df), names_to = "Quad") %>% 
    rename("Cover" = "value")
  
  return(cover_df_long)
  
}
