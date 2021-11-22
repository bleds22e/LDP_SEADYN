### Hondo BRYOID SURVEYS: reading in and cleaning data ###
## AVH June 2021 ##

library(tidyverse)
library(janitor)

file.list <- list.files(path = "./Hondo/BryoidCover/raw_data/1981_1984/txt_files/single_entry")

# first set of data: surveys for quadrats where name of quad was entered only once with data split over multiple lines
# of each txt file

for (file in 1:length(file.list)){
  filename = file.list[file]
  rawfileloc = paste("./Hondo/BryoidCover/raw_data/1981_1984/txt_files/single_entry/", filename, sep = "")
  file <- read.csv2(rawfileloc, header = FALSE, sep = "")
  trim.positions <- grep("[AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz]", file$V1) 
  # one can tell the start of a new line by the presence of a letter, so identify these break points
  empty_data <- data.frame()
  for (pos in 1:length(trim.positions)){
    if (pos < length(trim.positions)){ # if the line number is less than the total length of the file...
      start.trim = trim.positions[pos]
      end.trim = trim.positions[pos+1] - 1
    }
    else { # otherwise, will need to set the 'end' trim position as the length of the file (otherwise pos counter runs into issues)
      start.trim = trim.positions[pos]
      end.trim = length(file$V1)
    }
    quadrat_df <- file[start.trim:end.trim,]
    for (line in 1:length(quadrat_df$V1)){
      if (line == 1){
        row <- quadrat_df[line, ]
      }
      else {
        row <- cbind(row, quadrat_df[line, ])
      }
      row <- row[which(row != "")]
    }
    empty_data <- rbind(empty_data, unlist(row))
  }
  empty_data[,1] <- str_remove_all(empty_data[,1], "[']")
  csvname <- gsub(".txt", ".csv", filename)
  cleanfileloc <- paste("./Hondo/BryoidCover/raw_data/1981_1984/csv_files/", csvname, sep = "")
  write_csv(empty_data, cleanfileloc, col_names = F)
}

# second type of file = 'double entry'. every row has quadrat entered
# problem with this file = decimal cover entries run together with prior numbers. sep by "." and whitespace
# same method as for stand2 Vascular Plant Survey (readLines)

file.list <- list.files(path = "./Hondo/BryoidCover/raw_data/1981_1984/txt_files/double_entry")

for (file in 1:length(file.list)){
  filename = file.list[file]
  rawfileloc = paste("./Hondo/BryoidCover/raw_data/1981_1984/txt_files/double_entry/", filename, sep = "")
  con <- file(rawfileloc)
  open(con)
  results_list <- list()
  current_line <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    results_list[current_line] <- line
    current_line <- current_line + 1
  } 
  close(con) # at this point, have read in the file line by line
  results_list <- lapply(results_list, str_trim, side = "both") %>% # trim whitespace
    lapply(., str_squish) # and squish together
  results_list <- gsub(".01", " .01", results_list) # add a space between 0 and 0.01 values to ensure these are separate
  split_list <- lapply(results_list, 
                       str_split, 
                       pattern = " ") # split entries by single space
  # at this point, each list entry is a vectorized row of the original data

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
                            byrow = T)) # convert to a matrix
  data_pt1 <- data %>% filter(row_number() %% 2 == 1) # extract all odd rows
  data_pt2 <- data %>% filter(row_number() %% 2 == 0) %>% select(-1) # extract all even rows, and remove the first col (since entry should be the quadrat name)
  alldata <- cbind(data_pt1, data_pt2) %>% remove_empty(which = "cols")
  csvname <- gsub(".txt", ".csv", filename)
  cleanfileloc <- paste("./Hondo/BryoidCover/raw_data/1981_1984/csv_files/", csvname, sep = "")
  write_csv(alldata, cleanfileloc, col_names = F)
}


## now on to the metadata

# read in the species taxonomic codes
code_convert <- read_csv("./Hondo/BryoidCover/metadata/bryoid_codes_final.csv") %>% 
  select(code, unified_code) 

file.list <- list.files(path = "./Hondo/BryoidCover/raw_data/1981_1984/species_codes/sp_files", pattern = "*.txt")
list.headers.sp <- list()
list.dates.sp <- list()
filename_save <- vector()

for (x in 1:length(file.list)){
  filename = file.list[x]
  rawfileloc = paste("./Hondo/BryoidCover/raw_data/1981_1984/species_codes/sp_files/", filename, sep = "")
  file <- read.csv2(rawfileloc, header = FALSE)
  start.trim <- min(grep("VARIABLE", file[,1], fixed = TRUE)) # identify where col names start
  end.trim <- min(grep("INPUT MEDIUM", file[,1], fixed = TRUE)) - 1 # and where they end
  column_names <- file[start.trim:end.trim,] # extract these rows of the text file
  extract_col_names <- vector()
  for (i in 1:length(column_names)){
    chunk <- column_names[i]
    extract_col_names <- paste(extract_col_names, chunk) # paste together all the col names into a string
  }
  extract_col_names <- as.matrix(str_split(str_trim(extract_col_names, side = "both"), pattern = " ", 
                                        simplify = T)) # and then split these up after trimming whitespace
  full_col_names <- as.vector(extract_col_names[, which(extract_col_names != "")]) #remove empty column names in the vector
  full_col_names <- full_col_names[-(1:2)]
  
  column_correction <- data.frame(full_col_names) %>% rename(code = full_col_names) %>% 
    left_join(code_convert) %>% mutate(correct_code = if_else(is.na(unified_code), code, unified_code)) 
  full_col_names <- column_correction$correct_code # correct old codes with new codes
  
  date.start.trim <- min(grep("SUBFILE LIST", file[,1], fixed = TRUE)) # extract date subfile info
  date.end.trim <- min(grep("INPUT FORMAT", file[,1], fixed = TRUE)) - 1
  date.list <- as.vector(unlist(str_split(str_trim(str_remove(file[date.start.trim:date.end.trim,], "SUBFILE LIST"), side = "both"), " ")))
  date.list <- date.list[-grep("[()]", date.list)]
  
  filename_save[x] <- str_remove_all(str_remove_all(filename, ".sp"), ".txt")
  list.headers.sp[[x]] <- full_col_names # now have one list of the column names (headers)
  list.dates.sp[[x]] <- date.list # and one list of the associated dates where multiple surveys were done
}

names(list.headers.sp) <- filename_save
names(list.dates.sp) <- filename_save
# output is list of vectors containing col names associated with their corresponding file name

# some species code files appear to be missing, but these are present in the calculation (.sx files) metadata...
# just need to modify a new loop to extract those as well

file.list <- list.files(path = "./Hondo/BryoidCover/raw_data/1981_1984/species_codes/sx_files", pattern = "*.txt")
list.headers.sx <- list()
list.dates.sx <- list()
filename_save <- vector()

for (x in 1:length(file.list)){
  filename = file.list[x]
  rawfileloc = paste("./Hondo/BryoidCover/raw_data/1981_1984/species_codes/sx_files/", filename, sep = "")
  file <- read.csv2(rawfileloc, header = FALSE)
  start.trim <- min(grep("/PLOTNO", file[,1], fixed = TRUE))
  end.trim <- grep("[(]", file[,1])[2] - 1
  column_names <- file[start.trim:end.trim,]
  extract_col_names <- vector()
  for (i in 1:length(column_names)){
    chunk <- column_names[i]
    extract_col_names <- paste(extract_col_names, chunk)
  }
  extract_col_names <- as.matrix(str_split(str_trim(extract_col_names, side = "both"), pattern = " ", 
                                           simplify = T))
  full_col_names <- str_remove(as.vector(extract_col_names[, which(extract_col_names != "")]), "/")

  column_correction <- data.frame(full_col_names) %>% rename(code = full_col_names) %>% 
    left_join(code_convert) %>% mutate(correct_code = if_else(is.na(unified_code), code, unified_code))
  full_col_names <- column_correction$correct_code
  
  date.start.trim <- min(grep("VALUE LABELS DATE", file[,1], fixed = TRUE))
  date.end.trim <- min(grep("SORT CASES BY DATE", file[,1], fixed = TRUE)) - 1
  date.list <- as.vector(unlist(str_split(str_trim(str_remove(file[date.start.trim:date.end.trim,], "VALUE LABELS DATE"), side = "both"), " ")))
  date.list.scrub <- str_remove_all(str_remove(date.list, "...."), "[']")
  date.list <- date.list.scrub[which(date.list.scrub != "")]
  
  filename_save[x] <- str_remove_all(str_remove_all(filename, ".sx"), ".txt")
  list.headers.sx[[x]] <- full_col_names
  list.dates.sx[[x]] <- date.list
}

names(list.headers.sx) <- filename_save
names(list.dates.sx) <- filename_save

list.all.headers <- c(list.headers.sp, list.headers.sx)
list.all.dates <- c(list.dates.sp, list.dates.sx)

# same story with outputs here, so have now joined to two super-lists

# now need to associate the data in csv files with these column names and sampling dates

# back to the bryoid cover data, now in csv files ... let's make a list of these dfs now

file.list <- list.files(path = "./Hondo/BryoidCover/raw_data/1981_1984/csv_files")
filenames <- vector()
data.list <- list()

for (i in 1:length(file.list)){
  file <- read_csv(paste("./Hondo/BryoidCover/raw_data/1981_1984/csv_files/", file.list[i], sep = ""), col_names = F)
  filename <- file.list[i]
  filenames[i] <- str_remove_all(str_remove_all(filename, ".st"), ".csv")
  data.list[[i]] <- file
}
names(data.list) <- filenames
# now have a list of dataframes with the name of each list item as the name of the original file

# here goes the loop to associate all the information
for (x in 1:length(data.list)){
  df <- data.list[[x]]
  filename <- names(data.list)[x]
  for (y in 1:length(list.all.headers)){
    if (filename == names(list.all.headers)[y]){ # if there is a correspondence b/n the data file name and the headers file name...
      column.header <- list.all.headers[[y]]
      colnames(df) <- column.header # rename the dataframe columns appropriately
    }
  }
  ### add in file name details - year column, quad size, stand number
  df$stand <- substr(filename, 2, 2) #stand number is in position 2 of the file name string
  df$year <- paste("19", substr(filename, start = 3, stop = 4), sep = "") # and the date (19**) in the 3-4 position
  if (str_detect(filename, "25")){
    df$quadrat_size = 25
  }
  else {
    df$quadrat_size = 5
  } # note which stand size we are dealing with
  # change quadrats to all have uppercase characters in them and drop trailing characters where present
  df$PLOTNO <- substr(toupper(df$PLOTNO), 1, 2)
  # add date now
  for (z in 1:length(list.all.dates)){
    if (filename == names(list.all.dates)[z]){ # if the file name from data matches that of the date info
      date.vector = list.all.dates[[z]] 
      date.counter = 1
      for (row in 1:length(df$PLOTNO)){ # for each row of the dataframe
        if (row < (length(df$PLOTNO)-1)){ # if it's not the last row
          quad.number = as.numeric(substr(df$PLOTNO[row+1], 1, 1)) # then the current quadrat is the next row
          quad.number.prior = as.numeric(substr(df$PLOTNO[row], 1, 1)) # and the 'prior' quadrat is the current row
          diff = abs(quad.number.prior - quad.number) # we are looking for a change in date by a change in quadrat number over 2 (e.g. 8 -> 0)
          df$date_code[row] = date.vector[date.counter]
          if (diff > 2){
            date.counter = date.counter + 1 # so transition to the next sampling date when this disjunction is detected
          }
        }
        else {df$date_code[row] = date.vector[date.counter]}
      }
    }
  }
  data.list[[x]] <- df
}

# finally, join and write a clean, enormous file -- check spp codes first and make a dataframe to associate known codes with species.

#big.list <- unique(unlist(list.all.headers))
#write.csv(big.list, "../../metadata/sp_codes_list.csv")
#this has been gone over in detail using taxize.R now to generate the earlier referenced
#bryoid_codes_final.csv
## check out taxize.R for script to resolve taxonomy questions

# little fixes to individual dataframes

data.list[[12]] <- as.data.frame(data.list[[12]]) %>% select(-41) # first plot (1B) has one extra number recorded that seems wrong (or like it should apply to CLACEN rather than the current 0)
data.list[[17]] <- as.data.frame(data.list[[17]]) %>% select(-28) # last col is all zeroes
data.list[[18]] <- as.data.frame(data.list[[18]]) %>% select(-28) # last col is all zeroes
data.list[[19]] <- as.data.frame(data.list[[19]]) %>% select(-28) # after cross-referencing with existing physical data, the last column does not correspond to any species data
data.list[[21]] <- as.data.frame(data.list[[21]]) %>% select(-28) # same stand (3) but in 1984 - if we assume the same data sheet was being used, then the last col of empty zeores does not correspond to anything
data.list[[34]] <- as.data.frame(data.list[[34]]) %>% select(-30) # only numbers in last col are for plot D, and are the same as data value reported elsewhere, suggesting maybe one species was double-counted?
data.list[[35]] <- as.data.frame(data.list[[35]]) %>% select(-30) # last col is all zeroes

# some problem with s782.b.5 and s782.b.st25 - more data than column headers (by 2)
# I think based on s783.b.25 data, that the two missing species are ICMERI and PELPUL

full.bryoid.data <- data.frame()

for (i in 1:length(data.list)){
  if (i == 39 | i == 40){
    df <- as.data.frame(data.list[[i]]) %>% rename("ICMERI" = 46, "PELPUL" = 47)
  }
  else {
    df <- as.data.frame(data.list[[i]])
  }
    df <- df %>% mutate(PLOTNO = as.factor(PLOTNO),
                        stand = as.integer(stand),
                        year = as.numeric(year),
                        quadrat_size = as.factor(quadrat_size)) %>%
      rename(quadrat = PLOTNO)
    if (i == 1){
      full.bryoid.data <- df
    }
    else {
      full.bryoid.data <- full.bryoid.data %>% full_join(df)
    }
  }

# fix date codes to be actual months and dates

month_code <- levels(as.factor(str_remove_all(substr(full.bryoid.data$date_code,1,3), "[1234567890]")))
month_numeric <- c(8,7,6,7,6,5,5,5,10,9)
month_convert <- as.data.frame(cbind(month_code, month_numeric)) # this little df will allow month codes to be assigned a numeric value (1-12)

fixed.bryoid.data <- full.bryoid.data %>% 
  mutate(month_code = as.factor(str_remove_all(substr(date_code,1,3),"[1234567890]")), # isolate month code
         day = as.numeric(str_remove_all(date_code, "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]"))) %>% # isolate day
  full_join(month_convert) %>% select(-month_code, -date_code) %>% rename(month = month_numeric) %>% 
  unite(key, c(stand,year,month,day,quadrat_size,quadrat), sep = "_") %>% 
  separate(key, into = c("stand","year","month","day","quadrat_size","quadrat"), sep = "_") # unite into key and then split to put columns at the beginning of the df

## still need to QC, but for now, looking GREAT!

# 1980s files that were manually entered (25 m2 stands 1-3)

file.list <- list.files(path = "./Hondo/BryoidCover/raw_data/1980_files")
hondo_1980s <- data.frame()

for (i in 1:length(file.list)){
  df <- read_csv(paste("./Hondo/BryoidCover/raw_data/1980_files/", file.list[i], sep = ""))
  df_length <- length(df)
  df2 <- df %>% pivot_longer(cols = 3:df_length, names_to = "quadrat", values_to = "cover") # need to change format to put quadrat as a grouping variable
  df3 <- df2 %>% pivot_wider(id_cols = c(key,quadrat), names_from = species, values_from = cover) %>% # and then pivot back to wide format where species is the column, quadrat is associated with each observation
    separate(key, into = c("stand","year","date_code","quadrat_size"), sep = "_") %>% 
    mutate(month_code = as.factor(str_remove_all(substr(date_code,1,3),"[1234567890]")),
           day = as.numeric(str_remove_all(date_code, "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]"))) %>%
    full_join(month_convert) %>% select(-month_code, -date_code) %>% rename(month = month_numeric)
  if (i == 1){
    hondo_1980s <- df3
  }
  else {
    hondo_1980s <- hondo_1980s %>% full_join(df3)
  }
}

hondo_1980s <- hondo_1980s %>% mutate(stand = as.factor(stand),
                                    quadrat_size = as.factor(quadrat_size),
                                    quadrat = as.factor(quadrat),
                                    day = as.factor(day),
                                    month = as.factor(day))

# join the files together
all_bryoid_data <- full_join(fixed.bryoid.data, hondo_1980s) # join the data for all years together

#write_csv(all_bryoid_data, "./Hondo/BryoidCover/clean_data/Hondo_BryoidCover_1980_1984.csv")

cover <- read_csv("./Hondo/BryoidCover/clean_data/Hondo_BryoidCover_1980_1984.csv") %>% 
  mutate_at(7:90, as.numeric)

cover %>% assert(within_bounds(0,100), 7:90) 
cover %>% assert(within_bounds(1, 8), 1)
cover %>% assert(within_bounds(1980,2021), 2)
cover %>% assert(within_bounds(1,12), 3)

cover <- cover %>% mutate(month = if_else(year == "1980", 8, month)) # 1980 all have day and month the same for some reason, but all surveyed in August

cover %>% assert(within_bounds(1,12), 3)
cover %>% verify(quadrat_size %in% c(5,25))

cover2 <- cover %>% slice(-(3101:3147))

cover2 %>% verify(quadrat_size %in% c(5,25))
cover2 <- cover2 %>% select(-day) # day not super important, just month and year
# all done! now overwrite original data with QCed data

#write_csv(cover2, "./Hondo/BryoidCover/clean_data/Hondo_BryoidCover_1980_1984.csv")

