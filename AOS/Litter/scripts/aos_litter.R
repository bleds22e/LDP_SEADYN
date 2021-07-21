## LITTER PRODUCTION IN AOS SURVEY AREAS IN 1983-1984 ##
## AVH June 2021 ##

# This script takes all the litter txt files for AOS substands where litter was collected in traps,
# and joins this wide format data into a five-column long format where the trap number, litter component, 
# sampling date (in the case of AOS_litter_interannual.csv), sampling stand, and corresponding biomass are recorded.

# Column headers are entered in AOS_litter_dates.csv from a physical GLR report.

library(tidyverse)

file.list <- list.files("./AOS/Litter/raw_data/txt_files")

for (file in 1:length(file.list)){ # read in each individual litter file
  filename = file.list[file]
  rawfileloc = paste("./AOS/litter/raw_data/txt_files/", filename, sep = "")
  file <- read.csv2(rawfileloc, header = FALSE, sep = "")
  empty_data <- data.frame()
  stand_names <- c()
  row_started = 0
  for (row in 1:length(file$V1)){ # if there is no ampersand (indicates line break) and the row for a trap has started...
    if (length(which(file[row,] == "&")) > 0 & row_started == "1"){
      new_row <- cbind(new_row, file[row,]) # the indexed row should be joined to an existing new row
      print("case2")
    }
    if (length(which(file[row,] == "&")) > 0 & row_started == "0"){ # if instead it's the first line of a row ...
      new_row <- file[row,] # the new row becomes the row currently being indexed
      row_started = 1
      print("case1")
    }
    if (length(which(file[row,] == "&")) == 0){
      new_row <- cbind(new_row, file[row,]) # the same join of existing and current row should occur
      empty_data <- rbind(empty_data, new_row) # but then this row is finished - bind to dataframe being built
      row_started = 0 # reset the row counter so R knows to start a new row
      print("case3")
      new_row <- c() # and empty the row for good measure
    }
  }
  empty_data <- empty_data[, colSums(empty_data != "") != 0] #remove any empty rows in the dataframe
  empty_data$trap_number <- seq(from = 1, to = 12, by = 1)
  save_file <- paste("./AOS/Litter/raw_data/csv_files/", paste(str_remove(filename, ".txt"), ".csv", sep = ''),
                     sep = "")
  write.csv(empty_data, save_file, row.names = FALSE)  # and save the df
}

# now that raw text files are written to csv, read in as csv files and manipulate to join and name the cols appropriately

file.list <- list.files("./AOS/Litter/raw_data/csv_files")

# read in the collection dates and components from the metadata file
dates_cols <- read_csv("./AOS/Litter/metadata/SEADYN_AOS_LitterDates.csv") %>% 
  unite(comp_date, c(component, date), sep = "/") # combine the component & date info into one name for now

all_aos_litter <- as.data.frame(matrix(ncol = 5, nrow = 0)) # start a blank file to join to eventually
colnames(all_aos_litter) <- c("component","sample_date","biomass","stand", "trap_number")
all_aos_litter <- all_aos_litter %>% mutate_at(c("component", "sample_date", "stand"), as.factor) %>% 
  mutate_at(c("trap_number", "biomass"), as.numeric)

for (file in 1:length(file.list)){
  filename = file.list[file]
  location = substr(filename, 1, 2)
  df <- read_csv(paste("./AOS/Litter/raw_data/csv_files/", filename, sep = ""))
  remove.cols <- which(df[1,] == "&") # remove cols with ampersand
  df <- df %>% select(-remove.cols) %>% mutate_all(as.numeric)
  df_names <- dates_cols %>% filter(stand == location) %>% select(comp_date) # get vector of column names for that stand
  df_names <- rbind(as.vector(df_names), "trap_number")
  colnames(df) <- df_names$comp_date #rename columns correctly
  df_end <- length(df) - 1
  df_pivot <- df %>% pivot_longer(cols = (1:df_end), names_to = c("component","sample_date"), names_sep = "/",
                                  values_to = "biomass") %>%
    mutate(sample_date = if_else(sample_date == "NA", "annual_total", sample_date)) %>% 
    mutate(stand = location) # get in long format, and create a column to indicate stand before joining data
  all_aos_litter <- all_aos_litter %>% full_join(df_pivot) 
}

# save files for discrete sampling dates and annual means

all_aos_litter_noann <- all_aos_litter  %>% mutate(stand = toupper(stand)) %>% filter(sample_date != "annual_total") %>% 
  rename(biomass_g_per_m2 = biomass)

write_csv(all_aos_litter, "./AOS/Litter/clean_data/SEADYN_AOS_LitterBiomass_1983_1984.csv")

library(assertr)

ia_litter <- read_csv("./AOS/Litter/clean_data/SEADYN_AOS_LitterBiomass_1983_1984.csv")

summary(ia_litter)
levels(as.factor(ia_litter$stand))
levels(as.factor(ia_litter$component))

hist(ia_litter$biomass) # need to clarify units of mass in col name (g? mg?)

ia_litter %>% verify(substr(sample_date, 1, 4) %in% c("1983", "1984"))
ia_litter %>% verify(substr(sample_date, 6,7) %in% as.character(as.vector(sprintf("%0.2d", seq(1:12)))))
ia_litter %>% verify(substr(sample_date, 9,10) %in% as.character(as.vector(sprintf("%0.2d", seq(1:31)))))

       