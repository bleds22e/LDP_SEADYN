### Hondo LITTER DATA: READING AND CLEANING ###
### AVH June 2021 ###

# load in packages
library(tidyverse)
library(assertr)

# similar to the analogous script for AOS: taking txt files, reading in litter data according 
# to file format described in the George LaRoi 1987 report on these data
# Look at that script for properly annotated code

file.list <- list.files("./Hondo/Litter/raw_data/txt_files")

for (file in 1:length(file.list)){
  filename = file.list[file]
  rawfileloc = paste("./Hondo/Litter/raw_data/txt_files/", filename, sep = "")
  file <- read.csv2(rawfileloc, header = FALSE, sep = "")
  empty_data <- data.frame()
  site_names <- c()
  row_started = 0
  for (row in 1:length(file$V1)){
    if (length(which(file[row,] == "&")) > 0 & row_started == "1"){
      new_row <- cbind(new_row, file[row,])
      print("case2")
    }
    if (length(which(file[row,] == "&")) > 0 & row_started == "0"){
        new_row <- file[row,]
        row_started = 1
        print("case1")
      }
    if (length(which(file[row,] == "&")) == 0){
      new_row <- cbind(new_row, file[row,])
      empty_data <- rbind(empty_data, new_row)
      row_started = 0
      print("case3")
      new_row <- c()
    }
  }
  save_file <- paste("./Hondo/Litter/raw_data/csv_files/", paste(str_remove(filename, ".txt"), ".csv", sep = ''),
                     sep = "")
  write.csv(empty_data, save_file, row.names = FALSE)
}

file.list <- list.files("./Hondo/Litter/raw_data/csv_files/")

dates_cols <- read_csv("./Hondo/Litter/metadata/Hondo_LitterDates.csv") %>% 
  unite(comp_date, c(component, date), sep = "/")

all_hondo_litter <- as.data.frame(matrix(ncol = 5, nrow = 0))
colnames(all_hondo_litter) <- c("component","sample_date","biomass","stand", "trap_number")
all_hondo_litter <- all_hondo_litter %>% mutate_at(c("component", "sample_date", "stand", "trap_number"), as.factor) %>% 
  mutate(biomass= as.numeric(biomass))
file = 3
for (file in 1:length(file.list)){
  filename = file.list[file]
  location = substr(filename, 2, 2)
  df <- read_csv(paste("./Hondo/Litter/raw_data/csv_files/", filename, sep = ""))
  remove.cols <- which(df[1,] == "&" | (df[1,] == "") | is.na(df[1,]))
  df <- df %>% select(-remove.cols) %>% mutate_all(as.numeric) %>% slice(1:10) %>% select(1:70)
  colnames(df) <- dates_cols$comp_date
  df$trap_number <- as.factor(c(1:10))
  df_end <- length(df)
  df_pivot <- df %>% pivot_longer(cols = 1:(df_end-1), names_to = c("component","sample_date"), names_sep = "/", 
                                  values_to = "biomass") %>% 
    mutate(sample_date = if_else(sample_date == "NA", "annual_total", sample_date)) %>% 
    mutate(stand = location,
           trap_number = factor(trap_number))
  all_hondo_litter <- all_hondo_litter %>% full_join(df_pivot)
}

#write_csv(all_hondo_litter %>% filter(sample_date != "annual_total"), "./Hondo/Litter/clean_data/Hondo_LitterBiomass_1983_1984.csv")

# QC the data

library(assertr)

hondo_litter <- read_csv("./Hondo/Litter/clean_data/Hondo_LitterBiomass_1983_1984.csv")

levels(as.factor(hondo_litter$component))

hondo_litter <- hondo_litter %>% filter(component != "empty") # we don't need the empty data since this doesn't actually correspond to anything 

hondo_litter %>% verify(substr(date, 1,4) %in% c("1983", "1984"))
hondo_litter %>% verify(substr(date, 6,7) %in% as.character(as.vector(sprintf("%0.2d", seq(1:12)))))
hondo_litter %>% verify(substr(date, 9,10) %in% as.character(as.vector(sprintf("%0.2d", seq(1:31)))))
hondo_litter %>% verify(biomass >= 0)

hondo_litter2 <- hondo_litter %>% rename(biomass_g_per_m2 = biomass, 
                                         date = sample_date) %>% arrange(stand, date, trap_number, component, biomass_g_per_m2)

write_csv(hondo_litter2, "./Hondo/Litter/clean_data/Hondo_LitterBiomass_1983_1984.csv")
