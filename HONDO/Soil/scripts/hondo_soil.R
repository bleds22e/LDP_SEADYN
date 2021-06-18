### HONDO SOIL TEMP + MOISTURE: assembling & cleaning data ###
## AVH June 2021 ##

library(tidyverse)

file.list <- list.files("./HONDO/Soil/raw_data", full.names = F)
col_names <- c("relative_day","stand","sensor_id","soil_temp_C", "soil_matric_potential_bars",
               "sensor_error", "year")

hondo_soil_data <- as.data.frame(matrix(ncol = 7))
colnames(hondo_soil_data) <- col_names

for (file in 1:length(file.list)){
  filename = file.list[file]
  fileyear = paste("19", substr(filename, 5,6), sep = "") # extract year from file name
  df <- read.csv(paste("./HONDO/Soil/raw_data/", filename, sep = ""), header = F, sep = "",
                 col.names = col_names) # read in txt file with no header, sep is a space, add column names for joining
  df$year <- fileyear
  df <- df[which(df$sensor_error != "*"),] # remove all cases with sensor errors
  hondo_soil_data <- rbind(hondo_soil_data, df) # bind to a single dataframe
} 

hondo_soil_data <- hondo_soil_data %>% select(-sensor_error) %>% na.omit()

#write_csv(hondo_soil_data, "./HONDO/Soil/clean_data/hondo_soil.csv")
