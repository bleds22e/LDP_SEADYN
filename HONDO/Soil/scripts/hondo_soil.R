### Hondo SOIL TEMP + MOISTURE: assembling & cleaning data ###
## AVH June 2021 ##

library(tidyverse)
library(lubridate)

file.list <- list.files("./Hondo/Soil/raw_data", full.names = F)
col_names <- c("relative_day","stand","sensor_id","temp_C", "matric_potential_bars",
               "sensor_error", "year")

hondo_soil_data <- as.data.frame(matrix(ncol = 7))
colnames(hondo_soil_data) <- col_names

for (file in 1:length(file.list)){
  filename = file.list[file]
  fileyear = paste("19", substr(filename, 5,6), sep = "") # extract year from file name
  df <- read.csv(paste("./Hondo/Soil/raw_data/", filename, sep = ""), header = F, sep = "",
                 col.names = col_names) # read in txt file with no header, sep is a space, add column names for joining
  df$year <- fileyear
  df <- df[which(df$sensor_error != "*"),] # remove all cases with sensor errors
  hondo_soil_data <- rbind(hondo_soil_data, df) # bind to a single dataframe
} 

hondo_soil_data <- hondo_soil_data %>% select(-sensor_error) %>% na.omit()

hondo_soil_dates <- hondo_soil_data %>% mutate(date = ymd(paste(year, "05-01", sep = "")) + relative_day) %>% select(-year, -relative_day)

#write_csv(hondo_soil_dates, "./Hondo/Soil/clean_data/SEADYN_Hondo_SoilTempMP_1982_1984.csv")

# now to QC the data

library(assertr)

soil <- read_csv("./Hondo/Soil/clean_data/SEADYN_Hondo_SoilTempMP_1982_1984.csv")

soil %>% verify(relative_day > 0) # some values of -1, but as this is a relative day, it seems that some soil measurements were taken before the 0 mark
soil %>% verify(stand %in% c(1,2,3))
soil %>% verify(sensor_id > 0)
soil %>% assert(within_bounds(-30,30), soil_temp_C)
soil %>% verify(soil_matric_potential_bars > 0)
soil %>% assert(within_bounds(1982,1984), year)

# everything looks fine, no need to resave
