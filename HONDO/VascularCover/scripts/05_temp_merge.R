## Assembling and QCing stand temperature data from vascular surveys

library(tidyverse)
library(snakecase)

setwd("./Hondo/VascularCover/Hondo_compiled")
myfiles <- list.files(pattern = "*.csv", full.names = FALSE)

file.list <- list.files("./Hondo/VascularCover/Hondo_compiled",
                        pattern = "*_Temp.csv") # list only temperature files

for (file in 1:length(file.list)){ # read in temperature files
  filename = paste("./Hondo/VascularCover/Hondo_compiled/", file.list[file],
                   sep = "")
  current_file <- read_csv(filename)
  if (file == 1){
    df <- current_file
  }
  else {
    df <- full_join(df, current_file)
  }
}

col.names <- colnames(df)
col.names2 <- to_any_case(col.names, case = "snake") # make all column names snakecase

colnames(df) <- col.names2

df2 <- df %>% mutate(temp_C = (temp_f - 32)*5/9) %>% select(-temp_f) # convert to C from F

write_csv(df2, "./Hondo/VascularCover/clean_data/Hondo_SoilTemp_1980_2010.csv")

qc_temp <- read_csv("./Hondo/VascularCover/clean_data/Hondo_SoilTemp_1980_2010.csv")
range(qc_temp$temp_C) # looks fine
qc_temp %>% verify(month %in% 1:12) %>% verify(year %in% 1980:2010) %>% verify(stand %in% 1:8)
levels(as.factor(qc_temp$quad)) # there are double measurements of temp for some quads (as there are multiple cover measurements)
# need to average these

qc_temp2 <- qc_temp %>% mutate(quad = str_remove_all(quad, ".1")) %>% 
  group_by(month,year,stand,quad) %>% 
  summarize(temp_C = mean(temp_C)) %>% 
  arrange(stand,quad,year,month,temp_C)
# done!
write_csv(qc_temp2, "./Hondo/VascularCover/clean_data/Hondo_SoilTemp_1980_2010.csv")

