## Assembling and QCing stand temperature data from vascular surveys

library(tidyverse)
library(snakecase)

setwd("./Hondo/VascularPlantSurvey/Hondo_compiled")
myfiles <- list.files(pattern = "*.csv", full.names = FALSE)

file.list <- list.files("./Hondo/VascularPlantSurvey/Hondo_compiled",
                        pattern = "*_Temp.csv")

for (file in 1:length(file.list)){
  filename = paste("./Hondo/VascularPlantSurvey/Hondo_compiled/", file.list[file],
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
col.names2 <- to_any_case(col.names, case = "snake")

colnames(df) <- col.names2

df2 <- df %>% mutate(temp_C = (temp_f - 32)*5/9) %>% select(-temp_f)

write_csv(df2, "./Hondo/VascularPlantSurvey/Hondo_final/SEADYN_Hondo_SoilTemp_1980_2010.csv")
