### AOS DENDROCHRONOLOGY : reading & cleaning ###
## AVH June 2021 ##
library(tidyverse)
file.list <- list.files("./Hondo/Dendro/raw_data/")

all_dendro_data <- data.frame(matrix(ncol = 4))
colnames(all_dendro_data) <- c("year","tree_no","ring_width_mm","site")

start.trim <- seq(from = 1, to = 44, by = 4)
end.trim <- seq(from = 4, to = 44, by = 4)

for (file in 1:length(file.list)){
  filename = file.list[file]
  location = substr(filename, 11,12)
  filedir = paste("./Hondo/Dendro/raw_data/",filename,sep = "")
  con <- file(filedir)
  open(con)
  results_list <- list()
  current_line <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
    results_list[current_line] <- line
    current_line <- current_line + 1
  } 
  close(con) # at this point, have read in the file line by line
  df <- data.frame() # create a dataframe to store unlisted data
  
  for (line in 1:length(results_list)){ # for each line, ensure that white space entries are logged as being really there
    line_vector <- c()
    for (trim in 1:11){
      x <- as.numeric(substr(results_list[[line]], start.trim[trim], end.trim[trim]))
      line_vector <- c(line_vector, x)
    }
    df <- rbind(df, line_vector) # split up values, reclass as vector, and join to df
  }
  no_trees = length(df) - 1
  colnames <- c("year", seq(from = 1, to = no_trees, by = 1))
  colnames(df) <- colnames # create a column names vector, and bind this to the df to help with joining
  df_pivot <- df %>% pivot_longer(2:(no_trees+1), names_to = "tree_no", values_to = "ring_width_mm") %>% 
    mutate(site = location) # pivot to long format where each row is a unique ring width observation
  all_dendro_data <- rbind(all_dendro_data, df_pivot)
}

all_dendro_data <- all_dendro_data %>%
  na.omit() %>% mutate(site = as.numeric(site)) %>%
  rename(stand = site) %>% mutate(ring_width_mm = ring_width_mm/100) # and unite the tree number and site into a unique id for each tree followed

# write dataframe in long format for now
View(all_dendro_data)
#write_csv(all_dendro_data,"./Hondo/Dendro/clean_data/Hondo_Dendrochronology_1983.csv")

dendro <- read_csv("./Hondo/Dendro/clean_data/Hondo_Dendrochronology_1983.csv")
summary(dendro)
levels(as.factor(dendro$stand))
hist(dendro$ring_width_mm)

dendro <- dendro %>% arrange(stand,year,tree_no,ring_width_mm)
write_csv(dendro, "./Hondo/Dendro/clean_data/Hondo_Dendrochronology_1983.csv")
