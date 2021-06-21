#join data
library(tidyverse)
library(taxize)

file.list <- list.files("./AOS/VascularSurvey/metadata", pattern = "sp_codes")
all_taxa <-data.frame()

for (file in 1:length(file.list)){
  df <- read_csv(paste("./AOS/VascularSurvey/metadata/", file.list[file], sep = ""), col_names = F)
  colnames(df) <- c("sp.code","sp.name")
  if (file == 1){
    all_metadata <- df
  }
  else {
    all_metadata <- all_metadata %>% full_join(df)
  }
}

unique_taxa <- all_metadata %>% 
  mutate(sp.code = toupper(sp.code)) %>% 
  unique() %>% arrange(sp.name)

corrected_taxa <- unique_taxa %>% 
  mutate(correct.sp.code = str_replace_all(sp.code, c("DRASP" = "DRSP",
                                                      "LOCA" = "LOVI",
                                                      "RACE" = "RASP",
                                                      "SALSP" = "SASP",
                                                      "SESE" = "SESP",
                                                      "STLO2" = "STLO"
                                                      )),
         correct.sp.name = str_replace_all(sp.name, c("Sheperdia canadensis" =
                                                        "Shepherdia canadensis",
                                                      "Stellaria longipes--duplicate" =
                                                        "Stellaria longipes"))) %>% 
  select(-sp.name) %>% 
  unique()

corrected_taxa <- corrected_taxa[-120,]

#checked spelling using taxize & it's all good!

### data

file.list <- list.files("./AOS/VascularSurvey/raw_data/csv_files", pattern = "*.csv")

all_vc <- data.frame()

for (file in 1:length(file.list)){
  df <- read_csv(paste("./AOS/VascularSurvey/raw_data/csv_files/", file.list[file], sep = ""))
  sp.code <- as.data.frame(colnames(df)) %>% rename(sp.code = 1)
  join <- left_join(sp.code, corrected_taxa) %>% 
    mutate(sp.code = if_else(is.na(correct.sp.code), sp.code, correct.sp.code))
  colnames(df) <- join$sp.code
  
  dupe_cols <- colnames(df[,which(duplicated(colnames(df)))])
  dupes <- df[,which(colnames(df) %in% dupe_cols)]
  
  sum_col <- data.frame()
  cn <- as.vector(colnames(dupes[,col]))
  for (dc in 1:length(cn)){
    subset <- dupes[colnames(dupes) == cn[dc]]
    for (row in 1:nrow(dupes)){
      sum_col[row,dc] <- dupes[row, 1] + dupes[row, 2]
      colnames(sum_col) <- cn
    }
  }
  
  nodupe_df <- df[,-which(colnames(df) %in% dupe_cols)]
  
  df <- cbind(nodupe_df, sum_col)
  
  if (file == 1){
    all_vc <- df
  }
  else {
    all_vc <- all_vc %>% full_join(df)
  }
}

temperature <- all_vc %>% select(stand,year,month,day,quad,TEMP)

cover_only <- all_vc %>% select(-TEMP) %>% relocate(stand,year,month,day,quad)

write_csv(temperature, "./AOS/VascularSurvey/clean_data/AOS_soil_temp_1981_1984.csv")
write_csv(cover_only, "./AOS/VascularSurvey/clean_data/AOS_vascular_cover_1981_1984.csv")
