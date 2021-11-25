## Vascular Plant Surveys - Data cleaning 
# JAL - 2/22/21

# What this script does: reads in the compiled cover data, figures out the problems with species names and column titles, and writes new files into the final folder

library(janitor)
library(tidyverse)
library(snakecase)
library(assertr)

setwd("./Hondo/VascularCover/Hondo_compiled")
myfiles <- list.files(pattern = "*.csv", full.names = FALSE)

list2env(
  lapply(setNames(myfiles, make.names(gsub("*.csv","", myfiles))), 
         read.csv), envir = .GlobalEnv)

setwd("../../../")

#take random decimals in column names out
Stand_1_Cover <- clean_names(Stand_1_Cover, case = "parsed")
Stand_2_Cover_complete <- clean_names(Stand_2_Cover_complete, case = "parsed")
Stand_3_Cover <- clean_names(Stand_3_Cover, case = "parsed")
Stand_4_Cover <- clean_names(Stand_4_Cover, case = "parsed")
Stand_5_Cover <- clean_names(Stand_5_Cover, case = "parsed")
Stand_6_Cover <- clean_names(Stand_6_Cover, case = "parsed")
Stand_7_Cover <- clean_names(Stand_7_Cover, case = "parsed")
Stand_8_Cover <- clean_names(Stand_8_Cover, case = "parsed")

cover2 <- Stand_1_Cover %>% full_join(Stand_2_Cover_complete) %>% 
  full_join(Stand_3_Cover) %>% full_join(Stand_4_Cover) %>% full_join(Stand_5_Cover) %>% 
  full_join(Stand_6_Cover) %>% full_join(Stand_7_Cover) %>% full_join(Stand_8_Cover)

# repair issues: 
# 1: DOTR = DITR (one is present where the other is absent and vice versa in stand 3, 1990, 
# + least keystrokes involved to make mistake)
# 2: coalesce split columns (GEPU, LAOC, SEIN)
# 3: remove X2IPH

cover3 <- cover2 %>% mutate(GEPU = coalesce(GEPU, GEPU_2), SEIN = coalesce(SEIN, SEIN_2), 
                            LAOC = coalesce(LAOC,LAOC_2), DITR = coalesce(DOTR, DITR)) %>% 
  select(-GEPU_2, -SEIN_2, -LAOC_2, -X2IPH, -DOTR)

# correct species codes to current taxonomy:
corrected_taxa <- read.csv("./Hondo/VascularCover/metadata/Hondo_VascularSpList.csv") 

code <- as.data.frame(colnames(cover3)) %>% rename(original_code = 1) # create data frame of column names
join <- code %>% left_join(corrected_taxa) %>% # join this column with the taxonomic data to rename incorrect codes with correct version
  mutate(sp.code = if_else(is.na(unified_code), original_code, unified_code))
colnames(df) <- join$sp.code # and rename the dataframe columns

# averaging double cover measurements

cover4 <- cover3 %>% 
  rename(month = Month, year = Year, stand = Stand, quad = Quad) %>% 
  # scrub the .1 indicator of double surveys
  mutate(quad = as.factor(str_remove_all(quadrat, ".1"))) %>% 
  # replace all missing values with zeroes since these are true zeroes (plant not detected in quadrat)
  mutate_at(.vars = 5:219, ~replace_na(.,0)) %>% 
  group_by(month, year, stand, quad) %>% # group by time and plot
  summarize_all(mean) %>% ungroup # and take the average of double-surveyed plots

# add in sampling dates
dates <- read_csv("./Hondo/VascularCover/metadata/Hondo_VascularCover_Dates.csv")

cover5 <- cover4 %>% left_join(dates) %>% relocate(stand, .before = month) %>% 
  relocate(date, .after = year) %>% mutate(quad = toupper(quad))

# sometimes exact date wasn't included in the metadata file, so these are real NA values

# QC data

cover5 %>% assert(within_bounds(0,100), 6:220) %>% assert(within_bounds(1, 12), month) %>%
  assert(within_bounds(1980,2015), year) %>% assert(within_bounds(1,8), stand)

write_csv(cover5, "./Hondo/VascularCover/clean_data/Hondo_VascularCover_1980_2015.csv")

##############################################################################

#checking the temp data:
temp <- bind_rows(Stand_1_Temp, Stand_2_Temp_complete, Stand_3_Temp, Stand_4_Temp, Stand_5_Temp, Stand_6_Temp, Stand_7_Temp, Stand_8_Temp) %>% 
  mutate(Temp_C = (Temp_F-32)*5/9, .keep = "unused") # convert to Celsius
names(temp) <- to_snake_case(names(temp))
temp <- temp %>% rename(temp_C = temp_c) %>% left_join(dates) %>% relocate(stand, .before = quad) %>% 
  relocate(year, .after = stand) %>% relocate(month, .after = year) %>% relocate(date, .after = month)

# QC

range(temp$temp_C) # some NA temperature values -> get rid of these

temp2 <- temp %>% filter(is.na(temp_C) == F)

range(temp2$temp_C) # seems reasonable

# all the other columns have already passed muster within the QC for vascular data, so save it

write_csv(temp, "./Hondo/VascularCover/clean_data/Hondo_ProbeTemp_1980_2010.csv")
##############################################################################