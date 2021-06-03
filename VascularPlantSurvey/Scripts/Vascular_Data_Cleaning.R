## Vascular Plant Surveys - Data cleaning 
# JAL - 2/22/21

# What this script does: reads in the compiled cover data, figures out the problems with species names and column titles, and writes new files into the final folder

library(janitor)
library(tidyverse)
library(snakecase)

setwd("./VascularPlantSurvey/Hondo_compiled")
myfiles <- list.files(pattern = "*.csv", full.names = FALSE)

list2env(
  lapply(setNames(myfiles, make.names(gsub("*.csv","", myfiles))), 
         read.csv), envir = .GlobalEnv)

setwd("../../")

#take random decimals in column names out
Stand_1_Cover <- clean_names(Stand_1_Cover, case = "parsed")
Stand_2_Cover_complete <- clean_names(Stand_2_Cover_complete, case = "parsed")
Stand_3_Cover <- clean_names(Stand_3_Cover, case = "parsed")
Stand_4_Cover <- clean_names(Stand_4_Cover, case = "parsed")
Stand_5_Cover <- clean_names(Stand_5_Cover, case = "parsed")
Stand_6_Cover <- clean_names(Stand_6_Cover, case = "parsed")
Stand_7_Cover <- clean_names(Stand_7_Cover, case = "parsed")
Stand_8_Cover <- clean_names(Stand_8_Cover, case = "parsed")

cover2 <- bind_rows(Stand_1_Cover, Stand_2_Cover_complete, Stand_3_Cover, Stand_4_Cover, Stand_5_Cover, Stand_6_Cover, Stand_7_Cover, Stand_8_Cover)

######
# here I'll work out the species names problems:
#GEPU 
gepu2 <- filter(cover2, !is.na(GEPU_2)) #all zeros
gepu <- filter(cover2, !is.na(GEPU)) #these are actually values
#no overlapping values, merge into one column:
cover2 <- cover2 %>% 
  mutate(GEPU = coalesce(GEPU, GEPU_2)) %>% 
  select(-GEPU_2)

#same as sein_2
sein2 <- filter(cover2, !is.na(SEIN_2))
#all zeros!
sein <- filter(cover2, !is.na(SEIN))
cover2 <- cover2 %>% 
  mutate(SEIN = coalesce(SEIN, SEIN_2)) %>% 
  select(-SEIN_2)

#LAOC
laoc2 <- filter(cover2, !is.na(LAOC_2))# these both have real values but never overlap
laoc <- filter(cover2, !is.na(LAOC)) 
#merge into one column:
cover2 <- cover2 %>% 
  mutate(LAOC = coalesce(LAOC, LAOC_2)) %>% 
  select(-LAOC_2)

#####################################################################################

# Looking at the species mistakes:
#DOTR is maybe COTR
dotr <- cover2 %>% select(DOTR)
#these are all zeros, so I'm assuming this is an error - taking it out:
cover2 <- cover2 %>% select(-DOTR)

#EAPN:
eapn <- cover2 %>% select(c(Month, Year, Stand, EAPN))
# there is non zero data in 1995 - there's no hard copies for this

# FZSB
fz <- cover2 %>% select(c(Month, Year, Stand, FZSB))
#this is a non zero value from the 2001 resurvey - do not have the hard copy

# RUSP
rusp <- cover2 %>% select(c(Month, Year, Stand, RUSP))
#this is a 2001 value in stand 6

#X2IPH
x2 <- cover2 %>% select(c(Month, Year, Stand, X2IPH))
#there are no vlaues for this, so I think it is a mistake. Taking it out:
cover2 <- cover2 %>% select(-X2IPH)

#getting just the species names to match up to the master list
species <- cover2[1,-c(1:4)]
species2 <- species %>% pivot_longer(cols = everything(), names_to = "Species", values_to = "value") 
species2 <- species2[,1]



#read in master species list
master <- read.csv("./VascularPlantSurvey/metadata/Archived_species_names/Species_List_Vascular_Surveys_not corrected.csv")

#add them together. The species with NA's are the ones we dont know!
final_species <- semi_join(master, species2, copy = TRUE)
final_species <- final_species %>% select(-c(Order.from.csv.file, X, X.1, X.2))

#writing this as its own file:
#write.csv(final_species, "./VascularPlantSurveys/Species_List_Vascular_Surveys.csv", row.names = F)

##############################################################################

#checking the temp data:
temp <- bind_rows(Stand_1_Temp, Stand_2_Temp_complete, Stand_3_Temp, Stand_4_Temp, Stand_5_Temp, Stand_6_Temp, Stand_7_Temp, Stand_8_Temp) %>% 
  mutate(Temp_C = (Temp_F-32)*5/9, .keep = "unused") # convert to Celsius
names(temp) <- to_snake_case(names(temp))
#write_csv(temp, "./VascularPlantSurvey/Hondo_final/temps_all.csv")
##############################################################################

# averaging double cover measurements

cover3 <- cover2 %>% 
  rename(month = Month, year = Year, stand = Stand, quad = Quad) %>% 
  # scrub the .1 indicator of double surveys
  mutate(quad = as.factor(str_remove_all(quad, ".1"))) %>% 
  # replace all missing values with zeroes since these are true zeroes (plant not detected in quadrat)
  na_replace(0) %>% 
  group_by(month, year, stand, quad) %>% # group by time and plot
  summarize_all(mean) # and take the average of double-surveyed plots

#write_csv(cover3, "./VascularPlantSurvey/Hondo_final/cover_all.csv")

# read all the data in to their own stand files:

stand1 <- cover3 %>% filter(stand == "1")
write.csv(stand1, "./VascularPlantSurvey/Hondo_final/Stand_1_Cover.csv", row.names = F)
stand2 <- cover3 %>% filter(Stand == "2")
write.csv(stand1, "./VascularPlantSurvey/Hondo_final/Stand_2_Cover.csv", row.names = F)
stand3 <- cover3 %>% filter(Stand == "3")
write.csv(stand3, "./VascularPlantSurvey/Hondo_final/Stand_3_Cover.csv", row.names = F)
stand4 <- cover3 %>% filter(Stand == "4")
write.csv(stand4, "./VascularPlantSurvey/Hondo_final/Stand_4_Cover.csv", row.names = F)
stand5 <- cover3 %>% filter(Stand == "5")
write.csv(stand5, "./VascularPlantSurvey/Hondo_final/Stand_5_Cover.csv", row.names = F)
stand6 <- cover2 %>% filter(Stand == "6")
write.csv(stand6, "./VascularPlantSurvey/Hondo_final/Stand_6_Cover.csv", row.names = F)
stand7 <- cover3 %>% filter(Stand == "7")
write.csv(stand7, "./VascularPlantSurvey/Hondo_final/Stand_7_Cover.csv", row.names = F)
stand8 <- cover3 %>% filter(Stand == "8")
write.csv(stand8, "./VascularPlantSurvey/Hondo_final/Stand_8_Cover.csv", row.names = F)
