## Living Data Project
# SEADYN Hondo Tree Dynamics
# Jenna Loesberg, February 16th, 2021
# Updated by AVH September 2021

### What this script does:
# - Read in the 8 tree dynamics data files
# - change the coloumn names into something sensible
# - some data cleaning before reading into new, final csv's

library(tidyverse)
library(janitor)

setwd("./Hondo/Mensuration/2003_clean_data/")

file.list <- list.files("./")

noheader_read <- function(i){
  read.csv(i, header = F)
}

list2env(lapply(setNames(file.list, make.names(gsub("*.csv$", "", file.list))),
                noheader_read), envir = .GlobalEnv)

setwd("../../../")

column_names <- c("stand", "plot", "tag", "species_code", "DBH_1983_in", "base_coord_N_1983_m", 
                  "base_coord_S_1983_m", "base_coord_E_1983_m", "base_coord_W_1983_m", "year_dead",
                  "fallen_stem_length_1983_m", "exit_coord_N_1983_m", "exit_coord_S_1983_m", 
                  "exit_coord_E_1983_m", "exit_coord_W_1983_m", "year_fallen_1983", "status", 
                  "age_1983", "height_1983_m", "stem_lean_amt_scaled_1983", "stem_lean_dir_1983", 
                  "stem_lean_amt_1991", "stem_lean_dir_1991", "stem_lean_amt_2000", "stem_lean_dir_2000",
                  "crown_width_NS_1983_m", "crown_width_EW_1983_m", "height_to_crown_1983_m", "density_1983_percent", "BSD_1983", 
                  "radial_10year_inc_1983_mm", "DBH_1991_cm", "DBH_2000_cm", "height_2000_m", 
                  "tree_code_2000", "fire_code_2000", "unknown_dummy", "comments")
stand1_clean <- rbind(column_names, stand1)
stand1_clean <- janitor::row_to_names(stand1_clean, row_number = 1)
stand1_clean <- stand1_clean[-c(1:6),]

# each df has different numbers of rows at the beginning, so need to change them separately:
stand2_clean <- rbind(column_names, stand2)
stand2_clean <- janitor::row_to_names(stand2_clean, row_number = 1)
stand2_clean <- stand2_clean[-c(1:5),]

stand3_clean <- rbind(column_names, stand3)
stand3_clean <- janitor::row_to_names(stand3_clean, row_number = 1)
stand3_clean <- stand3_clean[-c(1:5),]

stand4_clean <- rbind(column_names, stand4)
stand4_clean <- janitor::row_to_names(stand4_clean, row_number = 1)
stand4_clean <- stand4_clean[-c(1:5),]

stand5_clean <- rbind(column_names, stand5)
stand5_clean <- janitor::row_to_names(stand5_clean, row_number = 1)
stand5_clean <- stand5_clean[-c(1:4),]

stand6_clean <- rbind(column_names, stand6)
stand6_clean <- janitor::row_to_names(stand6_clean, row_number = 1)
stand6_clean <- stand6_clean[-c(1:5),]

stand7_clean <- rbind(column_names, stand7)
stand7_clean <- janitor::row_to_names(stand7_clean, row_number = 1)
stand7_clean <- stand7_clean[-c(1:5),]

stand8_clean <- rbind(column_names, stand8)
stand8_clean <- janitor::row_to_names(stand8_clean, row_number = 1)
stand8_clean <- stand8_clean[-c(1:5),]

#bind them into one data frame:
tree_dyn <- do.call("rbind", list(stand1_clean, stand2_clean, stand3_clean, stand4_clean, stand5_clean, stand6_clean, stand7_clean, stand8_clean))

# remove blank rows
tree_dyn <- tree_dyn[c(1:7335),]
	
# change periods to NA's:
is.na(tree_dyn) <- tree_dyn == "."

# there are two X species - what are these??
# they are supposed to be "S" species - says so on data sheet!
tree_dyn$species_code[tree_dyn$tag == "5270"]  <- "S"
tree_dyn$species_code[tree_dyn$tag == "5271"]  <- "S"

# changing these species codes to match the ones used in vascular surveys:
#- species codes: A = POTR, B = BEPA, F = ABBA, J = PIBA, L = LALA, P = POBA, S = PIMA, W = PIGL
tree_dyn$species_code[tree_dyn$species_code == "A"]  <- "POTR"
tree_dyn$species_code[tree_dyn$species_code == "B"]  <- "BEPA"
tree_dyn$species_code[tree_dyn$species_code == "F"]  <- "ABBA"
tree_dyn$species_code[tree_dyn$species_code == "J"]  <- "PIBA"
tree_dyn$species_code[tree_dyn$species_code == "L"]  <- "LALA"
tree_dyn$species_code[tree_dyn$species_code == "P"]  <- "POTR"
tree_dyn$species_code[tree_dyn$species_code == "S"]  <- "PIMA"
tree_dyn$species_code[tree_dyn$species_code == "W"]  <- "PIGL"
unique(tree_dyn$species_code)

# change dbh in 1983 from inches to cm (like the rest of the dbh measurements)
tree_dyn2 <-  tree_dyn %>% 
  mutate(DBH_1983_in = as.numeric(DBH_1983_in),
         DBH_1983_cm = DBH_1983_in * 2.5,
         DBH_1983_cm = round(DBH_1983_cm, 1)) %>% #change to one decimal point
  select(-DBH_1983_in) %>% 
  relocate(DBH_1983_cm, .after = species_code)


# removing unknown columns
tree_dyn2 <-  tree_dyn2 %>% 
  select(-c(unknown_dummy, status))

# for scaled stem lean in 1983, s, m, and l are small, medium, and large. Changing m and l to upper case:
tree_dyn2$stem_lean_amt_scaled_1983[tree_dyn2$stem_lean_amt_scaled_1983 == "m"] <- "M"
tree_dyn2$stem_lean_amt_scaled_1983[tree_dyn2$stem_lean_amt_scaled_1983 == "l"] <- "L"

# change coordinate system to match saplings (stem position S becomes negative stem position N, same with E -> W)

tree_dyn3 <- tree_dyn2 %>% mutate_at(6:9, as.numeric) %>% mutate(base_coord_S_1983_m = if_else(is.na(base_coord_N_1983_m),
                                                                base_coord_S_1983_m, (5 - base_coord_N_1983_m)),
                                  base_coord_W_1983_m = if_else(is.na(base_coord_E_1983_m),
                                                                base_coord_W_1983_m, (5 - base_coord_E_1983_m))) %>% 
                           mutate_at(12:15, as.numeric) %>% mutate(exit_coord_S_1983_m = if_else(is.na(exit_coord_N_1983_m),
                                                                          exit_coord_S_1983_m, (5 - exit_coord_N_1983_m)),
                                            exit_coord_W_1983_m = if_else(is.na(exit_coord_E_1983_m),
                                                                          exit_coord_W_1983_m, (5 - exit_coord_E_1983_m))) %>% 
  select(-base_coord_N_1983_m, -base_coord_E_1983_m, -exit_coord_N_1983_m, -exit_coord_E_1983_m) %>% 
  rename(quadrat = plot) %>% 
  mutate(fire_code_2000 = str_remove_all(fire_code_2000, "F")) %>% # F denotes (I think) that tree was killed by fire, and L denotes (in one case) living-ness
  mutate(fire_code_2000 = str_remove_all(fire_code_2000, "L")) # remove F and L to leave only RBC

###########################################################################################
summary(tree_dyn3)

#write_csv(tree_dyn3, "./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2001.csv")

# now to QC
library(assertr)

tree_dynamics <- read_csv("./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2001.csv", guess_max = 5000)

tree_dynamics %>% assert(within_bounds(1,8), stand)
tree_dynamics %>% verify(tag > 0) # do we want to keep in trees that have no tag number? Yes for now

levels(as.factor(tree_dynamics$species_code)) # species codes valid and spelled correctly

tree_dynamics %>% assert(within_bounds(0,5), c(base_coord_S_1983_m,base_coord_W_1983_m,
                                               exit_coord_S_1983_m, exit_coord_W_1983_m))
# base stem position relative to the centre of the plot should be within the quadrat (5 m max value)
# as should be exit position. one case has exit coordinate of 6. This was changed to 5.

tree_dynamics[1972,"exit_coord_S_1983_m"] <- 5

tree_dynamics %>% verify(year_dead <= 2015 | is.na(year_dead))

tree_dynamics %>% verify(year_fallen_1983 <= 1983 | is.na(year_fallen_1983))
# 1988 is the latest year that appears in this column; thus, I am renaming
tree_dynamics2 <- tree_dynamics %>% rename(year_fallen_1988 = year_fallen_1983,
                                           tree_tag = tag,
                                           quad = quadrat)

levels(as.factor(tree_dynamics2$stem_lean_amt_scaled_1983))
tree_dynamics3 <- tree_dynamics2 %>% mutate(stem_lean_amt_scaled_1983 = if_else(
  stem_lean_amt_scaled_1983 == "0", "Z", if_else(
    stem_lean_amt_scaled_1983 == "O", "Z", if_else(
      stem_lean_amt_scaled_1983 == "N", "M", if_else(
        stem_lean_amt_scaled_1983 == "H", "M", stem_lean_amt_scaled_1983
      )
    )
  ) 
)) # corrections noted in the metadata

levels(as.factor(tree_dynamics3$stem_lean_dir_1983))
tree_dynamics4 <- tree_dynamics3 %>% mutate(stem_lean_dir_1983 = if_else(
  stem_lean_dir_1983 == "SES", "SSE", if_else(stem_lean_dir_1983 == "SWS", "SSW", stem_lean_dir_1983)))

levels(as.factor(tree_dynamics4$stem_lean_dir_1991))
levels(as.factor(tree_dynamics4$stem_lean_dir_2000))

tree_dynamics4 %>% assert(within_bounds(0,90), c(stem_lean_amt_1991, stem_lean_amt_2000))

levels(as.factor(tree_dynamics4$tree_code_2000)) # what is ---, DB, DDB ?? DB only one occurrence, looks like DB2 (since DBH could be recorded)
# --- looks like it should be NA
# DDB ... dead down broken? DD is good enough for this

tree_dynamics5 <- tree_dynamics4 %>% mutate(tree_code_2000 = if_else(tree_code_2000 == "DB", "DB2",
                                                                           if_else(tree_code_2000 == "DDB", "DD",
                                                                                   tree_code_2000)))
tree_dynamics5$tree_code_2000[tree_dynamics5$tree_code_2000 == "---"] <- NA

levels(as.factor(tree_dynamics5$fire_code_2000))

# check to see if the stem height (from ground to top of tree) greater than/equal to height to canopy

tree_dyn6 <- tree_dynamics5 %>% select(tree_tag, height_1983_m, height_to_crown_1983_m) %>% na.omit() 
tree_dyn6 %>% verify(height_1983_m >= height_to_crown_1983_m) # this is violated in 15 cases

problem_tree <- (tree_dyn6[which(tree_dyn6$height_to_crown_1983_m > tree_dyn6$height_1983_m),])
remove <- as.vector(problem_tree$tree_tag)
tree_dynamics6 <- tree_dynamics5 %>% filter((tree_tag %in% remove) == FALSE) %>% 
  # remove cases where height to canopy absurdly large compared to stem height
  rename(BSD_1983_cm = BSD_1983)

# everything has units, is consistent with other dfs and is of a reasonable value

#write_csv(tree_dynamics6, "./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2001.csv")
