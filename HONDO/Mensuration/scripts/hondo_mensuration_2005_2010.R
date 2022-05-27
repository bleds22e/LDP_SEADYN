### Extracting 2005 and 2010 tree data from messy dataframes
### AVH September 2021

# read in packages
pkgs <- c("dplyr","stringr","assertr","skimr","readr")

lapply(pkgs, library, character.only = T)

## First, read in the relevant files

file.list <- list.files("./Hondo/Mensuration/2005_2010_raw/")

## Loop will trim away the unnecessary variables,
## rename columns,
## add stand information

# Create small df to convert between the shorthand species codes and unified taxonomic codes
tree_codes <- cbind(c("A","B","F","J","L","P","S","W"),
                    c("POTR","BEPA","ABBA","PIBA","LALA","POTR","PIMA","PIGL"))
colnames(tree_codes) <- c("species", "species_code")
tree_codes <- as.data.frame(tree_codes)

# for each file
for (file in 1:length(file.list)){
  file.name <- paste("./Hondo/Mensuration/2005_2010_raw/", file.list[file],
                     sep = "") # read in the file, select only columns with data and remove weird header information
  raw_file <- read_csv(file.name, col_names = F) %>% select(1:4, 38:49) %>% slice(-c(1:4))
  rename_file <- raw_file %>% rename(stand = 1, quad = 2, tree_tag = 3,
                                     species = 4, DBH_2005_cm = 5, height_2005_m = 6,
                                     code_2005 = 7, lean_amount_2005 = 8,
                                     lean_direction_2005 = 9, comments_2005 = 10,
                                     DBH_2010_cm = 11, height_2010_m = 12,
                                     code_2010 = 13, lean_amount_2010 = 14,
                                     lean_direction_2010 = 15, comments_2010 = 16) %>% slice(-1)
  # rename all the columns with consistent terminology
  tidy_file <- rename_file %>% left_join(tree_codes) %>% select(-species) 
  is.na(tidy_file) <- tidy_file == "." # replace periods with NA
  if (file == 1){
    full_file <- tidy_file
  }
  if (file > 1){
    full_file <- full_file %>% full_join(tidy_file) # join all the read-in files together
  }
}

tidy_file2 <- full_file %>% 
  mutate_at(c(1,2,3,6,8,12,14,16), as.factor) %>% mutate_at(c(4,5,7,10,11,13), as.numeric)

tidy_file3 <- tidy_file2 %>% mutate(comments_2005 = tolower(str_replace_all(comments_2005, "_", " ")),
                                    comments_2010 = str_replace_all(comments_2010, "_", " "))

tidy_file3 <- as_tibble(tidy_file3)

## Then, join these objects into one dataframe to join with -2001 data

treedyn_1980_2001 <- read_csv("./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2001.csv") %>% 
  mutate(stand = as.factor(stand), tree_tag = as.factor(tree_tag))

treedyn_1980_2010 <- treedyn_1980_2001 %>% full_join(tidy_file3)

# need to qc, but all joined together for now.

# QC data

# 1) tree species should be in the list of valid tree species codes
# 2) quadrats can only have certain specified values as well (1st character 0-9, 2nd letter A-J)
# 3) stand must be in 1:8
# 4) tree tag must be numeric
# 5) DBH and height for 2005/2010 must be greater than zero
# 6) Lean amount must be 0-90
# 7) Lean direction must be a compass direction in c(N, S, E, W, NE, NW, SE, SW)
# 8) Codes must be in list already described in metadata


treedyn_1980_2010 %>% assert(in_set(1:8), stand) %>% 
  mutate(quad_number = as.numeric(substr(quad,1,1)), quad_letter = substr(quad,2,2)) %>% 
  assert(in_set(0:9), quad_number) %>% 
  assert(in_set(c("A","B","C","D","E","F","G","H","I","J")), quad_letter) %>% 
  assert(in_set(c("PIBA","PIGL", "POTR","PIMA","LALA", "BEPA","ABBA")), species_code) %>% 
  verify(DBH_2005_cm > 0 | is.na(DBH_2005_cm)) %>% 
  verify(DBH_2010_cm > 0 | is.na(DBH_2010_cm)) %>% 
  verify(height_2005_m > 0 | is.na(height_2005_m)) %>% 
  verify(height_2010_m > 0 | is.na(height_2010_m)) %>% 
  verify(lean_amount_2005 >= 0 & lean_amount_2005 < 90 | is.na(lean_amount_2005)) %>% 
  verify(lean_amount_2010 >= 0 & lean_amount_2010 < 90 | is.na(lean_amount_2010)) %>% 
  assert(in_set(c("N","NE","E","SE","S","SW","W","NW")), c(lean_direction_2005,lean_direction_2010)) %>% 
  assert(in_set(c("DD","DL","DS","LB","LL","DB1","DB2", "LB_LL")), c(code_2005, code_2010))
  
skim(treedyn_1980_2010 %>% select(DBH_2005_cm,DBH_2010_cm,height_2005_m,height_2010_m))
# no outliers, looks good.

qc_treedyn <- treedyn_1980_2010 %>% rename(tree_code_2005 = code_2005,
                                           tree_code_2010 = code_2010,
                                           comments_2000 = comments) %>% 
  mutate(comments_2000 = str_replace_all(comments_2000, "_", " "))

# save QCed file

#write_csv(qc_treedyn, "./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2010.csv")

# make example figure of one stand with where different variables were measured??
# color code by variable... height = all, age = small subset, bsd = all ... multipanel with all trees, then color
# where certain variables apply.

letter_to_m <- cbind(LETTERS[1:10], c(0:9)) %>% as.data.frame() %>% rename(row = 1, metres_WE = 2) %>% 
  mutate(metres_WE = as.numeric(metres_WE)*5)

grid_theme <- theme_classic() +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    legend.position = "top",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 9),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = 7, face = "italic"),
    panel.spacing = unit(0.7, "lines"))

tree_measurements <- read_csv("./Hondo//Mensuration/clean_data/Hondo_Mensuration_1983_2010.csv") %>% 
  separate(quad, into = c("col","row"), 1,1) %>% 
  left_join(letter_to_m) %>% 
  mutate(metres_SN = as.numeric(col)*5 + base_coord_S_1983_m, metres_WE = metres_WE + base_coord_W_1983_m) %>% 
  mutate(species = case_when(species_code == "ABBA" ~ "A. balsamea",
                             species_code == "BEPA" ~ "B. papyrifera",
                             species_code == "POTR" ~ "P. tremuloides",
                             species_code == "PIBA" ~ "P. banksiana",
                             species_code == "PIGL" ~ "P. glauca",
                             species_code == "PIMA" ~ "P. marina",
                             species_code == "LALA" ~ "L. laricina"),
         is_dead = if_else((is.na(year_dead) | year_dead > 1983), 0, 1)) %>% 
  mutate(
         dbh_measured = if_else(is.na(DBH_1983_cm), "No", if_else(is_dead == 1, "Dead", "Yes")),
         age_measured = if_else(is.na(age_1983),"No", if_else(is_dead == 1, "Dead", "Yes")),
         height_measured = if_else(is.na(height_1983_m), "No", if_else(is_dead == 1, "Dead", "Yes")),
         lean_estimated = if_else(is.na(stem_lean_amt_scaled_1983), "No", if_else(is_dead == 1, "Dead", "Yes")),
         crown_measured = if_else(is.na(crown_width_EW_1983_m), "No", if_else(is_dead == 1, "Dead", "Yes")),
         bsd_measured = if_else(is.na(BSD_1983_cm), "No", if_else(is_dead == 1, "Dead", "Yes")),
         is_dead = if_else(is_dead == 1, "Dead","Alive"))

all_trees <- ggplot(aes(x = metres_WE, y = metres_SN, col = species, fill = species, alpha = is_dead), data = tree_measurements) +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  #theme(strip.text = element_blank(), strip.background = element_blank()) +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  geom_point(shape = 21, size = 0.8, stroke = 0.1, col = "black") +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  scale_alpha_manual(values = c(0.8, 0.4), guide = "none") +
  facet_wrap(~stand, nrow = 2, ncol = 4) 

dbh_trees <- ggplot(aes(x = metres_WE, y = metres_SN, fill = species), data = tree_measurements %>% filter(dbh_measured == "Yes" & is_dead == "Alive")) +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  #theme(strip.text = element_blank(), strip.background = element_blank()) +
  theme(legend.position = "none") +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  geom_point(alpha = 0.7, size = 0.8, pch = 21, stroke = 0.1, col = "black") +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~stand, nrow = 2, ncol = 4) 

age_trees <- ggplot(aes(x = metres_WE, y = metres_SN, fill = species), data = tree_measurements %>% filter(age_measured == "Yes" & is_dead == "Alive")) +
  geom_point(alpha = 0.7, pch = 21, size = 0.8, stroke = 0.1, col = "black") +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  #theme(strip.text = element_blank(), strip.background = element_blank()) +
  theme(legend.position = "none") +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~stand, nrow = 2, ncol = 4) 

bsd_trees <- ggplot(aes(x = metres_WE, y = metres_SN, fill = species), data = tree_measurements %>% filter(bsd_measured == "Yes" & is_dead == "Alive")) +
  geom_point(alpha = 0.7, pch = 21, size = 0.8, stroke = 0.1, col = "black") +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  #theme(strip.text = element_blank(), strip.background = element_blank()) +
  theme(legend.position = "none") +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~stand, nrow = 2, ncol = 4) 

height_trees <- ggplot(aes(x = metres_WE, y = metres_SN, fill = species), data = tree_measurements %>% filter(height_measured == "Yes" & is_dead == "Alive")) +
  geom_point(alpha = 0.7, size = 0.8, pch = 21, stroke = 0.1, col = "black") +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  #theme(strip.text = element_blank(), strip.background = element_blank()) +
  theme(legend.position = "none") +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~stand, nrow = 2, ncol = 4) 

crown_trees <- ggplot(aes(x = metres_WE, y = metres_SN, fill = species), data = tree_measurements %>% filter(crown_measured == "Yes" & is_dead == "Alive")) +
  geom_point(alpha = 0.7, size = 0.8, pch = 21, stroke = 0.1, col = "black") +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  #theme(strip.text = element_blank(), strip.background = element_blank()) +
  theme(legend.position = "none") +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~stand, nrow = 2, ncol = 4) 
crown_trees

library(patchwork)

mensuration_spatial <- (all_trees + dbh_trees + height_trees) / (age_trees + bsd_trees + crown_trees) + 
  plot_layout(guides = "collect")  + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom",
                                                                               plot.tag = element_text(face= "bold", size = 9))
png("./mensuration_hondo_spatial.png", res = 600, width = 11, height = 7, unit = "in")
mensuration_spatial
dev.off()
