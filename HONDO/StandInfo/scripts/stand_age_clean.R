## Hondo cleaning stand age data
## AVH May 2022

library(tidyverse)
library(assertr)

stand.age <- read_csv("./Hondo/StandInfo/raw_data/Hondo_StandAge.csv") %>% 
  mutate(DBH_cm = round(dbh_in*2.54,1), age = number_rings + rings_correct) %>% 
  select(-number_rings, -rings_correct, -dbh_in) %>% 
  rename(tree_tag = tag_number)

stand.age %>% verify(substr(quad,1,1) %in% c(0:9))
stand.age %>% verify(substr(quad,2,2) %in% LETTERS[1:10])
stand.age %>% verify(is.integer(tree_tag)) # one tree outside stand
stand.age %>% verify(species_code %in% c("ABBA","BEPA","PIBA","PIMA","PIGL",
                                         "POTR","POBA","LALA"))
stand.age %>% verify(is.integer(age)) # one missing

stand.age <- stand.age %>% 
  rename(height_to_crown_m = stem_height_m) %>% 
  relocate(height_to_crown_m, .after = "DBH_cm") %>% 
  relocate(notes, .after = "age")

write_csv(stand.age, "./Hondo/StandInfo/clean_data/Hondo_StandAge_1982.csv")


letter_to_m <- cbind(LETTERS[1:10], c(0:9)) %>% as.data.frame() %>% rename(row = 1, metres_WE = 2) %>% 
  mutate(metres_WE = as.numeric(metres_WE)*5)
tree_positions <- read_csv("./Hondo/Mensuration/clean_data/Hondo_Mensuration_1983_2010.csv") %>% 
  select(tree_tag, base_coord_W_1983_m, base_coord_S_1983_m)

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

tree_age <- read_csv("./Hondo/StandInfo/clean_data/Hondo_StandAge_1982.csv") %>% 
  separate(quad, into = c("col","row"), 1,1) %>% 
  left_join(letter_to_m) %>% 
  left_join(tree_positions) %>% 
  mutate(metres_SN = as.numeric(col)*5+base_coord_S_1983_m,
         metres_WE = metres_WE + base_coord_W_1983_m) %>% 
  mutate(species = case_when(species_code == "ABBA" ~ "A. balsamea",
                             species_code == "BEPA" ~ "B. papyrifera",
                             species_code == "POTR" ~ "P. tremuloides",
                             species_code == "PIBA" ~ "P. banksiana",
                             species_code == "PIGL" ~ "P. glauca",
                             species_code == "PIMA" ~ "P. marina",
                             species_code == "LALA" ~ "L. laricina",
                             species_code == "POBA" ~ "P. balsamifera")) 

aged_trees <- ggplot(aes(x = metres_WE, y = metres_SN, col = species, fill = species), data = tree_age) +
  labs(x = "W-E position within plot (m)",
       y = "S-N position within plot (m)",
       fill = "Species") +
  grid_theme +
  coord_fixed(xlim = c(0,50), ylim = c(0,50), expand = F, clip="off") +
  scale_x_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_y_continuous(breaks = seq(0, 50, by =10), minor_breaks = seq(5, 45, by =10)) +
  scale_fill_manual(values = c("#74B72E", "#795C34", "#E1AD01", "#AF69ED",
                               "#028A0F",  "#63C5DA", "#1338BE", "#F05E16")) +
  geom_point(shape = 21, size = 1.4, stroke = 0.1, col = "black") +
  guides(fill = guide_legend(override.aes = list(size=4))) +
  facet_wrap(~stand, nrow = 2, ncol = 4) 
aged_trees

png("./aging_hondo_spatial.png", res = 600, width = 8, height = 5, unit = "in")
aged_trees
dev.off()
