library(taxize)

# taxize for bryoid data

bryoid_spp <- read_csv("./sp_codes_list.csv")

temp <- gnr_resolve(bryoid_spp$sp_name_1980, best_match_only = TRUE, canonical = TRUE)

sp.list <- temp %>% 
  rename(sp_name_1980 = user_supplied_name) %>% 
  full_join(bryoid_spp) %>% 
  select(-2,-3,-4) %>% 
  rename(repaired_name_1980 = matched_name2)
View(sp.list)
sp.list$repaired_name_1980[79] <- "Ptilidium crista-castrensis"
sp.list$repaired_name_1980[4] <- "Bryum pseudotriquetum"

sp.list <- sp.list %>% 
  mutate(current_name = str_replace_all(repaired_name_1980, c("Cladina" = "Cladonia",
                                                   "Cetraria pinastri" = "Vulpicida pinastri",
                                                   "Cladonia gonecha" = "Cladonia sulphurina",
                                                   "Cladonia capitata" = "Cladonia peziziformis",
                                                   "Amblystegium juratzkanum" = "Amblystegium serpens",
                                                   "Drepanocladus revolvens" = "Scorpidium revolvens",
                                                   "Bacidia" = "Biatora",
                                                   "Orthodicranum" = "Dicranum",
                                                   "Helodium" = "Elodium",
                                                   "Pylaisiella" = "Pylaisia"))) 


sp.list.bryoids <- sp.list %>% filter(group == "b") %>% select(-group, -notes) %>%
  separate(current_name, into = c("genus", "species"), sep = " ") 

sp.list.bryoids.fill <- sp.list.bryoids %>% 
  select(genus) %>% 
  unique() %>% 
  add_column(kingdom = NA, order = NA, family = NA, class = NA)

for (i in 1:length(sp.list.bryoids.fill$genus)){
  if (sp.list.bryoids.fill$genus[i] != "Marchantiophyta"){
    output <- tax_name(sp.list.bryoids.fill$genus[i], get = c("kingdom", "division", "family","class","order"), db = "itis")
    sp.list.bryoids.fill$kingdom[i] = as.character(output[[3]])
    sp.list.bryoids.fill$division[i] = as.character(output[[4]])
    sp.list.bryoids.fill$class[i] = as.character(output[[5]])
    sp.list.bryoids.fill$order[i] = as.character(output[[6]])
    sp.list.bryoids.fill$family[i] = as.character(output[[7]])
  }
  else {
    sp.list.bryoids.fill$kingdom[i] = "Plantae"
    sp.list.bryoids.fill$division[i] = "Marchantiophyta"
  }
}
  

all.sp.codes.taxonomy <- sp.list.bryoids.fill %>% full_join(sp.list.bryoids) %>%  full_join(bryoid_spp) %>% 
  unite(accepted_name, c(genus, species), sep = " ", remove = FALSE) %>% 
  relocate(9, 8, 1, 11, 12, 3, 6, 4, 5, 2, 10, 7) %>% 
  arrange(group, code) %>% 
  mutate(group = str_replace_all(group, c("v" = "vascular", "b" = "bryoid")),
         accepted_name = str_remove_all(accepted_name, " NA")) %>% 
  filter(group == "bryoid") %>% 
  select(-group)

all.sp.codes.taxonomy$accepted_name[all.sp.codes.taxonomy$accepted_name == "NA NA"] <- NA  
all.sp.codes.taxonomy[all.sp.codes.taxonomy == "NULL"] <- NA

write_csv(all.sp.codes.taxonomy, "./bryoid_codes_roughdraft.csv")
# then write out unified codes -> final csv
