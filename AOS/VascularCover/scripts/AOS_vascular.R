### VASCULAR SURVEYS: reading data ###
## AVH June 2021 ##

library(tidyverse)
library(taxize)

# 1: 1982 data - problem: zeroes are implicit

# read in file line by line
con <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos82.v.st25.txt")
open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

# establish the dimensions of each row - each column is three digits long
# and each line string ought to be 78 characters long
start_boundary <- seq(from = 1, to = 76, by = 3)
end_boundary <- seq(from = 3, to = 78, by = 3)
quad.col <- c()
vc_df <- data.frame()

# need to get from single-string lines to 26-col vectors for each line
for (line in 1:length(results_list)){
  line.length <- nchar(results_list[[line]]) # what's the length of the line?
  if (line.length == 79){ # if there is an extra space at the end of the line,
  current.line <- substr(results_list[[line]], 1, 78) # remove that space
  line.length <- nchar(current.line) 
  }
  ws_add <- 78 - line.length # need to add a chunk of explicit whitespace to shorter lines
  if (ws_add > 0){
    ws_chunk <- str_flatten(rep(" ", times = ws_add))
    line.lengthened <- paste(results_list[[line]], ws_chunk)
  }
  else if (ws_add == 0){
    line.lengthened <- current.line
  }
  split_line <- c() # create an empty vector to put in the split string
  for (trim_pos in 2:length(start_boundary)){
    cover.value <- as.numeric(substr(line.lengthened, start_boundary[trim_pos], end_boundary[trim_pos]))
    # go along each start & end vector to carve out each three-character value for each column
    split_line <- c(split_line, cover.value) # and then add that value to the existing vector
  }
  vc_df <- rbind(vc_df, split_line) # bind together this new split line with the empty cover data frame
  quad.col[line] <- substr(line.lengthened, 1, 3) # and strip the quadrat from the original line string
}

# the survey data for each quadrat is split across 5 lines - need to join these all together

join.lines <- seq(from = 1, to = length(vc_df$X3), by = 5) # create a vector for the first line associated with each quadrat

vc_df2 <- data.frame() # and a blank dataframe to put the joined lines for each quadrat into

for (quads in 1:length(join.lines)){ # for each quadrat
  if (quads < length(join.lines)){ # for all but the last quadrat
    start.quad <- join.lines[quads] # the starting line is the one indexed by quads
    end.quad <- join.lines[quads + 1] -1 # the ending line is before the next index value
  }
  else if (quads == length(join.lines)){ # but if it's the final set of quadrat survey values
    start.quad <- max(join.lines) # then the start line is the last value of the sequence vector generated
    end.quad <- length(vc_df$X3) # and the end line is the final line of the data frame
  }
  lines.to.join <- seq(from = start.quad, to = end.quad, by = 1) # now find the lines to join together
  full.line <- c() # start an empty vector for the full line
  for (line in start.quad:end.quad){
    full.line <- c(full.line, unlist(vc_df[line,])) # and join these all together
  }
  vc_df2 <- rbind(vc_df2, full.line) # add to the dataframe
}

quad <- substr(quad.col[join.lines], 1, 2) # extract the quadrat info (leave out the line number)
quad <- quad[1:450] # for some reason, an NA row was added at the end, so leave that out

vc_1982 <- cbind(quad, vc_df2) %>% select(1:106) # some empty columns present at the end, so retain only columns with actual data in them to match colnames

# get the metadata to add the column names!

# read in the metadata
con <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos82.v.sp25.txt")

open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

# find the lines that correspond to:
sp.code.lines <- seq(from = 7, to = 19, by = 1) # the species codes for each column
subfile.lines <- seq(from = 25, to = 29, by = 1) # the stand information for each segment of the dataframe
sp.names <- seq(from = 170, to = 274, by = 1) # the scientific names the species codes correspond to

# read the above three pieces of information into vectors
sp_string <- c()
for (line in sp.code.lines){
  sp_string <- paste(sp_string, results_list[[line]])
}

file_string <- c()
for (line in subfile.lines){
  file_string <- paste(file_string, results_list[[line]])
}

name_string <- c()
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

# first, the species codes
# get all the species codes into a vector
sp_string2 <- toupper(unlist(str_split(str_squish(str_trim(sp_string, side = "both")), " ")))

# create a vector of column names from these species codes to apply to the dataframe
col.names <- c("quad",sp_string2)
colnames(vc_1982) <- col.names

# second, the species names. get all the species codes and species names into vector format
name_string2 <- unlist(str_extract_all(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS")), ".*?/"))

# get each pair of sp.code + name into separate elements of a vector
for (pair in 1:length(name_string2)){
  name_string2[pair] <- str_trim(str_remove_all(name_string2[pair], fixed(" /")), side = "both")
}

# create a dataframe by separating codes and names
taxonomy.1982 <- as.data.frame(name_string2) %>% 
  separate(1, into = c("sp.code.1982", "genus", "species"), sep = " ") %>% 
  unite(sci.name.1980, c(genus, species), sep = " ")

# write this metadata to a csv file
#write_csv(taxonomy.1982, "./AOS/VascularSurvey/metadata/sp_codes_1982.csv")

# third, deal with the subfiles - all stands are present in the single dataframe,
# but we need to sort out in what order and assign a column to represent this
file_string2 <- unlist(str_split(str_squish(str_trim(str_remove_all(str_remove_all(str_remove_all(file_string, c(fixed("25"))), fixed("SUBFILE LIST")), "[S()]"), side = "both")), " "))

# some files have leading zeroes, so this removes those to create a vector of 
# subfile names that is an integer + sometimes B/N (burned or new)
for (q in 1:length(file_string2)){
  if (substr(file_string2[q], 1,1) == "0"){
    char = nchar(file_string2[q])
    file_string2[q] = substr(file_string2[q], 2, char)
  }
}

# repeat each stand code 25 times to make stand column
stand <- as.data.frame(rep(file_string2, each = 25)) %>% rename(vascular_1982 = 1)

# join to coordinates dataframe to get the two-letter code for the stand
stand_info <- read_csv("./AOS/StandInfo/AOS_coordinates.csv")
stand2 <- stand %>% left_join(stand_info) %>% select(plot_code) %>% rename(stand = plot_code)

# all stands surveyed in August (month 8) of 1982, so make cols to this effect
vc_1982$month <- 8
vc_1982$year <- 1982

vc_1982 <- cbind(stand2, vc_1982) 
vc_1982x <- vc_1982
vc_1982x[is.na(vc_1982x)] <- 0 # put in explicit zeroes where NA values present

# save data to csv file
#write_csv(vc_1982x, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vc_1982.csv")

# 2: 1981 data

# read in file line by line
con <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos81.v.st25.txt")

open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

# as with 1982 data, create a blank df to get from character string to dataframe
vc_df_1981 <- data.frame()
quad.col <- c()

# code is less complicated because there are explicit zeroes
# still two loops present: 1) get data from string to vector, 2) join rows for each quadrat survey
for (line in 1:length(results_list)){
  split_line <- c()
  for (trim_pos in 2:length(start_boundary)){
    cover.value <- as.numeric(substr(results_list[[line]], start_boundary[trim_pos], end_boundary[trim_pos]))
    split_line <- c(split_line, cover.value)
  }
  vc_df_1981 <- rbind(vc_df_1981, split_line)
  quad.col[line] <- substr(results_list[[line]], 1, 3)
}

join.lines <- seq(from = 1, to = length(vc_df_1981[,1]), by = 5)

vc_df2_1981 <- data.frame()

for (quads in 1:length(join.lines)){
  if (quads < length(join.lines)){
    start.quad <- join.lines[quads]
    end.quad <- join.lines[quads + 1] -1
  }
  else if (quads == length(join.lines)){
    start.quad <- max(join.lines)
    end.quad <- length(vc_df_1981[,1])
  }
  lines.to.join <- seq(from = start.quad, to = end.quad, by = 1)
  full.line <- c()
  for (line in start.quad:end.quad){
    full.line <- c(full.line, unlist(vc_df_1981[line,]))
  }
  vc_df2_1981 <- rbind(vc_df2_1981, full.line)
}

quad <- substr(quad.col[join.lines], 1, 2)

vc_1981 <- cbind(quad, vc_df2_1981) %>% select(1:114)

# join with the metadata

con <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos81.v.sx25.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.code.lines <- seq(from = 9, to = 21, by = 1)
subfile.lines <- seq(from = 176, to = 178, by = 1)
sp.names <- seq(from = 50, to = 160, by = 1)

sp_string <- c()
for (line in sp.code.lines){
  sp_string <- paste(sp_string, results_list[[line]])
}

file_string <- c()
for (line in subfile.lines){
  file_string <- paste(file_string, results_list[[line]])
}

name_string <- c()
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

# get the column names sorted
sp_string2 <- toupper(unlist(str_split(str_squish(str_trim(sp_string, side = "both")), " ")))
col.names <- c("quad",sp_string2)
colnames(vc_1981) <- col.names

# get the codes for each site from metadata file (2-letter code)
file_string2 <- unlist(str_split(str_trim(file_string, side = "both"), " "))
file_string3 <- substr(file_string2[file_string2 != ""], 6,7)

stand <- rep(file_string3, each = 25)

# create month and year columns
vc_1981$month <- 8
vc_1981$year <- 1981

vc_1981 <- cbind(stand, vc_1981)

#write_csv(vc_1981, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vc_1981.csv")

# get species code + scientific names to get together
name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))

name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 222, by = 2)
sp.sci.names <- seq(from = 2, to = 222, by = 2)

sp.code.1981 <- name_string2[sp.codes]
sci.name.1981 <- name_string2[sp.sci.names]

taxonomy.1981 <- as.data.frame(cbind(sp.code.1981, sci.name.1981))

#write_csv(taxonomy.1981, "./AOS/VascularSurvey/metadata/sp_codes_1981.csv")
# filled in file manually where there were codes >4 characters (x 6)

# 3: 1983 & 1984 data

# list all the file names
file.list <- list.files("./AOS/VascularSurvey/raw_data/txt_files/later_years/data")

vc_1983 <- data.frame() # create data frame for 1983 survey data
vc_1984 <- data.frame() # and another for 1984 survey data
all_quads <- as.data.frame(matrix(ncol = 3))
colnames(all_quads) <- c("quad","stand","year")

# define start and end of each numeric field
start_boundary <- seq(from = 1, to = 76, by = 3) 
end_boundary <- seq(from = 3, to = 78, by = 3)

for (file in 1:length(file.list)){
  print(file) # helpful counter for which file is being worked on
  filename = file.list[file] # define the file name
  
  # read in the file line by line
  con <- file(paste("./AOS/VascularSurvey/raw_data/txt_files/later_years/data/", filename, sep = ""))
  open(con)
  results_list <- list()
  current_line <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
    results_list[current_line] <- line
    current_line <- current_line + 1
  } 
  close(con)
  
  df <- data.frame()
  quad.info <- as.data.frame(matrix(ncol = 3))
  colnames(quad.info) <- c("quad","stand","year")
  
  # for each string in the file (line of original .txt file)
  for (line in 1:length(results_list)){
    split_line <- c() # same as with 1981, 1982 data ... need to get string to vector
    for (trim_pos in 2:length(start_boundary)){
      cover.value <- str_trim(substr(results_list[line], start_boundary[trim_pos], end_boundary[trim_pos]), side = "both")
      if (cover.value == "+"){  # fix where trace amts represented by +/r/.
        cover.value <- "0.5"
      }
      if (cover.value == "r"){
        cover.value <- "0.1"
      }
      if (cover.value == "."){
        cover.value <- "0.01"
      }
      split_line <- c(split_line, as.numeric(cover.value))
    }
    df <- rbind(df, split_line)
    quad <- substr(results_list[[line]], 1, 2) # extract quadrat
    stand <- toupper(substr(filename, 1, 2)) # and stand
    year <- paste("19",substr(filename, 3, 4), sep = "") # and year
    quad.info[line,] <- c(quad,stand,year) # and add this to the quadrat info vector to eventually join to df
  }


  all_quads <- all_quads %>% full_join(quad.info)
  
  join.lines <- seq(from = 1, to = length(df[,1]), by = 5) 
  # each quadrat split over 5 lines. define these groups, and join together
  
  df2 <- data.frame()
  
  for (quads in 1:length(join.lines)){ 
    if (quads < length(join.lines)){
      start.quad <- join.lines[quads]
      end.quad <- join.lines[quads + 1] -1
    }
    else if (quads == length(join.lines)){
      start.quad <- max(join.lines)
      end.quad <- length(df[,1])
    }
    lines.to.join <- seq(from = start.quad, to = end.quad, by = 1)
    full.line <- c()
    for (line in start.quad:end.quad){
      full.line <- c(full.line, unlist(df[line,]))
    }
    df2 <- rbind(df2, full.line)
  }
  
  colnames(df2) <- seq(from = 1, to = 125, by =1) # give columns all uniform names
  
  if (str_detect(filename, "83")){
    vc_1983 <- rbind(vc_1983, df2)
  }
  else{
    vc_1984 <- rbind(vc_1984, df2)
  }
}

all_quads2 <- na.omit(all_quads)

subset_rows <- seq(from = 1, to = 16875, by = 5)

all_quads3 <- all_quads2[subset_rows,] # get the quadrat names

# read in survey dates to associate with survey data
survey_dates <- read_csv("./AOS/VascularSurvey/metadata/AOS_VascularSurveyDates.csv") %>% 
  separate(month, into = c("month", "day"), sep = "_") %>% 
  mutate(month = as.numeric(month), day = as.numeric(day)) %>% 
  arrange(stand, year, month, day)

all_quads3$month <- NA
all_quads3$day <- NA
current_survey <- 1

for (line in 1:length(all_quads3$quad)){
  if (line > 1){
    quad = as.numeric(substr(all_quads3$quad[line],1,1)) # get the current quadrat number
    pquad = as.numeric(substr(all_quads3$quad[line-1],1,1)) # get the previous quadrat number
    diff = pquad-quad # get the difference between those numbers
    if (diff == 8){ # df is structured such that changes between surveys is signalled by a drop of 8 in that number
      current_survey <- current_survey + 1
    }
    all_quads3$month[line] <- survey_dates$month[current_survey]
    all_quads3$day[line] <- survey_dates$day[current_survey]
  }
  else{
    all_quads3$month[1] <- survey_dates$month[1]
    all_quads3$day[1] <- survey_dates$day[1]
  }
}

quads_1983 <- all_quads3 %>% filter(year == "1983")
quads_1984 <- all_quads3 %>% filter(year == "1984")

## extract the metadata to give dfs dates and sp codes

# 1983 metadata

con <- file("./AOS/VascularSurvey/raw_data/txt_files/later_years/metadata/aos83.v.sx25.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.lines <- seq(from = 205, to = 217, by = 1) # lines containing species codes
sp.names <- seq(from = 248, to =358, by = 1) # lines containing scientific names

sp.string <- c() # get species codes
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}
sp.string
col.names <- unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " "))
col.names <- col.names[col.names != "STLO2"]


vc_1983_2 <- vc_1983 %>% select(1:113) %>% select(-67, -77, -109)

colnames(vc_1983_2) <- toupper(col.names)

name_string <- c() # get scientific names
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))

name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 222, by = 2) # the way that the names have been read, sci codes are in odd elements
sp.sci.names <- seq(from = 2, to = 222, by = 2) # and sci names are in even elements

sp.code.1983 <- name_string2[sp.codes]
sci.name.1983 <- name_string2[sp.sci.names]

taxonomy.1983 <- as.data.frame(cbind(sp.code.1983, sci.name.1983)) %>% 
  mutate(sp.code.1983 = str_trim(sp.code.1983, side = "both")) # save the taxonomic equivalence of code and name

# 1984 metadata = same as 1983 metadata

vc_1984_2 <- vc_1984 %>% select(1:113) %>% select(-67,-77,-109)
colnames(vc_1984_2) <- toupper(col.names)

aos_vc_1983 <- cbind(quads_1983, vc_1983_2) # bind together quadrat information (quadrat + date + stand) and cover data
aos_vc_1984 <- cbind(quads_1984, vc_1984_2)

#write_csv(aos_vc_1983, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vc_1983.csv")
#write_csv(aos_vc_1984, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vc_1984.csv")

#write_csv(taxonomy.1983, "./AOS/VascularSurvey/metadata/sp_codes_1983.csv")

# now join data across all years

file.list <- list.files("./AOS/VascularSurvey/metadata", pattern = "sp_codes")

all_taxa <-data.frame() # create an empty dataframe for storing all taxonomic information

for (file in 1:length(file.list)){
  df <- read_csv(paste("./AOS/VascularSurvey/metadata/", file.list[file], sep = ""), col_names = F)
  colnames(df) <- c("sp.code","sp.name")
  if (file == 1){
    all_metadata <- df
  }
  else {
    all_metadata <- all_metadata %>% full_join(df)
  }
} # should join together all species codes files

# isolate unique combinations of species codes and species names
unique_taxa <- all_metadata %>% 
  mutate(sp.code = toupper(sp.code)) %>% 
  unique() %>% arrange(sp.name)

# replace incorrect species codes with ones that make more sense
# and any misspelled species names
corrected_taxa <- unique_taxa %>% 
  mutate(correct.sp.code = str_replace_all(sp.code, c("DRASP" = "DRSP",
                                                      "LOCA" = "LOVI",
                                                      "RACE" = "RASP",
                                                      "SALSP" = "SASP",
                                                      "SESE" = "SESP",
                                                      "STLO2" = "STLO",
                                                      "LEGR" = "RHGR",
                                                      "ARLY" = "BOLY",
                                                      "ASCI" = "SYCI",
                                                      "ASLA" = "SYLA",
                                                      "CAIN" = "CAST",
                                                      "COPA" = "COUM",
                                                      "ELIN" = "LEIN",
                                                      "EPAN" = "CHAN",
                                                      "ORPU" = "PIPU",
                                                      "PEPA" = "PEFR",
                                                      "POTRI" = "SITR",
                                                      "PYVI" = "PYCH",
                                                      "PYSE" = "ORSE",
                                                      "RUAC" = "RUAR",
                                                      "RUST" = "RUSA",
                                                      "SEST" = "PAST",
                                                      "SEIN" = "PAST",
                                                      "SODE" = "SOSI",
                                                      "VACA" = "VACE")),
  accepted_name = str_replace_all(sp.name, c("Sheperdia canadensis" =
                                                 "Shepherdia canadensis",
                                               "Stellaria longipes--duplicate" =
                                                 "Stellaria longipes",
                                               "Ledum" = "Rhododendron",
                                               "Labiatae" = "Lamiaceae",
                                               "Arabis" = "Boechera",
                                               "Aster" = "Symphyotrichum",
                                               "inexpansa" = "stricta",
                                               "pallida" = "umbellata",
                                               "Elymus innovatus" = "Leymus innovatus",
                                               "Epilobium" = "Chamerion",
                                               "Oryzopsis pungens" = "Piptatherum pungens",
                                               "palmatus" = "frigidus",
                                               "Potentilla" = "Sibbaldiopsis",
                                               "Pyrola secunda" = "Orthilia secunda",
                                               "Pyrola virens" = "Pyrola chlorantha",
                                               "acaulis" = "arcticus",
                                               "strigosus" = "sachalinensis",
                                               "Senecio streptanthifolius" = "Packera streptanthifolia",
                                               "Senecio cymbalarioides" = "Packera streptanthifolia",
                                               "decumbens" = "simplex",
                                               "caespitosum" = "cespitosum"))) %>% 
  unique() %>% 
  mutate(repaired_name_1980 = sp.name) %>% 
  mutate(code = sp.code, unified_code = correct.sp.code) %>% 
  select(code, unified_code, accepted_name) # and re-isolate unique taxon codes

corrected_taxa <- corrected_taxa[-grep("SP.CODE", corrected_taxa$code),] # remove "SP.CODE..." artifacts
corrected_taxa <- corrected_taxa[-c(117,118),] # remove temperature since not taxonomic

sp.list.vasc <- corrected_taxa %>% 
  separate(accepted_name, into = c("genus", "species"), sep = " ") 

sp.list.vasc.fill <- sp.list.vasc %>% 
  select(genus) %>% 
  unique() %>% 
  add_column(kingdom = NA, order = NA, family = NA, class = NA)

for (i in 1:length(sp.list.vasc.fill$genus)){
    output <- tax_name(sp.list.vasc.fill$genus[i], get = c("kingdom", "division", "family","class","order"), db = "itis")
    sp.list.vasc.fill$kingdom[i] = as.character(output[[3]])
    sp.list.vasc.fill$division[i] = as.character(output[[4]])
    sp.list.vasc.fill$class[i] = as.character(output[[5]])
    sp.list.vasc.fill$order[i] = as.character(output[[6]])
    sp.list.vasc.fill$family[i] = as.character(output[[7]])
  }

all.sp.codes.taxonomy <-  sp.list.vasc.fill %>% 
  full_join(sp.list.vasc) %>% 
  unite(col = "accepted_name", c(genus, species), sep = " ", remove = FALSE) %>% unique()

#write.csv(all.sp.codes.taxonomy, "./AOS/VascularSurvey/metadata/AOS_VascularSpList.csv")

# finally, to join together the data

corrected_taxa <- read_csv("./AOS/VascularCover/metadata/AOS_VascularSpList.csv") %>% select(-1)
file.list <- list.files("./AOS/VascularCover/raw_data/csv_files", pattern = "*.csv")

all_vc <- data.frame() # create empty dataframe to join everything together

for (file in 1:length(file.list)){
  # read in csv file
  df <- read_csv(paste("./AOS/VascularCover/raw_data/csv_files/", file.list[file], sep = ""))
  # need to first sum duplicate columns
  code <- as.data.frame(colnames(df)) %>% rename(code = 1) # create data frame of column names
  join <- code %>% left_join(corrected_taxa) %>% # join this column with the taxonomic data to rename incorrect codes with correct version
    mutate(sp.code = if_else(is.na(unified_code), code, unified_code))
  colnames(df) <- join$sp.code # and rename the dataframe columns

  dupe_cols <- colnames(df[,which(duplicated(colnames(df)))]) # locate duplicate species codes if present
  dupes <- df[,which(colnames(df) %in% dupe_cols)] # and isolate those columns that need to be summed
  
  sum_col <- data.frame() # create dataframe to hold these sums
  cn <- unique(as.vector(colnames(dupes))) # isolate column name(s) aka sp. codes for these duplicates
  
  if (is_empty(cn) == F){
    for (dc in 1:length(cn)){ # for each duplicate code in this vector of codes
      subset <- dupes[colnames(dupes) == cn[dc]] # subset those columns associated with a given code
      for (row in 1:nrow(dupes)){ # and then sum those two columns (never more than that present) to make a unified column
        sum_col[row,dc] <- dupes[row, 1] + dupes[row, 2]
        colnames(sum_col) <- cn[dc] # and name the column with the code
      }
    }

  nodupe_df <- df[,-which(colnames(df) %in% dupe_cols)] # isolate non-duplicate codes
  
  df <- cbind(nodupe_df, sum_col) # and join with the summed, unified column for any duplicates
  
  }
  
  if (file == 1){
    all_vc <- df
  }
  else {
    all_vc <- all_vc %>% full_join(df)
  }
} # join the data

all_vc2 <- all_vc %>% unite("date",c("year","month","day"), sep = "-", remove = F) %>% 
  mutate(date = ymd(date))

temperature <- all_vc2 %>% select(stand,year,month,date, quad,TEMP) %>% #isolate soil temperature
  mutate(temp_C = (TEMP-32)*5/9) %>% filter(year != "1981") %>%  # 1981 temperatures always incorrect
  filter((stand %in% c("RY","RO")) == F) %>% select(-TEMP) # RY and RO not sampled extensively since burned

cover_only <- all_vc2 %>% select(-TEMP) %>% select(-day) %>% relocate(stand,year,month,date,quad) %>%  
  filter((stand %in% c("RY","RO")) == F) %>% mutate_at(.vars = 6:50, ~replace_na(., 0))
# isolate cover only and rearrange data before saving, replace na values with 0
# take out RY and RO - these two stands burned and were not sampled much in time series

#write_csv(temperature, "./AOS/VascularCover/clean_data/AOS_ProbeTemp_1982_1984.csv")
#write_csv(cover_only, "./AOS/VascularCover/clean_data/AOS_VascularCover_1981_1984.csv")


library(assertr)

# QC temp data

qc_temp <- read_csv("./AOS/VascularCover/clean_data/AOS_ProbeTemp_1982_1984.csv")
hist(qc_temp$temp_C) # some temp values seem WAY too low (in August) - looking back, the temps measured are 0 F, which is ridiculous.
qc_temp2 <- qc_temp %>% filter(temp_C > 0)
hist(qc_temp2$temp_C) # much more reasonable

qc_temp2 %>% verify(year %in% c(1982,1983,1984)) %>% 
  verify(month %in% 1:12)

levels(as.factor(qc_temp2$stand))
levels(as.factor(qc_temp2$quad))

qc_temp2 <- qc_temp2 %>% arrange(stand,quad,year,month,temp_C)

write_csv(qc_temp2, "./AOS/VascularCover/clean_data/AOS_ProbeTemp_1982_1984.csv")
