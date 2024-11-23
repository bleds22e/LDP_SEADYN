### AOS BRYOID DATA: Reading & joining data ###
## Files that cannot be batch-read (earlier years data) ##
## AVH June 2021 ##

# load packages
library(tidyverse)
library(taxize)
library(lubridate)

stand_info <- read_csv("./AOS/StandInfo/AOS_StandInformation.csv")

# 1: 1981, 5 x 5 m plots

# This one is easy to read in:

con <- file("./AOS/BryoidCover/raw_data/txt_files/earlier_years/aos81.b.st25.txt")

open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

close(con)

#establish the dimensions of each row - each column is three digits long
# and each line string ought to be 78 characters long
start_boundary <- seq(from = 1, to = 76, by = 3)
end_boundary <- seq(from = 3, to = 78, by = 3)

df <- data.frame()
quad.info <- as.data.frame(matrix(ncol = 3))
colnames(quad.info) <- c("quad","year","month")

for (line in 1:length(results_list)){ # for each line of the file
  split_line <- c()
  for (trim_pos in 2:length(start_boundary)){ # separate cells
    cover.value <- as.numeric(substr(results_list[line], start_boundary[trim_pos], end_boundary[trim_pos]))
    split_line <- c(split_line, cover.value)
  }
  df <- rbind(df, split_line)
  quad <- substr(results_list[[line]], 1, 2)
  year <- 1981
  month <- 8
  quad.info[line,] <- c(quad,year,month) # piece together the metadata
}

join.lines <- seq(from = 1, to = length(df[,1]), by = 3) 
# each quadrat survey split over three lines -- define lines to join 

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
  df2 <- rbind(df2, full.line) # assemble unified lines for each quadrat and join together into df
}

colnames(df2) <- seq(from = 1, to = 75, by =1)

# read in 1981 bryoid metadata

con <- file("./AOS/BryoidCover/raw_data/txt_files/earlier_years/aos81.b.sx25.txt")

open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

close(con)

##
sp.lines <- seq(from = 7, to = 17, by = 1) # contains original taxonomic codes
sp.names <- seq(from = 36, to = 100, by = 1) # contains full spp names
subfile.lines <- seq(from = 114, to = 117, by = 1) # contains date info

sp.string <- c() # get species codes
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- toupper(unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " ")))

bc_1981 <- df2 %>% select(1:65)
colnames(bc_1981) <- col.names

file_string <- c()
for (line in subfile.lines){ # associate dates with file names
  file_string <- paste(file_string, results_list[[line]])
}

file_string2 <- unlist(str_split(str_trim(file_string, side = "both"), " "))
file_string2 <- file_string2[file_string2 != ""]
file_string3 <- substr(file_string2, 6, 7)

stand <- as.data.frame(rep(file_string3, each = 15)) %>% rename(stand = 1) 

quad.subset <- seq(from = 1, to = 720, by = 3)
quad.sub <- quad.info[quad.subset,]
quad.info <- cbind(quad.sub, stand)
bc_1981x <- cbind(quad.info, bc_1981) %>% mutate(quad_size = 25)

name_string <- c() # get scientific names -- note that this list is eventually integrated into
# the "BryoidSpList.csv" file
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))
name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 130, by = 2)
sp.sci.names <- seq(from = 2, to = 130, by = 2)

sp.code.1981 <- name_string2[sp.codes]
sci.name.1981 <- name_string2[sp.sci.names]

taxonomy.1981 <- as.data.frame(cbind(sp.code.1981, sci.name.1981))

# write_csv(taxonomy.1981, "./AOS/BryoidCover/metadata/sp_codes_1981.csv")
# write_csv(bc_1981x, "./AOS/BryoidCover/raw_data/csv_files/AOS_bryoid_1981.csv")

# 2: 1982 25 m2 plot bryoid data: has implicit zeroes

con <- file("./AOS/BryoidCover/raw_data/txt_files/earlier_years/aos82.b.st25.txt")

open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

close(con)

results_list <- results_list[11:730] # remove lines of metadata at the beginning & end

# establish the dimensions of each row - each column is three digits long
# and each line string ought to be 78 characters long
start_boundary <- seq(from = 1, to = 76, by = 3)
end_boundary <- seq(from = 3, to = 78, by = 3)
quad.col <- c()
df <- data.frame()

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
  df <- rbind(df, split_line) # bind together this new split line with the empty cover data frame
  quad.col[line] <- substr(line.lengthened, 1, 3) # and strip the quadrat from the original line string
}

# the survey data for each quadrat is split across 3 lines - need to join these all together

join.lines <- seq(from = 1, to = nrow(df), by = 3) # create a vector for the first line associated with each quadrat

df2 <- data.frame() # and a blank dataframe to put the joined lines for each quadrat into

for (quads in 1:length(join.lines)){ # for each quadrat
  if (quads < length(join.lines)){ # for all but the last quadrat
    start.quad <- join.lines[quads] # the starting line is the one indexed by quads
    end.quad <- join.lines[quads + 1] -1 # the ending line is before the next index value
  }
  else if (quads == length(join.lines)){ # but if it's the final set of quadrat survey values
    start.quad <- max(join.lines) # then the start line is the last value of the sequence vector generated
    end.quad <- nrow(df) # and the end line is the final line of the data frame
  }
  lines.to.join <- seq(from = start.quad, to = end.quad, by = 1) # now find the lines to join together
  full.line <- c() # start an empty vector for the full line
  for (line in start.quad:end.quad){
    full.line <- c(full.line, unlist(df[line,])) # and join these all together
  }
  df2 <- rbind(df2, full.line) # add to the dataframe
}

quad <- substr(quad.col[join.lines], 1, 2) # extract the quadrat info (leave out the line number)

bc_1982 <- df2 # some empty columns present at the end, so retain only columns with actual data in them to match colnames

View(bc_1982)

# read in metadata

con <- file("./AOS/BryoidCover/raw_data/txt_files/earlier_years/aos82.b.sp25.txt")

open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

close(con)

##
sp.lines <- seq(from = 7, to = 16, by = 1)
sp.names <- seq(from = 129, to = 190, by = 1)
subfile.lines <- seq(from = 18, to = 22, by = 1)

sp.string <- c() # get species codes
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- toupper(unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " ")))

bc_1982 <- df2 %>% select(1:62)
colnames(bc_1982) <- col.names

file_string <- c()
for (line in subfile.lines){
  file_string <- paste(file_string, results_list[[line]])
}

file_string2 <- unlist(str_split(str_remove(str_trim(file_string, side = "both"), fixed("SUBFILE LIST ")), " "))
file_string2 <- file_string2[file_string2 != "" & file_string2 != "(15)"]
file_string3 <- substr(file_string2, 2, 4)

for (q in 1:length(file_string3)){
  if (substr(file_string3[q], 1,1) == "0"){
    char = nchar(file_string3[q])
    file_string3[q] = substr(file_string3[q], 2, char)
  }
}

stand <- as.data.frame(rep(file_string3, each = 15)) %>% rename(stand_number = 1) %>% 
  left_join(stand_info) %>% select(stand_code)

quad.info <- cbind(quad, stand) %>%  mutate(quad_size = 25, year = 1982)

bc_1982x <- cbind(quad.info, bc_1982) %>% rename(stand = stand_code) %>% mutate(month = 8) %>% 
  relocate(month, .after = year)

name_string <- c() # get scientific names
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[/]"))
name_string2 <- name_string2[name_string2 != ""]

taxonomy.1982 <- as.data.frame(name_string2) %>% 
  separate(1, into = c("sp.code.1982", "genus", "species"), sep = " ") %>% 
  unite(sci.name.1980, c(genus, species), sep = " ")

# write_csv(taxonomy.1982, "./AOS/BryoidCover/metadata/sp_codes_1982.csv")
# write_csv(bc_1982x, "./AOS/BryoidCover/raw_data/csv_files/AOS_bryoid_1982.csv")

# 3: 1983 25 m2 bryoid data

con <- file("./AOS/BryoidCover/raw_data/txt_files/earlier_years/aos83.b.st25.txt")

open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

close(con)

results_list <- results_list[13:867]

#establish the dimensions of each row - each column is three digits long
# and each line string ought to be 78 characters long
start_boundary <- seq(from = 1, to = 76, by = 3)
end_boundary <- seq(from = 3, to = 78, by = 3)

df <- data.frame()
quad.info <- c()

for (line in 1:length(results_list)){
  split_line <- c()
  for (trim_pos in 2:length(start_boundary)){
    cover.value <- as.numeric(substr(results_list[line], start_boundary[trim_pos], end_boundary[trim_pos]))
    split_line <- c(split_line, cover.value)
  }
  df <- rbind(df, split_line)
  quad.info[line] <- substr(results_list[[line]], 1, 2)
}

join.lines <- seq(from = 1, to = nrow(df), by = 3)

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

colnames(df2) <- seq(from = 1, to = 75, by =1)

# read in 1983 bryoid metadata

con <- file("./AOS/BryoidCover/raw_data/txt_files/earlier_years/aos83.b.sx25.txt")

open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

close(con)

##
sp.lines <- seq(from = 7, to = 16, by = 1)
sp.names <- seq(from = 35, to = 97, by = 1)
subfile.lines <- seq(from = 111, to = 114, by = 1)

sp.string <- c() # get species codes
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- toupper(unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " ")))

bc_1983 <- df2 %>% select(1:63)
colnames(bc_1983) <- col.names
quad.info
quad.subset <- seq(from = 1, to = 855, by = 3)
quad.sub <- quad.info[quad.subset]

file_string <- c()
for (line in subfile.lines){
  file_string <- paste(file_string, results_list[[line]])
}

file_string2 <- unlist(str_split(str_trim(file_string, side = "both"), " "))
file_string2 <- file_string2[file_string2 != "" & file_string2 != "(15)"]
file_string3 <- substr(file_string2, 6, 7)
file_string3 <- file_string3[file_string3 != ""]

stand <- as.data.frame(rep(file_string3, each = 15))

quad.info <- cbind(quad.sub, stand) %>% rename(stand = 2) %>% 
  mutate(year = 1983, month = 8, quad_size = 25)
                                                                     
bc_1983x <- cbind(quad.info, bc_1983) %>% rename(quad = quad.sub) 

name_string <- c() # get scientific names
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))
name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 126, by = 2)
sp.sci.names <- seq(from = 2, to = 126, by = 2)

sp.code.1983 <- name_string2[sp.codes]
sci.name.1983 <- name_string2[sp.sci.names]

taxonomy.1983 <- as.data.frame(cbind(sp.code.1983, sci.name.1983))

# write_csv(taxonomy.1983, "./AOS/BryoidCover/metadata/sp_codes_1983.csv")
# write_csv(bc_1983x, "./AOS/BryoidCover/raw_data/csv_files/AOS_bryoid_1983.csv")

# 4: 0.7 x 0.7 m microplots, 1983 & 1984

# list all the file names
file.list <- list.files("./AOS/BryoidCover/raw_data/txt_files/later_years/data")

bc_1983m <- data.frame() # create data frame for 1983 survey data
bc_1984m <- data.frame() # and another for 1984 survey data
all_quads <- as.data.frame(matrix(ncol = 3))
colnames(all_quads) <- c("quad","stand","year")

# define start and end of each numeric field
start_boundary <- seq(from = 1, to = 76, by = 3) 
end_boundary <- seq(from = 3, to = 78, by = 3)

for (file in 1:length(file.list)){
  print(file) # helpful counter for which file is being worked on
  filename = file.list[file] # define the file name
  
  # read in the file line by line
  con <- file(paste("./AOS/BryoidCover/raw_data/txt_files/later_years/data/", filename, sep = ""))
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
      cover.value <- as.numeric(substr(results_list[line], start_boundary[trim_pos], end_boundary[trim_pos]))
      split_line <- c(split_line, cover.value)
    }
    df <- rbind(df, split_line)
    quad <- substr(results_list[[line]], 1, 2) # extract quadrat
    stand <- toupper(substr(filename, 1, 2)) # and stand
    year <- paste("19",substr(filename, 3, 4), sep = "") # and year
    quad.info[line,] <- c(quad,stand,year) # and add this to the quadrat info vector to eventually join to df
  }
  
  all_quads <- all_quads %>% full_join(quad.info)
  
  join.lines <- seq(from = 1, to = length(df[,1]), by = 3) 
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
  
  colnames(df2) <- seq(from = 1, to = 75, by =1) # give columns all uniform names
  
  if (str_detect(filename, "83")){
    bc_1983m <- rbind(bc_1983m, df2)
  }
  else{
    bc_1984m <- rbind(bc_1984m, df2)
  }
}

all_quads2 <- na.omit(all_quads)

subset_rows <- seq(from = 1, to = 5550, by = 3)

all_quads3 <- all_quads2[subset_rows,]  # get the quadrat names

# read in survey dates to associate with survey data
survey_dates <- read_csv("./AOS/BryoidCover/metadata/AOS_BryoidCoverDates.csv") %>% 
  separate(month, into = c("month", "day"), sep = "_") %>% 
  mutate(month = as.numeric(month), day = as.numeric(day), year = as.numeric(year)) %>% 
  arrange(stand, year, month, day)
View(survey_dates)
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

quads_1983 <- all_quads3 %>% filter(year == "1983") %>% mutate(quad_size = 0.5)
quads_1984 <- all_quads3 %>% filter(year == "1984") %>% mutate(quad_size = 0.5)

## extract the metadata to give dfs dates and sp codes

# 1983 metadata

con <- file("./AOS/BryoidCover/raw_data/txt_files/later_years/metadata/aos83.b.sx.5.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.lines <- seq(from = 169, to = 178, by = 1) # lines containing species codes
sp.names <- seq(from = 199, to = 261, by = 1) # lines containing scientific names

sp.string <- c() # get species codes
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " "))

bc_1983_2 <- bc_1983m %>% select(1:63)
colnames(bc_1983_2) <- toupper(col.names)

name_string <- c() # get scientific names
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))

name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 126, by = 2) # the way that the names have been read, sci codes are in odd elements
sp.sci.names <- seq(from = 2, to = 126, by = 2) # and sci names are in even elements

sp.code.1983 <- name_string2[sp.codes]
sci.name.1983 <- name_string2[sp.sci.names]

taxonomy.1983 <- as.data.frame(cbind(sp.code.1983, sci.name.1983)) %>% 
  mutate(sp.code.1983 = str_trim(sp.code.1983, side = "both")) # save the taxonomic equivalence of code and name

# 1984 metadata

con <- file("./AOS/BryoidCover/raw_data/txt_files/later_years/metadata/aos84.b.sx.5.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.lines <- seq(from = 194, to = 203, by = 1)
sp.names <- seq(from = 306, to = 368, by = 1)

sp.string <- c()
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " "))

bc_1984_2 <- bc_1984m %>% select(1:63)
colnames(bc_1984_2) <- toupper(col.names)

name_string <- c() # get scientific names
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))

name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 126, by = 2) # the way that the names have been read, sci codes are in odd elements
sp.sci.names <- seq(from = 2, to = 126, by = 2) # and sci names are in even elements

sp.code.1984 <- name_string2[sp.codes]
sci.name.1984 <- name_string2[sp.sci.names]

taxonomy.1984 <- as.data.frame(cbind(sp.code.1984, sci.name.1984)) %>% 
  mutate(sp.code.1984 = str_trim(sp.code.1984, side = "both")) # save the taxonomic equivalence of code and name

# write_csv(taxonomy.1984, "./AOS/BryoidCover/metadata/sp_codes_1984.csv")

aos_bc_1983 <- cbind(quads_1983, bc_1983_2) # bind together quadrat information (quadrat + date + stand) and cover data
aos_bc_1984 <- cbind(quads_1984, bc_1984_2)

# write_csv(aos_bc_1983, "./AOS/BryoidCover/raw_data/csv_files/AOS_bryoid_1983.csv")
# write_csv(aos_bc_1984, "./AOS/BryoidCover/raw_data/csv_files/AOS_bryoid_1984.csv")

# now need to put together all of these dataframes

file.list <- list.files("./AOS/BryoidCover/metadata", pattern = "sp_codes")

all_taxa <-data.frame() # create an empty dataframe for storing all taxonomic information

for (file in 1:length(file.list)){
  df <- read_csv(paste("./AOS/BryoidCover/metadata/", file.list[file], sep = ""), col_names = F)
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
  mutate(correct.sp.code = str_replace_all(sp.code, c("ORT" = "DIC", "SPP" = "SP")),
  correct.sp.name = str_replace_all(sp.name, c("Cladonis" =
                                                 "Cladonia", "Cladina" = "Cladonia",
                                               "Orthodicranum" = "Dicranum",
                                               "flagillare" =
                                                 "flagellare",
                                               "Tomenthypnum" = "Tomentypnum", "spp." = "sp."))) %>% 
  select(-sp.name) %>% 
  unique() # and re-isolate unique taxon code

corrected_taxa <- corrected_taxa[-grep("SP.CODE", corrected_taxa$sp.code),] # remove "SP.CODE..." artifacts

sp.list.bryoids <- corrected_taxa %>% 
  separate(correct.sp.name, into = c("genus", "species"), sep = " ") 

sp.list.bryoids.fill <- sp.list.bryoids %>% 
  select(genus) %>% 
  unique() %>% 
  add_column(kingdom = NA, order = NA, family = NA, class = NA)

#for (i in 1:length(sp.list.bryoids.fill$genus)){
  #if (sp.list.bryoids.fill$genus[i] != "Marchantiophyta"){
  #  output <- tax_name(sp.list.bryoids.fill$genus[i], get = c("kingdom", "division", "family","class","order"), db = "itis")
  #  sp.list.bryoids.fill$kingdom[i] = as.character(output[[3]])
  #  sp.list.bryoids.fill$division[i] = as.character(output[[4]])
  #  sp.list.bryoids.fill$class[i] = as.character(output[[5]])
  #  sp.list.bryoids.fill$order[i] = as.character(output[[6]])
  #  sp.list.bryoids.fill$family[i] = as.character(output[[7]])
 # }
 # else {
 #   sp.list.bryoids.fill$kingdom[i] = "Plantae"
 #   sp.list.bryoids.fill$division[i] = "Marchantiophyta"
#  }
#}

sp.list.bryoids.fill <-  sp.list.bryoids.fill %>% 
  full_join(sp.list.bryoids) %>% 
  unite(col = "accepted_name", c(genus, species), sep = " ", remove = FALSE) 

all.sp.codes.taxonomy <- corrected_taxa %>%  mutate(accepted_name = correct.sp.name) %>% 
  full_join(sp.list.bryoids.fill)

# write_csv(all.sp.codes.taxonomy, "./AOS/BryoidCover/metadata/AOS_BryoidSpList.csv")

# finally, to join together the data

file.list <- list.files("./AOS/BryoidCover/raw_data/csv_files", pattern = "*.csv")

all_bc <- data.frame() # create empty dataframe to join everything together

for (file in 1:length(file.list)){
  # read in csv file
  df <- read_csv(paste("./AOS/BryoidCover/raw_data/csv_files/", file.list[file], sep = ""), guess_max = 8000)
  if (file == 1){
    all_bc <- df
  }
  else {
    all_bc <- all_bc %>% full_join(df)
  }
} # join the data

all_bc2 <- all_bc %>%  relocate(stand,year,month,day,quad_size,quad) %>% # isolate cover only and rearrange data before saving
  mutate_at(c(7:50), ~replace_na(., 0)) %>% 
  unite("date", c("year","month","day"), sep = "-", remove = F) %>% 
  mutate(date = ymd(date)) %>% select(-day) %>% relocate(date, .after = day)

# write_csv(all_bc2, "./AOS/BryoidCover/clean_data/AOS_BryoidCover_1981_1984.csv")
