### VASCULAR SURVEYS: READING & CLEANING data PT2 ###
## AVH June 2021 ##

file.list <- list.files("./AOS/VascularSurvey/raw_data/txt_files/later_years/data")
file.list
vc_1983 <- data.frame()
vc_1984 <- data.frame()
all_quads <- as.data.frame(matrix(ncol = 3))
colnames(all_quads) <- c("quad","stand","year")

start_boundary <- seq(from = 1, to = 76, by = 3)
end_boundary <- seq(from = 3, to = 78, by = 3)

for (file in 1:length(file.list)){
  print(file)
  filename = file.list[file]
  
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
  
  for (line in 1:length(results_list)){
    split_line <- c()
    for (trim_pos in 2:length(start_boundary)){
      cover.value <- as.numeric(substr(results_list[line], start_boundary[trim_pos], end_boundary[trim_pos]))
      split_line <- c(split_line, cover.value)
    }
    df <- rbind(df, split_line)
    quad <- substr(results_list[[line]], 1, 2)
    stand <- toupper(substr(filename, 1, 2))
    year <- paste("19",substr(filename, 3, 4), sep = "")
    quad.info[line,] <- c(quad,stand,year)
  }

  all_quads <- all_quads %>% full_join(quad.info)
  
  join.lines <- seq(from = 1, to = length(df[,1]), by = 5)
  
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
  
  colnames(df2) <- seq(from = 1, to = 125, by =1)
  
  if (str_detect(filename, "83")){
    vc_1983 <- rbind(vc_1983, df2)
  }
  else{
    vc_1984 <- rbind(vc_1984, df2)
  }
}

all_quads2 <- na.omit(all_quads)

subset_rows <- seq(from = 1, to = 16875, by = 5)

all_quads3 <- all_quads2[subset_rows,]

survey_dates <- read_csv("./AOS/VascularSurvey/metadata/vascular_survey_dates.csv") %>% 
  separate(month, into = c("month", "day"), sep = "_") %>% 
  mutate(month = as.numeric(month), day = as.numeric(day)) %>% 
  arrange(stand, year, month, day)

current_quad <- 1

all_quads3$month <- NA
all_quads3$day <- NA

for (line in 1:length(all_quads3$quad)){
  if (line > 1){
    quad = as.numeric(substr(all_quads3$quad[line],1,1))
    pquad = as.numeric(substr(all_quads3$quad[line-1],1,1))
    diff = pquad-quad
    if (diff == 8){
      current_quad <- current_quad + 1
    }
    all_quads3$month[line] <- survey_dates$month[current_quad]
    all_quads3$day[line] <- survey_dates$day[current_quad]
  }
  else{
    all_quads3$month[1] <- survey_dates$month[1]
    all_quads3$day[1] <- survey_dates$day[1]
  }
}

quads_1983 <- all_quads3 %>% filter(year == "1983")
quads_1984 <- all_quads3 %>% filter(year == "1984")

## extract the metadata to give dfs dates and sp codes

#1983

con <- file("./AOS/VascularSurvey/raw_data/txt_files/later_years/metadata/aos83.v.sx25.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.lines <- seq(from = 205, to = 217, by = 1)
sp.names <- seq(from = 248, to =358, by = 1)

sp.string <- c()
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " "))

vc_1983_2 <- vc_1983 %>% select(1:111)
colnames(vc_1983_2) <- toupper(col.names)

name_string <- c()
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))

name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 222, by = 2)
sp.sci.names <- seq(from = 2, to = 222, by = 2)

sp.code.1983 <- name_string2[sp.codes]
sci.name.1983 <- name_string2[sp.sci.names]

taxonomy.1983 <- as.data.frame(cbind(sp.code.1983, sci.name.1983)) %>% 
  mutate(sp.code.1983 = str_trim(sp.code.1983, side = "both"))

#1984

con <- file("./AOS/VascularSurvey/raw_data/txt_files/later_years/metadata/aos84.v.sx25.txt")
open(con)
results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.lines <- seq(from = 213, to = 225, by = 1)
sp.names <- seq(from = 322, to = 432, by = 1)

sp.string <- c()
for (line in sp.lines){
  sp.string <- paste(sp.string, results_list[[line]])
}

col.names <- unlist(str_split(str_squish(str_trim(sp.string, side = "both")), " "))

vc_1984_2 <- vc_1984 %>% select(1:111)
colnames(vc_1984_2) <- toupper(col.names)

aos_vc_1983 <- cbind(quads_1983, vc_1983_2)
aos_vc_1984 <- cbind(quads_1984, vc_1984_2)

#write_csv(aos_vc_1983, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vc_1983.csv")
#write_csv(aos_vc_1984, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vc_1984.csv")


name_string <- c()
for (line in sp.names){
  name_string <- paste(name_string, results_list[[line]])
}

name_string2 <- unlist(str_split(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS ")), "[']"))

name_string2 <- name_string2[name_string2 != ""]

sp.codes <- seq(from = 1, to = 222, by = 2)
sp.sci.names <- seq(from = 2, to = 222, by = 2)

sp.code.1984 <- name_string2[sp.codes]
sci.name.1984 <- name_string2[sp.sci.names]

taxonomy.1984 <- as.data.frame(cbind(sp.code.1984, sci.name.1984)) %>% 
  mutate(sp.code.1984 = str_trim(sp.code.1984, side = "both"))

#write_csv(taxonomy.1983, "./AOS/VascularSurvey/metadata/sp_codes_1983.csv")
#write_csv(taxonomy.1984, "./AOS/VascularSurvey/metadata/sp_codes_1984.csv")

