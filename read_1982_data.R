### VASCULAR SURVEYS: READING & CLEANING data ###
## AVH June 2021 ##

aos_missingzero <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos82.v.st25.txt")
open(aos_missingzero)

results_list <- list()
current_line <- 1
while (length(line <- readLines(aos_missingzero, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

start_boundary <- seq(from = 1, to = 76, by = 3)
end_boundary <- seq(from = 3, to = 78, by = 3)
quad.col <- c()
vc_df <- data.frame()

for (line in 1:length(results_list)){
  line.length <- nchar(results_list[[line]])
  if (line.length == 79){
  current.line <- substr(results_list[[line]], 1, 78)
  line.length <- nchar(current.line)
  }
  ws_add <- 78 - line.length
  if (ws_add > 0){
    ws_chunk <- str_flatten(rep(" ", times = ws_add))
    line.lengthened <- paste(results_list[[line]], ws_chunk)
  }
  else if (ws_add == 0){
    line.lengthened <- current.line
  }
  split_line <- c()
  for (trim_pos in 2:length(start_boundary)){
    cover.value <- as.numeric(substr(line.lengthened, start_boundary[trim_pos], end_boundary[trim_pos]))
    split_line <- c(split_line, cover.value)
  }
  vc_df <- rbind(vc_df, split_line)
  quad.col[line] <- substr(line.lengthened, 1, 3)
}


join.lines <- seq(from = 1, to = length(vc_df$X3), by = 5)

vc_df2 <- data.frame()

for (quads in 1:length(join.lines)){
  if (quads < length(join.lines)){
    start.quad <- join.lines[quads]
    end.quad <- join.lines[quads + 1] -1
  }
  else if (quads == length(join.lines)){
    start.quad <- max(join.lines)
    end.quad <- length(vc_df$X3)
  }
  lines.to.join <- seq(from = start.quad, to = end.quad, by = 1)
  full.line <- c()
  for (line in start.quad:end.quad){
    full.line <- c(full.line, unlist(vc_df[line,]))
  }
  vc_df2 <- rbind(vc_df2, full.line)
}

quad <- substr(quad.col[join.lines], 1, 2)
quad <- quad[1:450]

vc_1982 <- cbind(quad, vc_df2) %>% select(1:106)

# metadata

vc_metadata_1982 <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos82.v.sp25.txt")
open(vc_metadata_1982)
results_list <- list()
current_line <- 1
while (length(line <- readLines(vc_metadata_1982, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

sp.code.lines <- seq(from = 7, to = 19, by = 1)
subfile.lines <- seq(from = 25, to = 29, by = 1)
sp.names <- seq(from = 170, to = 274, by = 1)

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

sp_string2 <- unlist(str_split(str_squish(str_trim(sp_string, side = "both")), " "))
col.names <- c("quad",sp_string2)
colnames(vc_1982) <- col.names

name_string2 <- unlist(str_extract_all(str_remove(str_squish(str_trim(name_string, side = "both")), fixed("VAR LABELS")), ".*?/"))

for (pair in 1:length(name_string2)){
  name_string2[pair] <- str_trim(str_remove_all(name_string2[pair], fixed(" /")), side = "both")
}

taxonomy.1982 <- as.data.frame(name_string2) %>% 
  separate(1, into = c("sp.code.1982", "genus", "species"), sep = " ") %>% 
  unite(sci.name.1980, c(genus, species), sep = " ")

write_csv(taxonomy.1982, "./AOS/VascularSurvey/metadata/sp_codes_1982.csv")

file_string2 <- unlist(str_split(str_squish(str_trim(str_remove_all(str_remove_all(str_remove_all(file_string, c(fixed("25"))), fixed("SUBFILE LIST")), "[S()]"), side = "both")), " "))

for (q in 1:length(file_string2)){
  if (substr(file_string2[q], 1,1) == "0"){
    char = nchar(file_string2[q])
    file_string2[q] = substr(file_string2[q], 2, char)
  }
}

stand <- as.data.frame(rep(file_string2, each = 25)) %>% rename(vascular_1982 = 1)

stand_info <- read_csv("./AOS/StandInfo/AOS_coordinates.csv")

stand2 <- stand %>% left_join(stand_info) %>% select(plot_code) %>% rename(stand = plot_code)

vc_1982 <- cbind(stand2, vc_1982) 
vc_1982x <- vc_1982
vc_1982x[is.na(vc_1982x)] <- 0

#write_csv(vc_1982x, "./AOS/VascularSurvey/raw_data/csv_files/AOS_vasc_198208.csv")

## 1981 data

con <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos81.v.st25.txt")
open(con)

results_list <- list()
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(con)

vc_df_1981 <- data.frame()
quad.col <- c()

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

# metadata

vc_metadata_1981 <- file("./AOS/VascularSurvey/raw_data/txt_files/earlier_years/aos81.v.sx25.txt")
open(vc_metadata_1981)
results_list <- list()
current_line <- 1
while (length(line <- readLines(vc_metadata_1981, n = 1, warn = FALSE)) > 0){
  results_list[current_line] <- line
  current_line <- current_line + 1
} 
close(vc_metadata_1981)

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


sp_string2 <- unlist(str_split(str_squish(str_trim(sp_string, side = "both")), " "))
col.names <- c("quad",sp_string2)
colnames(vc_1981) <- col.names

file_string
file_string2 <- unlist(str_split(str_trim(file_string, side = "both"), " "))
file_string3 <- substr(file_string2[file_string2 != ""], 6,7)

stand <- rep(file_string3, each = 25)

vc_1981 <- cbind(stand, vc_1981)

write_csv(vc_1981, "./AOS/VascularSurvey/raw_data/csv_files/AOS_")

name_string2 <- str_remove(str_squish(str_trim(name_string, side = "both")), "VAR LABELS ")
name_string2
sci.name.1981 <- str_remove_all(unlist(str_extract_all(name_string2, "'.*?'")), "[']")
names.and.codes <- unlist(str_split(name_string2, pattern =  " "))
sci.codes.1981 <- names.and.codes[nchar(names.and.codes) == 4 & 
                               names.and.codes != "sp.'" & 
                               names.and.codes != "5-10"]
taxonomy <- as.data.frame(cbind(sci.codes.1981, sci.name.1981))

#write_csv(taxonomy, "./AOS/VascularSurvey/metadata/sp_codes_1981.csv")
# filled in file manually where there were codes >4 characters (x 6)

# Fin