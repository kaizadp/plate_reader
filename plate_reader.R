### PLATE READER CODE
### May-08-2020

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(drake)

# 1. import data files ----

# list all xlsx files in the target directory, and then read and collate them
FILEPATH = "data/date1-data/"
filePaths <- list.files(path = FILEPATH,pattern = "*.xlsx", full.names = TRUE)


data <- do.call(rbind, lapply(filePaths, function(path) {
    df <- read_excel(path) 
    df[["file"]] <- rep(path, nrow(df))
    df}))

PROCESSEDPATH = "processed/"
processedPaths <- list.files(path = PROCESSEDPATH,pattern = "*.csv", full.names = TRUE)

# identify only the new files
# do this by comparing the files in the data folder and the files in the processed folder, and see which are new
# first, remove unnecessary strings from data files and processed files, so we can compare
files_removestr = str_remove_all(filePaths, paste0(FILEPATH,"/")) %>%  str_remove_all(".xlsx")
processed_removestr = str_remove_all(processedPaths, paste0(PROCESSEDPATH,"/"))%>%  str_remove_all(".csv")
# then compare
new_files = setdiff(files_removestr, processed_removestr) %>% paste0(FILEPATH,"/",.,".xlsx")

# now run the code on the new data
newdata <- do.call(rbind, lapply(new_files, function(path) {
    df <- read_excel(path) 
    df[["file"]] <- rep(path, nrow(df))
    df}))


    # data = sapply(list.files(path = FILEPATH,pattern = "*.xlsx", full.names = TRUE),
    #               read_excel, simplify = FALSE) %>% bind_rows()

    # if importing only single file, use this instead:
    # data = read_excel("data/FILE-NAME.xlsx")

#    
# 2. clean data files ----

# the data are currently arranged as a matrix of letters vs. numbers (96 well plate)
# melt all the numbers into a single column

data_melt  = 
    newdata %>% 
    rename(letter = `...1`) %>% 
    reshape2::melt(id = c("letter", "file"),
                 variable.name = "num",
                 value.name = "val") %>% 
# clean the `file` column. remove unnecessary strings of text
    dplyr::mutate(Source = paste0(letter, num),
                file = str_replace_all(file, paste0(FILEPATH,"/"),""),
                file = str_replace_all(file, ".xlsx",""))


# 3. import calibration files ----

# list all files in the calibration curve (cc) target directory
# then read and collate

CC_FILEPATH = "data/date1-cc/"
cc_filePaths <- list.files(path = CC_FILEPATH,pattern = "*.xlsx", full.names = TRUE)

cc_data <- do.call(rbind, lapply(cc_filePaths, function(path) {
    df <- read_excel(path) 
    df[["file"]] <- rep(path, nrow(df))
    df}))

#
# 4. clean the calib file and perform standard curve calculations ----
calibration = 
  cc_data %>%
  dplyr::mutate(file = str_replace_all(file, paste0(CC_FILEPATH,"/"),""),
                file = str_replace_all(file, "-cc.xlsx","")) %>% 
  tidyr::gather(rep, val, rep1:rep2) %>% 
  # calculate mean
  group_by(file, conc) %>% 
  dplyr::summarise(mean = mean(val)) %>% 
  # do standard curve, y = mx+b
  na.omit() %>% 
  ungroup %>% 
  group_by(file) %>% 
  dplyr::summarise(slope = lm(conc ~ mean)$coefficients["mean"],
                intercept = lm(conc ~ mean)$coefficients["(Intercept)"])

#

# 5. calculate concentrations using the standard curve ----

data_calc = 
  data_melt %>% 
  ungroup %>% 
# merge the calibration file
# each row will be assigned a slope and intercept, which is unique for that run (`file` column)
  left_join(calibration, by = "file") %>% 
  dplyr::mutate(conc_ng_uL = (slope * val)+ intercept,
                conc_ng_uL = round(conc_ng_uL,4)) %>% 
# calculate normalization volume uL, as 240/concentration (ng/uL)
  dplyr::mutate(norm_uL = 240/conc_ng_uL,
                norm_uL = round(norm_uL,0)) %>% 
# select only the relevant columns
  dplyr::select(Source, file, norm_uL) %>% 
  rename(Volume = norm_uL) %>% 
# add other columns to match the instrument requirements
  dplyr::mutate(
    Rack.x = 1,
    Rack.y = 1,
    Destination = "A1",
    Tool = 1
  ) %>%
# rearrange the columns to match the instrument requirements  
  dplyr::select(Rack.x, Source, Rack.y, Destination, Volume, Tool, file)


# rename the `Rack*` columns. 
# both `Rack*` columns need to be renamed to `Rack`, to match the instrument requirements
# do this using base R, because dplyr doesn't like two columns with the same name
names(data_calc)[names(data_calc) == "Rack.x"] = "Rack"
names(data_calc)[names(data_calc) == "Rack.y"] = "Rack"

#
# 6. export ----
# group by `file` column, each "file"/run must be saved separately

# make a function, and then apply it to `data_calc`
save_files <- function(data, group) {
  write.csv(data, file = file.path("processed", paste0(group$file, ".csv")), row.names = F)
}

export = 
  data_calc %>%
  group_by(file) %>%
  group_walk(save_files)

###