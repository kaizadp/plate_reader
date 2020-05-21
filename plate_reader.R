### PLATE READER CODE
### May-08-2020

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(drake)

plan = plan(
# 1. import data files
  FILEPATH = "data/date1-data/",
  filePaths <- list.files(path = FILEPATH,pattern = "*.xlsx", full.names = TRUE),

  raw = target(process_directory(FILEPATH),
                       trigger = trigger(change = list.files(FILEPATH, pattern = "xlsx$", recursive = TRUE))),


  data <- do.call(rbind, lapply(filePaths, function(path) {
    df <- read_excel(path) 
    df[["file"]] <- rep(path, nrow(df))
    df})),

    # data = sapply(list.files(path = FILEPATH,pattern = "*.xlsx", full.names = TRUE),
    #               read_excel, simplify = FALSE) %>% bind_rows()

    # if importing only single file, use this instead:
    # data = read_excel("data/FILE-NAME.xlsx")
    
    # melt/gather the dataset

  data_melt  = 
    data %>% 
    rename(letter = `...1`) %>% 
    reshape2::melt(id = c("letter", "file"),
                 variable.name = "num",
                 value.name = "val") %>% 

#  tidyr::gather(num, val, 2:13) %>% 
#  rename(letter = `...1`) %>% 
  dplyr::mutate(Source = paste0(letter, num),
                file = str_replace_all(file, paste0(FILEPATH,"/"),""),
                file = str_replace_all(file, ".xlsx","")),


    # 2. import calibration files
CC_FILEPATH = "data/date1-cc/",

calib = sapply(list.files(path = CC_FILEPATH,pattern = "*.xlsx", full.names = TRUE),
              read_excel, simplify = FALSE) %>% bind_rows(),

# clean the calib file and perform standard curve calculations 
calib_2 = 
  calib %>% 
  tidyr::gather(rep, val, 2:3) %>% 
  # calculate mean
  group_by(conc) %>% 
  dplyr::summarise(mean = mean(val)) %>% 
  # do standard curve, y = mx+b
  na.omit() %>% 
  ungroup %>% 
  dplyr::mutate(slope = lm(conc ~ mean)$coefficients["mean"],
                intercept = lm(conc ~ mean)$coefficients["(Intercept)"]),

SLOPE = mean(calib_2$slope),
INTERCEPT = mean(calib_2$intercept),

# 3. calculate concentrations using the standard curve

data_calc = 
  data_melt %>% 
  dplyr::mutate(conc_ng_uL = (SLOPE * val)+ INTERCEPT,
                conc_ng_uL = round(conc_ng_uL,4)) %>% 
# calculate normalization volume
  dplyr::mutate(norm_uL = 240/conc_ng_uL,
                norm_uL = round(norm_uL,0)) %>% 
# select only the relevant columns
  dplyr::select(Source, file, norm_uL) %>% 
  rename(Volume = norm_uL) %>% 
# add other columns
  dplyr::mutate(
    Rack.x = 1,
    Rack.y = 1,
    Destination = "A1",
    Tool = 1
  ) %>% 
  dplyr::select(Rack.x, Source, Rack.y, Destination, Volume, Tool, file),

# rename the "rack" columns. 
# do this using base R, because dplyr doesn't like two columns with the same name
names(data_calc)[names(data_calc) == "Rack.x"] = "Rack",
names(data_calc)[names(data_calc) == "Rack.y"] = "Rack",

save_files <- function(data, group) {
  write.csv(data, file = file.path("processed", paste0(group$file, ".csv")), row.names = F)
}

data_calc %>%
  group_by(file) %>%
  group_walk(save_files)
)




# 3. EXPORT
write.csv(data_calc, "processed/processed_data.csv", row.names = FALSE)