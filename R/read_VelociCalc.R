source("R/functions.R")

path_xls <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE,
                                         pattern = "\\.xls$"))
path <- path_xls[path %like% "Air study_93 Henderson Road" & path %like% "VC-"]$path[1]



# path <- path_xls[path %like% "Henderson_210423"]$path[4]
# path <- path_xls[path %like% "Chem office"]$path[1]
# dt_all <- map(path_xls[path %like% "Chem office" & path %like% "VC-"]$path, read_velocicalc_with_co2) %>% rbindlist

dt_all <- map(path_xls[path %like% "Air study_93 Henderson Road" & path %like% "VC-"]$path, read_velocicalc) %>% 
  rbindlist %>% .[!is.na(datetime)] %>% separate(base_name, c("equip_unit", "s_r", "loc", "date_id"), sep = "_") %>% setcolorder("datetime")

dt_all %>% fwrite("clean_data/velocicalc/velocicalc_raw.csv")



read_vc_raw <- function(path) {
    
    dt <- fread(path, sep = "", header = F)
    
    dt[, n_char := str_count(V1)]
    dt[, n_tab := str_count(V1, "\t")]
    dt[, n_digit := str_count(V1, "[^0-9]+")]
    dt[, path := path]
}

dt_all <-
  map(path_xls[path %like% "Air study_93 Henderson Road" &
                 path %like% "VC-"]$path[35], read_vc_raw) %>% rbindlist
dt_all[n_char == 41]

dt_all %>% ggplot(aes(n_char)) + geom_histogram()

path_test <- path_xls[path %like% "Air study_93 Henderson Road" & path %like% "VC-"]$path[32]


dt <- fread(path_test, sep = "", header = F)

dt[, n_char := str_count(V1)]
dt[, n_tab := str_count(V1, "\t")]
dt[, n_digit := str_count(V1, "[^0-9]+")]


read_velocicalc_2 <- function(path){
  
  dt <- fread(path, sep = "", header = F)
  
  dt[, n_char := str_count(V1)]
  dt[, n_tab := str_count(V1, "\t")]
  dt[, n_digit := str_count(V1, "[^0-9]+")]
  dt <- dt[n_char %in% c(32:50) & n_digit %in% c(10:17) & n_tab %in% c(4:6)]
  if (unique(dt$n_tab) == 4) {
    dt[, c('date', 'time', 'vel', 'temp', 'rh') := tstrsplit(V1, "\t")]
    dt[, c('vel', 'temp', 'rh') :=lapply(.SD, as.numeric), 
       .SDcols=c('vel', 'temp', 'rh')]
  } else if (unique(dt$n_tab) == 5) {
    dt[, c('date', 'time', 'vel', 'p_inH20', 'temp', 'rh') := tstrsplit(V1, "\t")]
    dt[, c('vel', 'p_inH20', 'temp', 'rh') :=lapply(.SD, as.numeric), 
       .SDcols=c('vel', 'p_inH20', 'temp', 'rh')]
  } else if (unique(dt$n_tab) == 6) {
    dt[, c('date', 'time', 'vel', 'p_inH20', 'temp', 'rh', 'p_hPa') := tstrsplit(V1, "\t")]
    dt[, c('vel', 'p_inH20', 'temp', 'rh', 'p_hPa') :=lapply(.SD, as.numeric), 
       .SDcols=c('vel', 'p_inH20', 'temp', 'rh', 'p_hPa')]
  }
  dt[, datetime := dmy_hms(paste(date, time))]
  dt[, c('V1', 'n_char', 'n_tab', 'n_digit', 'date', 'time') := NULL]
  
  dt[, equip_unit := file_path_sans_ext(basename(path)) %>% str_sub(start = 4L, end = 4L)]
}

dt_all <-
  map(path_xls[path %like% "Air study_93 Henderson Road" &
                 path %like% "VC-"]$path, read_velocicalc_2) %>% 
  rbindlist(fill = T) %>% .[!is.na(datetime)] %>% .[, c("p_inH20","p_hPa") := NULL]
  
dt_all %>% fwrite("clean_data/velocicalc/velocicalc_raw.csv")
