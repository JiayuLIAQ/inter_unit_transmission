source("R/functions.R")

path_xls <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE,
                                         pattern = "\\.xls$"))

# path <- path_xls[path %like% "Henderson_210423"]$path[4]
# path <- path_xls[path %like% "Chem office"]$path[1]
# dt_all <- map(path_xls[path %like% "Chem office" & path %like% "VC-"]$path, read_velocicalc_with_co2) %>% rbindlist

dt_all <- map(path_xls[path %like% "Air study_93 Henderson Road" & path %like% "VC-"]$path, read_velocicalc) %>% 
  rbindlist %>% .[!is.na(datetime)] %>% separate(base_name, c("equip_unit", "s_r", "loc", "date_id"), sep = "_") %>% setcolorder("datetime")

dt_all %>% fwrite("clean_data/velocicalc/velocicalc_raw.csv")



path <- path_xls[path %like% "Air study_93 Henderson Road" & path %like% "VC-"]$path[3]

test <- read_velocicalc(path_xls[path %like% "Air study_93 Henderson Road" & path %like% "VC-"]$path[2])
dt_all[is.na(co2)]

dt_all %>% ggplot(aes(datetime, co2, color = equip_unit) ) + geom_point()


dt_all <- map(path_xls[path %like% "Henderson_210423"]$path, read_velocicalc) %>% rbindlist

dt_all <- read_velocicalc(path_xls[path %like% "Henderson_210423"]$path[4])


char_cols <- sapply(dt_all, is.character)
names_to_factor <- dt_all %>% names %>% .[char_cols]
