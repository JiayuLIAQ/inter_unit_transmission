source("R/functions.R")

path_txt <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE,
                                         pattern = "\\.TXT$"))


dt_wind_sensor <- map(path_txt[path %like% "wind sensor_26 Jul"]$path, read_wind_sensor) %>% rbindlist 

dt_wind_sensor <- dt_wind_sensor[,-c("path")] %>% unique # %>% fwrite("data/dt_wind_sensor_raw.csv")
dt_wind_sensor %>% fwrite("clean_data/wind_sensor/dt_wind_sensor_raw.csv")

# dt_wind_sensor <- dt_wind_sensor_unique
dt_wind_sensor[, datetime := ymd_hms(paste(GPZDA_4, GPZDA_3, GPZDA_2, 
                                           str_sub(GPZDA_1, start = 1, end = 2),
                                           str_sub(GPZDA_1, start = 3, end = 4),
                                           str_sub(GPZDA_1, start = 5, end = 10))
                                     ) + hours(8)]

dt_wind_sensor[, vel_mag := as.numeric(WIMDA_19)]
dt_wind_sensor[, vel_angle := WIMDA_13]
dt_wind_sensor[, temp := WIMDA_11]
dt_wind_sensor[, rh := WIMDA_9]

dt_wind_sensor[, c("datetime","vel_mag","vel_angle","temp","rh", "wind_sensor_unit")] [!is.na(vel_mag)] %>% 
  fwrite("clean_data/wind_sensor/dt_wind_sensor.csv")

dt_wind_sensor$WIMWV_4 %>% unique

dt_wind_sensor[,c("WIMDA_17", "WIMDA_19","vel_mag","WIMWV_3")]

dt_wind_sensor %>% ggplot() + geom_line(aes(datetime, as.numeric(WIMDA_19)- vel_mag ) )

dt_wind_sensor[is.na(as.numeric(WIMDA_19))]
dt_wind_sensor[is.na(as.numeric(WIMWV_3))]


test <- read_wind_sensor(path_txt[path %like% "Air study_93 Henderson Road"]$path[5])

path <- path_txt[path %like% "Air study_93 Henderson Road"]$path[5]

hms("03 38 05.40")

dt_wind_sensor <- fread("clean_data/wind_sensor/dt_wind_sensor.csv")


