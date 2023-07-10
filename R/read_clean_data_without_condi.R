source("R/functions.R")
# innova ----------------
dt_innova <- fread("clean_data/innova/innova_raw.csv") %>% .[path %like% "Air study_93 Henderson Road"]
dt_innova[, innova_ch := as.numeric(str_sub(table_name, start = -2L, end = -1L))]

dt_innova[, datetime := round_date(datetime, unit = "minute")]
# wind sensor ----------------
dt_wind_sensor <- fread("clean_data/wind_sensor/dt_wind_sensor_raw.csv", colClasses = list(character=c("GPGGA_1","GPZDA_1")))

dt_wind_sensor[, datetime := ymd_hms(paste(GPZDA_4, GPZDA_3, GPZDA_2, 
                                           str_sub(GPZDA_1, start = 1, end = 2),
                                           str_sub(GPZDA_1, start = 3, end = 4),
                                           str_sub(GPZDA_1, start = 5, end = 10)) )  + hours(8)]

dt_wind_sensor[, vel_mag_WIMDA_19 := WIMDA_19]
dt_wind_sensor[, vel_mag_WIMWV_3 := WIMWV_3 * 0.514444]
dt_wind_sensor[, vel_angle_WIMDA_13 := WIMDA_13]
dt_wind_sensor[, vel_angle_WIMWV_1 := WIMWV_1]
dt_wind_sensor[, temp_WIMDA_5 := WIMDA_11]
dt_wind_sensor[, rh_WIMDA_9 := WIMDA_9]

dt_wind_sensor <- dt_wind_sensor[, c(
  "datetime",
  "vel_mag_WIMDA_19",
  "vel_mag_WIMWV_3",
  "vel_angle_WIMDA_13",
  "vel_angle_WIMWV_1",
  "temp_WIMDA_5",
  "rh_WIMDA_9",
  "wind_sensor_unit"
)] %>% .[!is.na(datetime)]
# fwrite("clean_data/wind_sensor/dt_wind_sensor.csv")
dt_wind_sensor[, datetime := round_date(datetime, unit = "minute")]

dt_wind_sensor <-
  dt_wind_sensor[, lapply (.SD, mean), .SDcols = c(
    "vel_mag_WIMDA_19",
    "vel_mag_WIMWV_3",
    "vel_angle_WIMDA_13",
    "vel_angle_WIMWV_1",
    "temp_WIMDA_5",
    "rh_WIMDA_9"
  ), by = .(datetime,wind_sensor_unit)]



# VelociCalc ---------------------------
dt_velocicalc <- fread("clean_data/velocicalc/velocicalc_raw.csv") 
dt_velocicalc[, vc_unit := str_sub(equip_unit, start = -1L, end = -1L)]
dt_velocicalc[, loc := str_to_lower(loc)]
dt_velocicalc[, s_r := str_to_lower(s_r)]

dt_velocicalc[, datetime := round_date(datetime, unit = "minute")]

# write them out-----------------------

# make them wide
# dt_innova[, innova_ch := paste0("innova_ch_", innova_ch,"_sf6_ppm")]
# dt_innova <- dt_innova[, c("datetime","sf6_conc","innova_ch")] %>% dcast(datetime ~ innova_ch, value.var = "sf6_conc")

# dt_wind_sensor[, wind_sensor_unit := paste0("from_Station_", wind_sensor_unit)]
# dt_wind_sensor <- dt_wind_sensor %>% dcast(datetime~wind_sensor_unit, value.var = c("vel_mag_WIMDA_19",
#                                                                                     "vel_mag_WIMWV_3",
#                                                                                     "vel_angle_WIMDA_13",
#                                                                                     "vel_angle_WIMWV_1",
#                                                                                     "temp_WIMDA_5",
#                                                                                     "rh_WIMDA_9")#,fun.aggregate = mean
# )

# dt_velocicalc <- 
#   dt_velocicalc %>% dcast(datetime ~ equip_unit, value.var = c("vel","temperature","RH") )


# dt_wide_all <- merge(merge(dt_innova, dt_velocicalc, by = "datetime", all = T), dt_wind_sensor, all = T) #%>% as.data.frame 
# 
# n_tables <- unique(date(dt_wide_all$datetime) ) %>% length 
# table_names <- unique(format(dt_wide_all$datetime, format = "%B_%d") )
# 
# # class(test)
# for(i in 1:n_tables){
#   xlsx::write.xlsx(dt_wide_all[format(datetime, format = "%B_%d") == table_names[i]], 
#                    file="wide_data_2.xlsx", 
#                    sheetName=table_names[i], append=TRUE, row.names=FALSE, showNA=FALSE)
# }
# 
# for(i in 13:n_tables){
#   xlsx::write.xlsx(dt_wide_all[format(datetime, format = "%B_%d") == table_names[i]], 
#                    file="wide_data_2.xlsx", 
#                    sheetName=table_names[i], append=TRUE, row.names=FALSE, showNA=FALSE)
# }