source("R/functions.R")

dt_innova <- fread("clean_data/innova/innova_raw.csv") %>% .[path %like% "Air study_93 Henderson Road"]
dt_wind_sensor <- fread("clean_data/wind_sensor/dt_wind_sensor.csv")
dt_velocicalc <- fread("clean_data/velocicalc/velocicalc_raw.csv")

cond_general_scenario <- fread("clean_data/conditions/general_scenario.csv")
cond_innova_vc <- fread("clean_data/conditions/innova_vc.csv")
cond_source_and_receptor <- fread("clean_data/conditions/source_and_receptor.csv")
cond_wind_sensor <- fread("clean_data/conditions/wind_sensor.csv")

cond_general_scenario[, start_datetime := dmy_hm(paste(exp_date, start_time) )]
cond_general_scenario[, end_datetime := dmy_hm(paste(exp_date, end_time) )]

cond_general_scenario[, 
                      c("test_id", "start_datetime", "end_datetime", "sf6_rate")][cond_source_and_receptor, 
                                                                                  on = .(test_id)][cond_innova_vc, on = .(test_id, s_r)]
cond_vc_innova_comb <- cond_general_scenario[,
                      c("test_id",
                        "start_datetime",
                        "end_datetime",
                        "sf6_rate")][cond_source_and_receptor[cond_innova_vc,
                                                              on = .(test_id, s_r)],
                                     on = .(test_id)]

add_general_condition <- function(dt, dt_condi) {
  for (i in 1:length(dt_condi$start_datetime) ) {
    dt[datetime >= dt_condi$start_datetime[i] & datetime <= dt_condi$end_datetime[i],
       `:=`(test_id = dt_condi$test_id[i],
            sf6_rate = dt_condi$sf6_rate[i])]
  }
}
# dt <- copy(dt_velocicalc)
# dt_condi <- copy(cond_general_scenario)
# 
# dt[datetime >= dt_condi$start_datetime[i] &
#      datetime <= dt_condi$datetime[i],
#    `:=`(test_id = dt_condi$test_id[i],
#         sf6_rate = dt_condi$sf6_rate[i])]

add_general_condition(dt_velocicalc, cond_general_scenario)
add_general_condition(dt_innova, cond_general_scenario)
add_general_condition(dt_wind_sensor, cond_general_scenario)


dt_velocicalc[, vc_unit := str_sub(equip_unit, start = -1L, end = -1L)]
dt_velocicalc[, loc := str_to_lower(loc)]
dt_velocicalc[, s_r := str_to_lower(s_r)]

dt_velocicalc <- cond_source_and_receptor[cond_innova_vc,
                         on = .(test_id, s_r)][vc_unit != ""][dt_velocicalc, on = .(test_id, s_r, loc, vc_unit) ]# %>% .[!is.na(test_id)]

dt_innova[, innova_ch := as.numeric(str_sub(table_name, start = -2L, end = -1L))]

dt_innova <-cond_source_and_receptor[cond_innova_vc,
                         on = .(test_id, s_r)][dt_innova, on = .(test_id, innova_ch) ]# %>% .[!is.na(test_id)]

dt_wind_sensor[, wind_sensor_unit := as.numeric(str_sub(basename(path), start = 9L, end = 10L)) ]

dt_wind_sensor <- cond_source_and_receptor[cond_wind_sensor,
                         on = .(test_id, s_r, room_type)][dt_wind_sensor, on = .(test_id, wind_sensor_unit) ]# %>% .[!is.na(test_id)]

