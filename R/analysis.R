source("R/read_clean_data.R")

dt_innova[!is.na(test_id)] %>% 
  ggplot(aes(datetime, sf6_conc, color = paste("Ch", innova_ch))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("Innova Ch")

dt_innova[!is.na(test_id)] %>% 
  ggplot(aes(datetime, sf6_conc, color = paste(loc, s_r))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("locations")


dt_velocicalc[!is.na(test_id)] %>% 
  ggplot(aes(datetime, vel, color = paste("VC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("VC unit")

dt_velocicalc[!is.na(test_id)] %>% 
  ggplot(aes(datetime, vel, color = paste(loc, s_r))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("VC unit")

dt_wind_sensor[!is.na(test_id)] %>% 
  ggplot(aes(datetime, vel_mag , color = paste("Station", wind_sensor_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("Wind sensor unit")

dt_wind_sensor[!is.na(test_id)] %>% 
  ggplot(aes(datetime, vel_mag , color = paste(room_type, s_r, position))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("Wind sensor unit")