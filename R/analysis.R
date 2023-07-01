source("R/read_clean_data.R")

dt_innova[!is.na(test_id)] %>% 
  ggplot(aes(datetime, sf6_conc, color = paste(loc, s_r, "\nCh.", innova_ch))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("locations")+
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")

ggsave("plot/SF6_raw.png", width = 16, height = 8)

dt_velocicalc[!is.na(test_id)] %>% 
  ggplot(aes(datetime, vel, color = paste("VC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("VC unit")

dt_velocicalc[!is.na(test_id)] %>% 
  ggplot(aes(datetime, vel, color = paste(loc, s_r, "\nVC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("VC unit\nlocation")+
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")

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