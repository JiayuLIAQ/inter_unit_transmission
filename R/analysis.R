source("R/read_clean_data.R")

# innova raw----------------------------------------------
dt_innova %>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(datetime, sf6_conc, color = paste(loc, s_r, "\nCh.", innova_ch))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("locations")+
  guides(color = guide_legend(nrow = 1)) +
  scale_y_log10() +
  theme(legend.position = "top")

ggsave("plots/SF6_raw.png", width = 16, height = 8)

dt_innova[, time_ := hms::as_hms(paste( hour(datetime), minute(datetime), second(datetime), sep = ":") )]%>% 
  setorder(datetime) %>%
  # .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, sf6_conc, color = paste("Ch.", innova_ch))) +
  geom_line() + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("Innova channels")+
  guides(color = guide_legend(nrow = 1)) +
  scale_y_log10() +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")

ggsave("plots/SF6_by_date_raw.png", width = 16, height = 8)

# velocicalc raw----------------------------------------------
dt_velocicalc[!is.na(test_id)] %>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(datetime, vel, color = paste(loc, s_r, "\nVC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("locations\nVC unit")+
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")

ggsave("plots/VC_vel_raw.png", width = 16, height = 8)


dt_velocicalc[!is.na(test_id)] %>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(datetime, temperature, color = paste(loc, s_r, "\nVC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("locations\nVC unit")+
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")
ggsave("plots/VC_temperature_raw.png", width = 16, height = 8)


dt_velocicalc[!is.na(test_id)] %>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(datetime, RH, color = paste(loc, s_r, "\nVC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("locations\nVC unit")+
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")
ggsave("plots/VC_RH_raw.png", width = 16, height = 8)


# velocicalc raw by date----------------------------------------------
dt_velocicalc[, time_ := hms::as_hms(paste( hour(datetime), minute(datetime), second(datetime), sep = ":") )]%>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, vel, color = paste("VC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("VC unit")+
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")

ggsave("plots/VC_vel_by_date_raw.png", width = 16, height = 8)


dt_velocicalc[, time_ := hms::as_hms(paste( hour(datetime), minute(datetime), second(datetime), sep = ":") )]%>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, temperature, color = paste("VC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("VC unit")+
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")
ggsave("plots/VC_temperature_by_date_raw.png", width = 16, height = 8)


dt_velocicalc[, time_ := hms::as_hms(paste( hour(datetime), minute(datetime), second(datetime), sep = ":") )]%>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, RH, color = paste("VC-", vc_unit))) +
  geom_line() + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("VC unit")+
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")
ggsave("plots/VC_RH_by_date_raw.png", width = 16, height = 8)

# windsensor raw----------------------------------------------
dt_wind_sensor[!is.na(test_id)] %>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(datetime, vel_mag , color = paste(room_type, s_r, position, "\nStation", wind_sensor_unit))) +
  geom_line() + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("Location\nWind sensor unit") +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")
ggsave("plots/windsensor_vel_mag_raw.png", width = 16, height = 8)


dt_wind_sensor[!is.na(test_id)] %>% 
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(datetime, vel_angle , color = paste(room_type, s_r, position, "\nStation", wind_sensor_unit))) +
  geom_point(size = 0.5) + 
  facet_wrap(vars(test_id), scales = "free") +
  scale_color_discrete("Location\nWind sensor unit") +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top")
ggsave("plots/windsensor_vel_angle_raw.png", width = 16, height = 8)


# windsensor raw by date----------------------------------------------
dt_wind_sensor[, time_ := hms::as_hms(format(datetime, format = "%H:%M:%S") )] %>% 
  .[!is.na(time_)] %>%
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, vel_mag , color = paste("Station", wind_sensor_unit))) +
  geom_line() + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("Wind sensor unit") +
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")
ggsave("plots/windsensor_vel_mag_by_date_raw.png", width = 16, height = 8)


dt_wind_sensor[, time_ := hms::as_hms(format(datetime, format = "%H:%M:%S") )] %>% 
  .[!is.na(time_)] %>%
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, vel_angle , color = paste("Station", wind_sensor_unit))) +
  geom_point(size = 0.5) + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("Wind sensor unit") +
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")
ggsave("plots/windsensor_vel_angle_by_date_raw.png", width = 16, height = 8)


# windsensor raw by date----------------------------------------------
dt_wind_sensor[, time_ := hms::as_hms(format(datetime, format = "%H:%M:%S") )] %>% 
  .[!is.na(time_)] %>%
  setorder(datetime) %>%
  # .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, vel_mag_WIMDA_19 , color = paste("Station", wind_sensor_unit))) +
  geom_line() + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("Wind sensor unit") +
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")
ggsave("plots/windsensor_vel_mag_WIMDA_19_by_date_raw.png", width = 16, height = 8)


dt_wind_sensor[, time_ := hms::as_hms(format(datetime, format = "%H:%M:%S") )] %>% 
  .[!is.na(time_)] %>%
  setorder(datetime) %>%
  .[, test_id := fct_inorder(paste("Test",test_id)) ] %>% 
  ggplot(aes(time_, vel_angle , color = paste("Station", wind_sensor_unit))) +
  geom_point(size = 0.5) + 
  facet_wrap(vars(date(datetime)), scales = "free") +
  scale_color_discrete("Wind sensor unit") +
  guides(color = guide_legend(nrow = 1)) +
  coord_cartesian(xlim = c(hms::as_hms("10:00:00"),hms::as_hms("22:00:00")) ) +
  theme(legend.position = "top")
ggsave("plots/windsensor_vel_angle_by_date_raw.png", width = 16, height = 8)
