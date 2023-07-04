source("R/read_clean_data.R")
library(forcats)
# innova raw----------------------------------------------
dt_innova[!is.na(test_id)] %>% 
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
