library(weathercan)
library(tidyverse)

stations_search(coords = c( 49.80472, -95.9040), normals_years = "current")

climate_id <- 5032162
# station_name <- "PINAWA WNRE"
data <- normals_dl(climate_ids = climate_id)
df_normal <- data %>%
  unnest(normals) %>%
  filter(period != "Year") %>%
  select(period, temp_daily_average, precip, dd_above_5) %>%
  mutate(date = mdy(paste0(period, "-15-2020")))



p1 <- ggplot() +
  theme_bw() +
  geom_segment(data = df_normal, aes(x = date, y = precip/3 - 30, xend = date, yend = -30), size = 8, colour =  "skyblue4") +
  geom_point(data = df_normal, aes(x = date, y = temp_daily_average), size = 2, colour =  "black") +
  stat_smooth(data = df_normal, aes(x = date, y = temp_daily_average), method = "loess", se = FALSE, linewidth = 1, colour =  "black") +
  scale_y_continuous(name = expression("Temperature " ( degree*C)), 
                     sec.axis = sec_axis(~ (. + 30) * 3 , name = "Precipitation (mm)"),
                     limits = c(-30, 30),
                     expand = c(0, 0)) + 
  scale_x_date(date_labels = "%b", 
               date_breaks = "1 month", 
               expand = c(0.01,0.01), 
               name = "",
               limits = (c(as_date("2020-01-01"), as_date("2020-12-31"))))
p1

ggsave(plot = p1, filename = "./images/climate.png", height = 125, width = 150, units = "mm")

df_annual <- data %>%
  unnest(normals) %>%
  filter(period == "Year") %>%
  select(period, snow, temp_daily_average, precip, dd_above_5) 

df_frost <- data %>%
  unnest(frost)
