library(tidyverse)
library(sf)



soil <- st_read("./maps/Soil_Survey_MB_-2858739705440123258/Soil_Survey_MB.shp") %>%
  st_transform(4324)

sf_use_s2(FALSE)

box = c(xmax = -95.75, ymin = 49.75, xmin = -96.0, ymax = 49.89)

plot1 <- soil %>%
  st_crop(box) %>%
  mutate(Texture = case_when(C_SURFTEXT == 21 ~ "Clayey",
                                C_SURFTEXT == 22 ~ "Fine Loamy",
                                C_SURFTEXT == 23 ~ "Coarse Loamy",
                                C_SURFTEXT == 24 ~ "Sands",
                                C_SURFTEXT == 25 ~ "Coase Sands",
                                C_SURFTEXT == 26 ~ "Organic",
                                C_SURFTEXT == -99 ~ "Rock",
                                C_SURFTEXT == 6 ~ "Water")) %>%
  mutate(Texture = fct_relevel(Texture, "Clayey", "Fine Loamy", "Coarse Loamy", "Sands", "Coase Sands", "Organic","Water","Rock")) %>%
  mutate(C_DRAIN = case_when(C_DRAIN == 22 ~ "Rapid",
                             C_DRAIN == 23 ~ "Well",
                             C_DRAIN == 25 ~ "Imperfect",
                             C_DRAIN == 26 ~ "Poor",
                             C_DRAIN == 27 ~ "Very Poor",
                             C_DRAIN == 28 ~ "Rock",
                             C_DRAIN == 6 ~ "Water")) %>%
  mutate(drainage = fct_relevel(C_DRAIN, "Rapid", "Well", "Imperfect", "Poor", "Very Poor", "Rock","Water"))%>%
  mutate(salt = case_when(C_SALT == 21 ~ "non-saline (<4 dS/m.)",
                          C_SALT == 6 ~ "Water")) %>%
  mutate(C_AGRI = case_when(C_AGRI == 22 ~ "Class 2",
                            C_AGRI == 23 ~ "Class 3",
                            C_AGRI == 24 ~ "Class 4",
                            C_AGRI == 25 ~ "Class 5",
                            C_AGRI == 26 ~ "Class 6",
                            C_AGRI == 27 ~ "Class 7",
                            C_AGRI == 28 ~ "Organic",
                            C_AGRI == 6 ~ "Water"))  %>%
  mutate(capability = fct_relevel(C_AGRI, "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7", "Organic Soils", "Water"))

                             
                             
unique(plot1$C_AGRI) 
                             
                             
stops <- data.frame(Stop = c("Winnipeg", "SunGro", "OK Farms", "Rivers Edge"), lat = c(49.822819, 49.769927, 49.861914, 49.875369), long = c(-97.148167, -95.936098, -95.881286, -95.901237)) %>%
  st_as_sf(coords = c("long", "lat"),
          crs = 4324)

texture <- ggplot() +
  geom_sf(data = plot1, aes(fill = Texture), lwd = 0, show.legend = T) +
  geom_sf(data = filter(stops, Stop != "Winnipeg")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t = 0,  
                             r = 0,  
                             b = 0, 
                             l = 0)) +
  ggrepel::geom_label_repel(
    data = filter(stops, Stop != "Winnipeg"),
    aes(label = Stop, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2
  ) +
  scale_fill_manual(values = c("#004B40", "#00AB9C", "#BAEAE4", "#F3DEC6", "#B98E45", "#533600", "blue", "black"))
texture
  # hcl_palettes(palette = "Lajolla", n = 7)
  # 
  # sequential_hcl(palette = "Green", n = 6)
  # diverging_hcl(palette = "Green-Brown", n = 6)
ggsave(plot = texture, filename = "./images/texture_map.png", height = 100, width = 160, units = "mm")


drain <- ggplot() +
  geom_sf(data = plot1, aes(fill = drainage), lwd = 0, show.legend = T) +
  geom_sf(data = filter(stops, Stop != "Winnipeg")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null")) +
  ggrepel::geom_label_repel(
    data = filter(stops, Stop != "Winnipeg"),
    aes(label = Stop, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2
  ) +
  scale_fill_manual(values = c("#E2E2E2", "#D6D7DD", "#BEC1D4", "#A1A6C8", "#7D87B9", "black", "#023FA5"))
drain

ggsave(plot = drain, filename = "./images/drainage_map.png", height = 100, width = 150, units = "mm")


salt <- ggplot() +
  geom_sf(data = plot1, aes(fill = stringr::str_wrap(salt,10)), lwd = 0, show.legend = T) +
  geom_sf(data = filter(stops, Stop != "Winnipeg")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null")) +
  ggrepel::geom_label_repel(
    data = filter(stops, Stop != "Winnipeg"),
    aes(label = Stop, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2
  ) +
  scale_fill_manual(values = c( "#FFBB80", "#023FA5"))
salt

ggsave(plot = salt, filename = "./images/salt_map.png", height = 100, width = 150, units = "mm")

capability <- ggplot() +
  geom_sf(data = plot1, aes(fill = capability), lwd = 0, show.legend = T) +
  geom_sf(data = filter(stops, Stop != "Winnipeg")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null")) +
  ggrepel::geom_label_repel(
    data = filter(stops, Stop != "Winnipeg"),
    aes(label = Stop, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2
  ) +
  scale_fill_manual(values = c("#006027", "#51825C", "#83A38A", "#B0C2B3", "#D6DED7", "#F1F1F1", "#533600", "#023FA5"))
capability

ggsave(plot = capability, filename = "./images/capability_map.png", height = 100, width = 150, units = "mm")
