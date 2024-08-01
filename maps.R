library(tidyverse)
library(OpenStreetMap)
library(sf)
map <- openmap(c(49.585661, -95.731583), c( 50.080105, -97.35741), zoom = 11,
                  type = "esri-imagery", mergeTiles = TRUE)

sa_map2 <- openproj(map)


stops <- data.frame(Stop = c("Winnipeg", "SunGro", "OK Farms", "Rivers Edge"), lat = c(49.822819, 49.774046, 49.861914, 49.875369), long = c(-97.148167, -95.936098, -95.881286, -95.901237)) 

p1 <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2) +
  theme_void() +
  geom_point(data = stops,
           aes(x = long , y = lat ), 
           colour = "red", size =  2.5) +
  ggrepel::geom_label_repel(data = stops, 
            aes(long, lat, label = Stop), 
            size = 3, colour = "black",
            min.segment.length = 0)  
p1

ggsave(plot = p1, filename = "./images/tour_map.png", height = 75, width = 200, units = "mm")

map2 <- openmap(c(49.5, -95.731583), c(50.1, -96.579280), zoom = 11,
               type = "esri-imagery", mergeTiles = TRUE) %>%
  openproj()
  

p2 <- OpenStreetMap::autoplot.OpenStreetMap(map2) +
  theme_void() +
  geom_point(data = filter(stops, Stop != "Winnipeg"),
             aes(x = long , y = lat ), 
             colour = "red", size =  2.5) +
  ggrepel::geom_label_repel(data = filter(stops, Stop != "Winnipeg"), 
                            aes(long, lat, label = Stop), 
                            size = 5, colour = "black",
                            min.segment.length = 0)  
p2

ggsave(plot = p2, filename = "./images/tour_map2.png", height = 150, width = 200, units = "mm")
