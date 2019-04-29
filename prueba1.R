library(tidyverse)
library(ggmap)
library(ggrepel)
# cargamos el paquete RgoogleMaps
library(RgoogleMaps)

# importamos los datos de las lineas de metro
datos <- read.csv("lineasmetro2.csv", stringsAsFactors = F)

# definimos colores de las líneas
cols <- c("pink2", "steelblue", "khaki4", "paleturquoise3",
          "gold", "red", "orange", "turquoise4", "brown", "maroon", "olivedrab")

# convertimos colores en hsv (hue, saturation, value)
cols.hsv = rgb2hsv(col2rgb(cols))

# generamos colores en hsv con transparencia
cols.hsva = rep("", 11)

for (j in 1:ncol(cols.hsv)) {
  a <- cols.hsv[,j]
  cols.hsva[j] <- hsv(h = a[1], s = a[2], v = a[3], alpha = 1)
}

datos %>% head

cdmx <- get_map('Mexico city')

cdmx <- get_map(
    location = c(mean(datos$lon), mean(datos$lat)),     #Long y lat del centro del mapa que buscamos
    source="google",           #Fuente, tb OpenStreetView
    maptype="terrain",         #Tipo. También "satellite", "roadmap"
    zoom=9)  

m <- qmplot(lon, lat, data = datos, 
       maptype = 'watercolor', color = factor(linea))
m + geom_line(aes(lon, lat, group = factor(linea)),
              size = 1)

datos %>% 
  filter(linea == 3) %>% 
  mutate(linf = as.numeric(as.factor(linea)))

datos %>% 
  # filter(linea == 2) %>% 
    ggplot(aes(lon, lat)) +
    geom_point(aes(lon, lat, group = factor(linea), 
                   size = afluencia, color = factor(linea)), alpha = 0.4) +
    geom_path(aes(lon, lat, group = factor(linea), color = factor(linea)),
              size = 1.3, alpha = 0.4) +
    # geom_text_repel(aes(label = nombre), size = 3) +
    coord_equal() +
    scale_color_manual(values = cols.hsva) +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          legend.position = 'none',
          axis.text = element_blank(),
          axis.title = element_blank())


ggsave(filename = 'metro1.png', plot = last_plot(),
       width = 20, height = 20, units = 'cm')

