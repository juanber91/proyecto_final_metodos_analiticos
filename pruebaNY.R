
# importamos los datos de las lineas de metro
datos <- read.csv("./data/NYbuses.csv", stringsAsFactors = F)

# traemos solo las paradas qe nos interesan
# stops <- datos %>% 
#           filter(location_type == 1) %>% 
#           mutate(linf = as.numeric(as.factor(stop_id))) %>%
#           select(stop_id,stop_lat,stop_lon) %>%
#           rename(linea=stop_id,lat=stop_lat,lon=stop_lon)

# traemos solo las paradas qe nos interesan
# stops <- datos %>% 
#           select(Routes,Longitude,Latitude) %>%
#           mutate(unpacked = str_split(Routes,",")) %>%
#           unnest %>%
#           mutate(Routes = str_trim(unpacked)) %>%
#           select(Longitude,Latitude,Routes) %>%
#           # filter(Routes == 114) %>%
#           rename(linea=Routes,lat=Latitude,lon=Longitude) %>%
#           arrange(linea,lon)

# Prueba con rutas de camiones segmentada por tiempo NY
stops <- datos %>%
          # select(PublishedLineName,OriginLat,OriginLong,DestinationLat,DestinationLong) %>%
          filter((PublishedLineName == "B8")  & (VehicleRef =="NYCT_430"))
          arrange(RecordedAtTime)
  
stops <- stops[complete.cases(stops), ]

# graficamos

stops %>% 
  ggplot(aes(lon, lat)) +
  geom_point(aes(lon, lat, group = factor(linea), 
                 # size = afluencia, 
                 color = factor(linea)), alpha = 0.4) +
  geom_path(aes(lon, lat, group = factor(linea), color = factor(linea)),
            size = 1.3, alpha = 0.4) +
  coord_equal() +
  # scale_color_manual(values = cols.hsva) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank())

