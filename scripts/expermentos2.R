library('tidyverse')
library('RgoogleMaps')
library('readxl')
library('rgdal')
library('stplanr')
library('rjson') 
library('ggmap')

# importamos los datos de las rutas
datos <- read_excel("data/rutas-y-corredores-del-transporte-publico-concesionado.xlsx")

# Lo mismo pero en shapefile, veremos con cuál de los dos conviene trabajar
datos.sf <- 
  readOGR("data/prueba/rutas-y-corredores-del-transporte-publico-concesionado.shp")

rawplot <- plot(datos.sf)


#### ---------------------------------------------------------------------
#### La idea aquí es ir haciendo pruebas con subconjuntos de los datos,
#### para eventualmente (cuando ya funcione) meterlo todo en una función.
####----------------------------------------------------------------------

#### Tomamos solamente una ruta para visualizar mejor lo que estamos haciendo
ruta <- 'RUTA 100'
ruta1 <- subset(datos.sf, ruta_corre == ruta)
# ruta1 <- datos.sf

#### El objeto datos.sf es muy complejo, extraemos las coordenadas.
#### El resultado es una lista con las coordenadas de cada ramal
coords <-
  lapply(slot(ruta1, "lines"), function(x)
    lapply(slot(x, "Lines"),
           function(y)
             slot(y, "coords")))

#### Hacemos un dataframe con lo que se aextrajo arriba
set.seed(1234)
res2 <- bind_rows(lapply(coords, as.data.frame), .id = 'ramal') %>% 
  rename(latitud = X1, longitud = X2) %>% 
  unique() 

# res2 %<>% 
res2 %>% 
  group_by(ruta, ramal) %>% 
  mutate(difs = n()) %>% 
  group_by(ruta) %>% 
  dplyr::top_n(1, difs) %>% summary
  select(-difs)

res2 %>% head

res3 <- res2 %>%
  group_by(ramal)

current <- 1
res3$nodo[1] <- 1
j <- 1

while(current + j <= nrow(res3)) {
  
  distancia <- sqrt((res3$latitud[current + j] - res3$latitud[current])^2 + (res3$longitud[current + j] - res3$longitud[current])^2)
  
  print(current)
  
  if (distancia < 0.01){
    res3$nodo[current + j] <- 0
    j <- j + 1
  } else  {
    res3$nodo[current + j] <- 1
    current <- current + j
    j <- 1
  }
  
}

res_filt <- res3 %>% 
  filter(nodo == 1)

library(magrittr)

res_filt %<>% 
  mutate(lat_lag = lag(latitud),
         lon_lag = lag(longitud))

res_filt %>% head

froms = paste(res_filt$latitud, res_filt$longitud)
tos = paste(res_filt$lat_lag, res_filt$lon_lag)

graph = graph.edgelist(cbind(froms, tos), directed = FALSE)


imps <- graph %>% as_tbl_graph() %>% 
  activate(edges) %>%
  mutate(weight = 1) %>%
  activate(nodes) %>% 
  filter(name != 'NA NA') %>% 
  mutate(importancia = centrality_betweenness(directed = F)) %>% 
  arrange(-importancia) 


importancias <- imps %>% as.data.frame() %>% 
  separate(name, c('latitud', 'longitud'), sep = ' ') %>% 
  mutate_at(vars(latitud, longitud), funs(as.numeric))

# res3 %>% 
#   ggplot(aes(latitud, longitud)) +
#   geom_path(aes(color = ramal), size = 1.3, alpha = 0.4) + 
#   geom_point(data = importancias,
#              aes(latitud, longitud, size = importancia),
#              alpha = 0.3) +
#   coord_equal() +
#   theme_minimal() +
#   theme(panel.grid = element_blank(), 
#         legend.position = 'none',
#         axis.text = element_blank(),
#         axis.title = element_blank())
# 
# importancias$importancia %>% summary


# ggraph(graph, layout = 'linear', circular = TRUE) +
ggraph(graph, layout = 'fr') +
  geom_edge_link(alpha=0.2) +
  geom_node_point(colour = 'salmon') +
  theme_graph(base_family = 'sans')

# ggraph(as.igraph(imps),layout = 'linear', circular = TRUE) +
ggraph(as.igraph(imps),layout = 'fr') +
  geom_edge_link(alpha=0.2) +
  geom_node_point(colour = 'salmon') +
  theme_graph(base_family = 'sans')

# ggsave(file = 'cerebro.png', width = 12, height = 12, units = 'cm')
