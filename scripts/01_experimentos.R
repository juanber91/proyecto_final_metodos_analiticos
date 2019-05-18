#### ----------------------------------------------------------------------
####
#### TODO: 
####    + Encontrar una forma de obtener subconjuntos de coordenadas
####      y utilizar la función graph.edgelist para rifarnos.
####    - Buscar datos de dos ciudades más
####    - Sumarizar todo en una función o rutina}

####
#### ----------------------------------------------------------------------
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
ruta <- 'RUTA 2'
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
  # filter(runif(nrow(.)) < 0.1) %>% 
  # group_by(ramal) %>%
  # mutate(latitud = round(latitud, 3),
  #        longitud = round(longitud, 3)) %>%
  # ungroup() %>%
  unique() 

res2 %>% select(-ramal) %>% unique() %>% dim
res2$ramal %>% unique %>% length

res2


#### Ahora vamos a probar otra cosa

require('rgeos')
require('igraph')
library('ggraph')
library('tidygraph')

#### Esta función la encontré en interné. Recibe un SpatialLineDataFrame,
#### calcula los nodos a partir de las intersecciones y contruye un igraph.
buildTopo <- function(lines) {
  g = gIntersection(lines, lines)
  
  edges =
    do.call(rbind, lapply(g@lines[[1]]@Lines, function(ls) {
      as.vector(t(ls@coords))
    }))
  
  lengths = sqrt((edges[, 1] - edges[, 3]) ^ 2 + (edges[, 2] - edges[, 4]) ^ 2)
  
  froms = paste(edges[, 1], edges[, 2])
  tos = paste(edges[, 3], edges[, 4])
  
  #### Esta es la parte que nos va a ser súper útil
  graph = graph.edgelist(cbind(froms, tos), directed = FALSE)
  E(graph)$weight = lengths
  
  return(graph)
  
}

datos.graf <- buildTopo(ruta1)

#### Ya que tenemos un grafo podemos hacer lo de siempre con él, como
#### calcular importancias 
imps <- datos.graf %>% as_tbl_graph() %>% 
  activate(edges) %>%
  mutate(weight = 1) %>%
  activate(nodes) %>% 
  mutate(importancia = centrality_betweenness()) %>% 
  arrange(-importancia) 

imps %>% filter(importancia == 6423464)


importancias <- imps %>% as.data.frame() %>% 
  separate(name, c('latitud', 'longitud'), sep = ' ') %>% 
  mutate_at(vars(latitud, longitud), funs(as.numeric)) #%>% 
  mutate(latitud = round(latitud, 4),
         longitud = round(longitud, 4)) #%>%
# group_by(latitud, longitud) %>% 
# summarise(importancia = mean(importancia)) %>% 
# ungroup()



#### Graficamos con fondo blanco
  
res2 %>% 
  ggplot(aes(latitud, longitud)) +
  geom_path(aes(group = ramal), size = 1.3, alpha = 1) + 
  # geom_point(data = importancias, 
  #            aes(latitud, longitud, size = importancia),
  #            alpha = 0.3) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank())

# ggsave('raw.png', width = 20, height = 20, units = 'in')


#### COn mapa de fondo

qmplot(latitud, longitud, data = res2, color = factor(ramal),
       maptype = 'watercolor', geom = 'blank') +
  geom_path(size = 1) +
  theme(legend.position = 'none')




