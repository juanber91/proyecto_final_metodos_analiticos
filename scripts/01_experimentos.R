# cargamos el paquete RgoogleMaps
library('tidyverse')
library('RgoogleMaps')
library('readxl')
library('rgdal')
library('stplanr')
library('rjson')
library('ggmap')

# importamos los datos de las lineas de metro
datos <- read_excel("data/rutas-y-corredores-del-transporte-publico-concesionado.xlsx")

datos.sf <- 
  readOGR("data/prueba/rutas-y-corredores-del-transporte-publico-concesionado.shp")

rawplot <- plot(datos.sf)

ruta <- 'RUTA 1'
ruta1 <- subset(datos.sf, ruta_corre == ruta)
# ruta1 <- subset(datos.sf, ruta_corre == ruta & runif(2311) < 0.5)
# ruta1 <- datos.sf

res <-
  lapply(slot(ruta1, "lines"), function(x)
    lapply(slot(x, "Lines"),
           function(y)
             slot(y, "coords")))

set.seed(1234)
res2 <- bind_rows(lapply(res, as.data.frame), .id = 'subruta') %>% 
  rename(latitud = X1, longitud = X2) %>% 
  # filter(runif(nrow(.)) < 0.1) %>% 
  group_by(subruta) %>%
  mutate(latitud = round(latitud, 3),
         longitud = round(longitud, 3)) %>%
  ungroup() %>%
  unique() 

res2 %>% select(-subruta) %>% unique() %>% dim
res2$subruta %>% unique %>% length



# Creamos grafo y calculamos importancias

require('rgeos')
require('igraph')

buildTopo <- function(lines){
  
  g = gIntersection(lines,lines)
  edges = 
    do.call(rbind,lapply(g@lines[[1]]@Lines,function(ls){as.vector(t(ls@coords))}))
  lengths = sqrt((edges[,1]-edges[,3])^2+(edges[,2]-edges[,4])^2)
  
  froms = paste(edges[,1],edges[,2])
  tos = paste(edges[,3],edges[,4])
  
  graph = graph.edgelist(cbind(froms,tos),directed=FALSE)
  E(graph)$weight=lengths
  return(graph)
  
}

dat_g <- buildTopo(ruta1)

library('ggraph')
library('tidygraph')

imps <- dat_g %>% as_tbl_graph() %>% 
  activate(edges) %>%
  mutate(weight = 1) %>%
  # filter(runif(imps %>% activate(edges) %>% as.data.frame() %>% nrow) < 0.1) %>% 
  activate(nodes) %>% 
  # filter(runif(imps %>% activate(nodes) %>% as.data.frame() %>% nrow) < 0.1) %>% 
  mutate(importancia = centrality_betweenness()) %>% 
  arrange(-importancia) 




importancias <- imps %>% as.data.frame() %>% 
  separate(name, c('latitud', 'longitud'), sep = ' ') %>% 
  mutate_at(vars(latitud, longitud), funs(as.numeric)) #%>% 
mutate(latitud = round(latitud, 4),
       longitud = round(longitud, 4)) #%>%
# group_by(latitud, longitud) %>% 
# summarise(importancia = mean(importancia)) %>% 
# ungroup()



# Graficamos

res2 %>% 
  ggplot(aes(latitud, longitud)) +
  geom_path(aes(color = subruta), size = 1.3, alpha = 0.4) + 
  # geom_point(data = importancias, aes(latitud, longitud, size = importancia)) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank())

# ggraph(imps, layout = 'linear', circular = TRUE) +
#   geom_edge_link(color = '#24CBE5')  +
#   geom_node_point(aes(size = importancia), color = 'salmon', fill = 'black') +
#   # geom_node_text(aes(label = name), nudge_y = 0.2, size = 3, repel = T) +
#   theme_graph(base_family = 'sans') +
#   theme(legend.position = 'bottom')

ggsave('raw.png', width = 20, height = 20, units = 'in')


qmplot(latitud, longitud, data = res2, color = factor(subruta),
       maptype = 'watercolor', geom = 'blank') +
  geom_path(size = 1) +
  theme(legend.position = 'none')




