library('tidyverse')
library('RgoogleMaps')
library('readxl')
library('rgdal')
library('stplanr')
library('rjson') 
library('ggmap')
library('igraph')
library('ggraph')
library('tidygraph')
library('data.table')
library(magrittr)

load("Nodos_e_intersecciones.RData")

### Filtramos por los nodos que nos interesan para no tomar la ruta completa
res_filt <- res4 %>% 
  filter(nodo == 1)

res_filt %<>% 
  mutate(lat_lag = lag(latitud),
         lon_lag = lag(longitud)) 

res_filt %>% head

froms = paste(res_filt$latitud, res_filt$longitud)
tos = paste(res_filt$lat_lag, res_filt$lon_lag)

graph <- graph.edgelist(cbind(froms, tos), directed = FALSE)
graph_from_edgelist(cbind(froms, tos))

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

res4 %>% 
  ggplot(aes(latitud, longitud)) +
  geom_path(aes(color = ruta,group = ruta_ramal), size = 1.3, alpha = 0.4) +
  geom_point(data = importancias,
             aes(latitud, longitud, size = importancia),
             alpha = 0.3) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        # legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank())

importancias$importancia %>% summary

ggraph(as.igraph(imps),layout = 'fr') +
  geom_edge_link(alpha=0.2) +
  geom_node_point(colour = 'salmon') +
  theme_graph(base_family = 'sans')


