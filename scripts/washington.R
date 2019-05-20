

datos.sf <- 
  readOGR('C:/Users/juan_/Desktop/Metro_Bus_Lines/Metro_Bus_Lines.shp')

plot(datos.sf)

#### Tomamos solamente una ruta para visualizar mejor lo que estamos haciendo
ruta <- 'W8'
ROUTE3 <- subset(datos.sf, ROUTE == ruta)
# ROUTE3 <- datos.sf

#### El objeto datos.sf es muy complejo, extraemos las coordenadas.
#### El resultado es una lista con las coordenadas de cada ramal
coords <-
  lapply(slot(ROUTE3, "lines"), function(x)
    lapply(slot(x, "Lines"),
           function(y)
             slot(y, "coords")))

#### Hacemos un dataframe con lo que se aextrajo arriba
set.seed(1234)
res2 <-
  bind_rows(lapply(1:length(coords), function(x) {
    bind_rows(lapply(coords[[x]], as.data.frame))
  })) %>% 
  # bind_rows(lapply(coords[[1]], as.data.frame)) %>% 
  rename(latitud = V1, longitud = V2) %>% 
  unique() 



res3 <- res2

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

res_filt %<>% 
  mutate(lat_lag = lag(latitud),
         lon_lag = lag(longitud)) %>% 
  na.omit()

res_filt %>% head

froms = paste(res_filt$latitud, res_filt$longitud)
tos = paste(res_filt$lat_lag, res_filt$lon_lag)


require('rgeos')
require('igraph')
library('ggraph')
library('tidygraph')

graph = graph.edgelist(cbind(froms, tos), directed = FALSE)
graph_from_edgelist(cbind(froms, tos))

imps <- graph %>% as_tbl_graph() %>% 
  activate(edges) %>%
  mutate(weight = 1) %>%
  activate(nodes) %>% 
  mutate(importancia = centrality_betweenness(directed = F)) %>% 
  arrange(-importancia) 

importancias <- imps %>% as.data.frame() %>% 
  separate(name, c('latitud', 'longitud'), sep = ' ') %>% 
  mutate_at(vars(latitud, longitud), funs(as.numeric))

res3 %>% 
  ggplot(aes(latitud, longitud)) +
  geom_path(size = 1.3, alpha = 0.4) + 
  geom_point(data = importancias,
             aes(latitud, longitud, size = importancia),
             alpha = 0.3) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank())

importancias$importancia %>% summary

ggraph(graph, layout = 'fr') +
  geom_edge_link(alpha=0.2) +
  geom_node_point(colour = 'salmon') +
  theme_graph(base_family = 'sans')

# ggsave(file = 'cerebro2.png', width = 12, height = 12, units = 'cm')
