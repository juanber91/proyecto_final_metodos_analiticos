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
library('rgeos')

# Lo mismo pero en shapefile, veremos con cuál de los dos conviene trabajar
datos.sf <- readOGR('data/CTA_BusRoutes')

datos <- datos.sf@data %>% as.data.frame()

#### ---------------------------------------------------------------------
#### La idea aquí es ir haciendo pruebas con subconjuntos de los datos,
#### para eventualmente (cuando ya funcione) meterlo todo en una función.
####----------------------------------------------------------------------

#### Hacemos un conjunto que contenga las rutas
set_rutas <- unique(datos %>% pull(ROUTE))
set_rutas <- as.vector(na.omit(set_rutas))

### Iteramos sobre set_rutas para pegar la columna ruta a todas las subrutas
res2 <- data.frame()
for(i in 1:length(set_rutas)){
  #### Filtramos por cada ruta
  ruta <- subset(datos.sf, ROUTE == set_rutas[i])
  
  #### El objeto datos.sf es muy complejo, extraemos las coordenadas.
  #### El resultado es una lista con las coordenadas de cada ramal
  coords <-  lapply(slot(ruta, "lines"), function(x)
    lapply(slot(x, "Lines"),function(y)
      slot(y, "coords")))
  
  #### Hacemos un dataframe con lo que se extrajo arriba
  res <- bind_rows(lapply(1:length(coords), function(x) {
    bind_rows(lapply(coords[[x]], as.data.frame))})) %>% 
    rename(latitud = V1, longitud = V2) %>% 
    mutate(ruta = set_rutas[i]) %>% 
    select(ruta, latitud, longitud) %>% 
    unique() 
  
  res2 <- rbind(res2,res)
}

# res2 %>% 
#   group_by(ruta) %>% 
#   mutate(difs = n()) %>% summarise(bla = max(difs)) %>% View
#   filter(difs <= 100) %>% 
#   dplyr::top_n(1,difs) %>% 
#   select(-difs) %>% 
#   ungroup 
# 
# ### Redefinimos set_rutas para solo aquellas que hayan sobrevivido al chasquido de Thanos
# set_rutas <- unique(res2$ruta)

### Etiqueto con uno el primer nodo
res3 <- res2 %>% 
  group_by(ruta) %>% 
  mutate(nodo = ifelse(row_number() == 1, 1,0)) %>% 
  mutate_if(is.numeric, round, 0) %>% 
  ungroup() %>% 
  unique()


current <- 1
j <- 1

### Etiquetar con uno aquellos nodos separados por una distancia de al menos r = 0.01
while(current + j <= nrow(res3)) {
  
  # Si es el primer punto de una ruta
  if(res3$nodo[current+j] == 1){
    current <- current + j
    j <- 1
  } else {
    distancia <- sqrt((res3$latitud[current + j] - res3$latitud[current])^2 + (res3$longitud[current + j] - res3$longitud[current])^2)
    if (distancia < 100000){
      j <- j + 1
    } else  {
      res3$nodo[current + j] <- 1
      current <- current + j
      j <- 1
    }
  }
  
}

### Etiqueto con uno el último nodo
res3 <- res3 %>% group_by(ruta) %>% 
  mutate(nodo = ifelse((row_number() == n()) | (nodo == 1), 1,0)) %>% 
  filter(nodo == 1)


### Obtenemos el nombre de las rutas que se intersectan en pares para después usar el resultado
### y encontrar los puntos de interseccion más eficientemente
intersecciones <- data.frame()
for(i in 1:length(set_rutas)){
  print(i)
  if(i+1 > length(set_rutas)){
    break
  }
  for(j in (i+1):length(set_rutas)){
    x <- res3 %>% filter(ruta == set_rutas[i]) 
    y <- res3 %>% filter(ruta == set_rutas[j]) 
    x_spatial <- SpatialLines(list(Lines(Line(cbind(x$latitud,x$longitud)), ID=set_rutas[i])))
    y_spatial <- SpatialLines(list(Lines(Line(cbind(y$latitud,y$longitud)), ID=set_rutas[j])))
    
    if(gIntersects(x_spatial,y_spatial)){
      intersecciones <- rbind(intersecciones,cbind(set_rutas[i],set_rutas[j]))
    }
    
  }
}

### Hacemos las intersecciones
x_spatial <- vector()
y_spatial <- vector()
res4 <- res3
tiempo <- Sys.time()
for(i in 1:dim(intersecciones)[1]){
  
  x <- res4 %>% 
    filter(ruta == intersecciones$V1[i]) %>%
    mutate(lat_lag = lag(latitud),lon_lag = lag(longitud)) %>%
    na.omit()
  
  y <- res4 %>% 
    filter(ruta == intersecciones$V2[i]) %>%
    mutate(lat_lag = lag(latitud),lon_lag = lag(longitud)) %>%
    na.omit()
  
  for(j in 1:dim(x)[1]){
    for(k in 1:dim(y)[1]){
      
      if((x$latitud[j] == x$lat_lag[j] & x$longitud[j] == x$lon_lag[j])|(y$latitud[k] == y$lat_lag[k] & y$longitud[k] == y$lon_lag[k])){
        next
      }
      
      x_spatial <- SpatialLines(list(Lines(Line(cbind(rbind(x$latitud[j],x$lat_lag[j]),rbind(x$longitud[j],x$lon_lag[j]))), ID=paste(intersecciones$V1[i],j,sep="_"))))
      y_spatial <- SpatialLines(list(Lines(Line(cbind(rbind(y$latitud[k],y$lat_lag[k]),rbind(y$longitud[k],y$lon_lag[k]))), ID=paste(intersecciones$V2[i],k,sep="_"))))
      
      if((class(gIntersection(x_spatial,y_spatial))[1]=='SpatialLines') | is.null(gIntersection(x_spatial,y_spatial))){
        next
      }
      
      if(gIntersects(x_spatial,y_spatial)){
        inter <- as.data.frame(gIntersection(x_spatial,y_spatial))
        
        x_coords <-  as.data.frame(lapply(slot(x_spatial, "lines"), function(x)
          lapply(slot(x, "Lines"),function(y)
            slot(y, "coords"))))
        
        y_coords <-  as.data.frame(lapply(slot(y_spatial, "lines"), function(x)
          lapply(slot(x, "Lines"),function(y)
            slot(y, "coords"))))
        
        aux_upper <- res4[1:which(res4$latitud==x_coords$X1[1] & res4$ruta==intersecciones$V1[i]),]
        aux_lower <- res4[(which(res4$latitud==x_coords$X1[1] & res4$ruta==intersecciones$V1[i])+1):dim(res4)[1],]
        aux_middle <- aux_upper[nrow(aux_upper),]
        aux_middle[1,2:4] <- c(inter[1,1:2], 1)
        
        res4 <- bind_rows(aux_upper, aux_middle, aux_lower)
        
        aux_upper <- res4[1:which(res4$latitud==y_coords$X1[1] & res4$ruta==intersecciones$V2[i]),]
        aux_lower <- res4[(which(res4$latitud==y_coords$X1[1] & res4$ruta==intersecciones$V2[i])+1):dim(res4)[1],]
        aux_middle <- aux_upper[nrow(aux_upper),]
        aux_middle[1,2:4] <- c(inter[1,1:2], 1)
        
        res4 <- bind_rows(aux_upper, aux_middle, aux_lower)
      }
      
    }
  }
  
  print(paste(i, ' ---- ', as.numeric(Sys.time() - tiempo)))
}


### Filtramos por los nodos que nos interesan para no tomar la ruta completa
res_filt <- res4

res_filt %<>% 
  mutate(lat_lag = lag(latitud),
         lon_lag = lag(longitud)) %>% 
  na.omit()

res_filt %>% head

froms = paste(res_filt$latitud, res_filt$longitud)
tos = paste(res_filt$lat_lag, res_filt$lon_lag)

graph <- graph.edgelist(cbind(froms, tos), directed = FALSE)
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

res2 %>% 
  ggplot(aes(latitud, longitud)) +
  geom_path(aes(color = ruta, group = ruta), size = 1.3, alpha = 0.4) + 
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

ggsave(file = 'cerebro2.png', width = 12, height = 12, units = 'cm')

