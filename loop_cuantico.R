x_spatial <- vector()
y_spatial <- vector()
res4 <- res3 %>% ungroup() 
tiempo <- Sys.time()
# for(i in 1:dim(intersecciones)[1]){
lapply(1:dim(intersecciones)[1], function(i) {
# lapply(432:534, function(i) {
  x <- res4 %>% 
    filter(ruta == intersecciones$V1[i]) %>%
    mutate(lat_lag = lag(latitud),lon_lag = lag(longitud)) %>%
    na.omit()
  
  y <- res4 %>% 
    filter(ruta == intersecciones$V2[i]) %>%
    mutate(lat_lag = lag(latitud),lon_lag = lag(longitud)) %>%
    na.omit()
  
  if(dim(x)[1] == 1) {
    x <- bind_rows(x, x)
  }
  
  if(dim(y)[1] == 1) {
    y <- bind_rows(y, y)
  }
  
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
        
        res4 <<- bind_rows(aux_upper, aux_middle, aux_lower) %>% unique
        
        aux_upper <- res4[1:which(res4$latitud==y_coords$X1[1] & res4$ruta==intersecciones$V2[i]),]
        aux_lower <- res4[(which(res4$latitud==y_coords$X1[1] & res4$ruta==intersecciones$V2[i])+1):dim(res4)[1],]
        aux_middle <- aux_upper[nrow(aux_upper),]
        aux_middle[1,2:4] <- c(inter[1,1:2], 1)
        
        res4 <<- bind_rows(aux_upper, aux_middle, aux_lower) %>% unique
      }
      
    }
    
  }
  # gc()
  print(paste(i, ' ---- ', as.numeric(Sys.time() - tiempo)))
})

res4_432_534 <- res4
save(res4_432_534, file = 'res4_432_534.RData')

# }