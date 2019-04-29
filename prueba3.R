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

# imagen de un mapa google de la ciudad de Mexico
MexicoDF <- GetMap.bbox(datos$lon, datos$lat, 
                        destfile = "MexDF.png", GRAYSCALE = T, zoom = 12)

# graficamos estaciones sobre la imagen del mapa
# primero las estaciones de la linea 1
PlotOnStaticMap(MexicoDF, datos$lat[datos$linea==1], datos$lon[datos$linea==1], col=cols.hsva[1], pch=19, cex=1.3)
PlotOnStaticMap(MexicoDF, datos$lat[datos$linea==1], datos$lon[datos$linea==1], col=cols.hsva[1], lwd=3, FUN=lines, add=TRUE)
TextOnStaticMap(MexicoDF, datos$lat[datos$linea==1], datos$lon[datos$linea==1], labels=datos$nombre[datos$linea==1], col=cols[1], cex=0.6, add=TRUE)

# luego las estaciones de las siguientes lineas
for (i in 2:11)
{
    PlotOnStaticMap(MexCity, datos$lat[datos$linea==i], datos$lon[datos$linea==i], col=cols.hsva[i], pch=19, cex=1.3, add=TRUE)
    PlotOnStaticMap(MexCity, datos$lat[datos$linea==i], datos$lon[datos$linea==i], col=cols.hsva[i], lwd=3, FUN=lines, add=TRUE)
    TextOnStaticMap(MexicoDF, datos$lat[datos$linea==i], datos$lon[datos$linea==i], labels=datos$nombre[datos$linea==i], col=cols[i], cex=0.6, add=TRUE)
}