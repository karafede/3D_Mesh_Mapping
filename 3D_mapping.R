
library(rgl)

# interactive 3D object
plot3d(rnorm(100),rnorm(100),rnorm(100))




# Plotting 3D maps using OpenStreetMap and RGL. For info see: 
# http://geotheory.co.uk/blog/2013/04/26/plotting-3d-maps-with-rgl/

map3d <- function(map, ...){
  if(length(map$tiles)!=1){stop("multiple tiles not implemented") }
  nx = map$tiles[[1]]$xres
  ny = map$tiles[[1]]$yres
  xmin = map$tiles[[1]]$bbox$p1[1]
  xmax = map$tiles[[1]]$bbox$p2[1]
  ymin = map$tiles[[1]]$bbox$p1[2]
  ymax = map$tiles[[1]]$bbox$p2[2]
  xc = seq(xmin,xmax,len=ny)
  yc = seq(ymin,ymax,len=nx)
  colours = matrix(map$tiles[[1]]$colorData,ny,nx)
  m = matrix(0,ny,nx)
  surface3d(xc,yc,m,col=colours, ...)
}

#Sys.setenv(NOAWT=1)   # Mac users: fixes an OSM/X11 issue that may arise
require(rgl)
require(OpenStreetMap)
require(ggplot2)
require(maptools)

library(leaflet)

# download map tile (the '8' parameter for map resolution)
lat <- c(51.7, 51.3); lon <- c(-0.53, 0.3)
map <- openproj(openmap(c(lat[1],lon[1]),c(lat[2],lon[2]), 8, 'osm'))

# import London rents data (originally from London Data Store)
rents <- read.csv("http://bit.ly/YZRYEC", header=T)
head(rents)

# create xyz matrix for line heights (row pairs for segment points)
m <- mat.or.vec(66, 3)
for(i in 1:66) for(j in 1:3) m[i,j] = rents[ceiling(i/2),j+2]
for(i in 1:33) m[i*2,3] = 0; m[,3] = m[,3]/15000
head(m)
colnames(m) <- c("longitude", "latitude", "price")
head(m)


# draw map
run <- function(){
  open3d(windowRect=c(100,100,800,600))
  map3d(map, lit=F)
  segments3d(m, lwd=5, col='red', alpha=0.6)
  play3d(spin3d(axis=c(0,0,0.5), rpm=24), duration=10)
}
run()


# adjust to custom view and save to file
rgl.snapshot("output.png", fmt="png", top=TRUE)

M <- as.data.frame(m)

map <- leaflet(M) %>%
  addTiles() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addCircleMarkers(
    lng = ~ longitude, lat = ~ latitude,
    # popup = ~popup_NO2,
    weight = 3, radius = 10,
    group = "variable"
  )

map
