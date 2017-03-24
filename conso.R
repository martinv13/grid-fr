library(deldir)


z <- deldir(spl_postes$x, spl_postes$y)
w <- tile.list(z)
polys <- vector(mode='list', length=length(w))
for (i in seq_along(polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}
spl_zones <- SpatialPolygons(polys, proj4string=CRS("+init=epsg:2154"))

spl_zones <- spTransform(spl_zones, "+init=epsg:4269")


voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                       y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                    function(x) slot(x, 'ID'))))
leaflet(spl_zones) %>% addTiles() %>% addPolygons()
