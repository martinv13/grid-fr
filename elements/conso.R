

# découpage voronoi des zones autour des postes
z <- deldir(spl_postes$x, spl_postes$y)
w <- tile.list(z)
polys <- vector(mode='list', length=length(w))
for (i in seq_along(polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}
spl_zones <- SpatialPolygons(polys, proj4string=CRS("+init=epsg:2154"))
voronoi <- SpatialPolygonsDataFrame(spl_zones, 
                                    data=data.frame(
                                      nom=spl_postes$nom,
                                      codnat=spl_postes$codeNat,
                                      row.names=sapply(slot(spl_zones, 'polygons'),
                                                       function(x) slot(x, 'ID'))))

# découpage des départements
nuts <- fread("fr_nuts.csv")
regions <- unique(nuts$region )
fr <- subset(nuts3.spdf, nuts3.spdf$id %in% nuts$nuts3)
fr <- spTransform(fr, "+init=epsg:2154")
depts <- nuts3.df[match(fr$id,nuts3.df$id),]
ids <- row.names(depts)
setDT(depts)
nuts[,id:=nuts3]
depts <- nuts[depts, on="id", mult="first"]
depts[,cle:=gdppps2008/sum(gdppps2008)*0.5+
        pop2008/sum(pop2008)*0.5, by=region]
setDF(depts)
row.names(depts) <- ids
fr <- SpatialPolygonsDataFrame(fr, depts)
fr$dept_area <- gArea(fr, byid=TRUE)

# intersection des départements et zones voronoi
inter <- intersect(fr, voronoi)
inter$i_area <- gArea(inter, byid=TRUE)

inter$cle_conso <- inter$cle*inter$i_area/inter$dept_area

voronoi <- spTransform(voronoi, "+init=epsg:4326")
inter <- spTransform(inter, "+init=epsg:4326")


