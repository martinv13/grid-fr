.libPaths("C:/R_packages")

library(leaflet)
library(dplyr)
library(magrittr)
library(cartography)
library(rgeos)
library(raster)
library(mapview)
library(rgdal)
library(data.table)
library(jsonlite)
library(httr)



load("carte_rte.RData")

unlist(lapply(layers, function(x){x[[1]]$geometryType }))

points <- lapply(c(1:3,7:10), function(i){
  lapply(layers[[i]], function(ch){
    temp <- as.list(ch$features)
    temp <- cbind(temp$attributes, temp$geometry)
    setDT(temp)
    temp
  }) %>% rbindlist
})

View(points[[7]])

for (i in 1:7) {
write.table(points[[i]], file=paste0(i,".csv"), sep=";", dec=",", row.names = FALSE)
}
postes 

nuts2.df
nuts2.spdf[[match("FR41",nuts2.spdf$id)]]

gArea(nuts2.spdf, byid=TRUE)


lignes <- fread("lines.csv", dec=",", sep=";")
lignes[,`:=`(x_debut=as.numeric(gsub(",",".",x_debut)),y_debut=as.numeric(gsub(",",".",y_debut)),
            x_fin=as.numeric(gsub(",",".",x_fin)),y_fin=as.numeric(gsub(",",".",y_fin)))]
lignes <- lignes[!is.na(x_fin)&!is.na(x_debut)]

x0 <- -2.0037508342787E7
y0 <- 2.0037508342787E7
cord.UTM <- SpatialPoints(cbind(lignes$x_debut, lignes$y_debut), proj4string=CRS("+proj=epsg:3857"))
cord.dec <- spTransform(cord.UTM, CRS("+proj=longlat"))
cord.dec

scale<-list(x=1.31963E-05,
            x0=-6.389136388,
            y=8.80813E-06,
            y0=-11.7446582)
s <- c(scale$x, scale$y)
s0 <- c(scale$x0, scale$y0)


lst <- lapply(1:nrow(lignes), function(i) {
  SpatialLines(
    list(
      Lines(
        list(
          Line(rbindlist(
            list(lignes[i,c("x_debut","y_debut"),with=FALSE]*s+s0,
                 lignes[i,c("x_fin","y_fin"),with=FALSE]*s+s0)))), ID = i)), 
               proj4string = CRS("+init=epsg:4326"))
})

sln <- do.call("bind", lst)

leaflet(sln) %>% addTiles()

postes <- rbind(lignes[,.(nom=début, x=x_debut, y=y_debut)],
                lignes[,.(nom=fin, x=x_fin, y=y_fin)])
setkey(postes, nom)
postes <- unique(postes)
postes[,x:=x*scale$x+scale$x0]
postes[,y:=y*scale$y+scale$y0]

leaflet(postes) %>% addTiles() %>% addMarkers(lng=~x, lat=~y, popup=~nom)



mapview(sln)

library(leaflet)
library(ggmap)

somePlace <-ggmap::geocode("Vienna")  

leaflet(somePlace) %>% addTiles() %>% addMarkers()

m<-leaflet() %>% addTiles()

m %>%  addMarkers( lng =  ~x*scale$x+scale$x0, lat = ~y*scale$y+scale$y0 , popup=~LIBELLE)


