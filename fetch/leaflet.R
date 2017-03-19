library(leaflet)
library(dplyr)
library(magrittr)

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

postes 
View()


  
scale<-list(x=1.31963E-05,
            x0=-6.389136388,
            y=8.80813E-06,
            y0=-11.7446582)
            
m <- leaflet(data=centrales )  %>% addTiles()

m %>%  addMarkers( lng =  ~x*scale$x+scale$x0, lat = ~y*scale$y+scale$y0 , popup=~LIBELLE)
