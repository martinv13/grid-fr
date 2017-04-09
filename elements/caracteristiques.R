
load("topologie.RData")
postes <- reseau$postes
lignes <- reseau$lignes

# correspondance avec les données élec des lignes
lines_car <- fread("lignes_raw.csv", dec=",")
lines_car <- lignes[,.(`Identifiant géographique / Asset location`=ADR_LIT,
                          IDR_LIT,matched=ADR_LIT)][lines_car, 
                                                            on="Identifiant géographique / Asset location", mult="first"]
lines_car[is.na(matched),
          c("adr","matched","IDR_LIT","matched_long","dist"):= {
            id_dist <- adist(`Identifiant géographique / Asset location`,lignes$ADR_LIT)
            id_dist[is.na(id_dist)] <- 1000
            id_min <- apply(id_dist, 1, which.min)
            id_dist <- apply(id_dist, 1, min)
            .(`Identifiant géographique / Asset location`,
              lignes$ADR_LIT[id_min],
              lignes$IDR_LIT[id_min],
              lignes$long[id_min],
              id_dist)
          }]
lines_car[!is.na(dist) & abs(matched_long/`Longueur / length (km)`-1)>.2,
          IDR_LIT:=NA]

# correspondance des transfos
postes_car <- fread("transfos_raw.csv", dec=",")
postes_car[,adr:=substr(`Identifiant géographique / Asset location`,23,200)]

postes_car <- postes[,.(CODNAT=CODNAT, adr=ADR_SITE)][
  postes_car, on="adr", mult="first"]

lignes <- lines_car[,.(IDR_LIT, 
                       X=`Xd (Ohm)`,
                       long_X=`Longueur / length (km)`,
                       IMAP_E=`IST_E1 (Amp)`,
                       IMAP_IS1=`IST_IS1 (Amp)`,
                       IMAP_IS2=`IST_IS2 (Amp)`,
                       IMAP_H=`IST_H1 (Amp)`)][lignes, on="IDR_LIT", mult="first"]

lignes[U_MAX==7, .(x=IMAP_H , y=X/long_X, g=CONFIG)] %>%
  ggplot(aes(y=y,x=x,group=g,colour=g)) + geom_point()




postes_400 <- unique(as.vector(unlist(merged[U_MAX==7, .(code_start,code_end)])))
postes_225 <- unique(as.vector(unlist(merged[U_MAX==6, .(code_start,code_end)])))

postes2[,l400:=CODNAT %in% postes_400]
postes2[,l225:=CODNAT %in% postes_225]
postes2[l400 & U_MAX==6]
postes2[!l400 & U_MAX==7]

postes2[transfo]

View(postes2[!transfo & U_MAX==7,])




spl_lignes <- apply(lignes, 1, function(ligne){
  ligne <- as.list(ligne)
  Lines(Line(cbind(ligne$paths[,,1],ligne$paths[,,2])),
        ID=ligne$IDR_LIT)
}) %>% SpatialLines(proj4string=CRS("+init=epsg:2154"))
setDF(lignes)
row.names(lignes) <- lignes$IDR_LIT
spl_lignes <- SpatialLinesDataFrame(spl_lignes, lignes)
setDT(lignes)
spl_lignes <- spTransform(spl_lignes, "+init=epsg:4326")

sca <- scale_color_continuous(limits=range(lignes[$IMAP_H,na.rm=TRUE),
                              low="green", 
                              high="red")
colrs <- 
  

leaflet() %>% addTiles() %>%
  addPolylines(spl_lignes)
