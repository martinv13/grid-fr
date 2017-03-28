
lignes <- fread("lines.csv", dec=",", sep=";")

postes <- rbindlist(list(lignes[,.(nom=début,codeNat=codeNat_debut, x=x_debut, y=y_debut)],
                         lignes[,.(nom=fin,codeNat=codeNat_fin, x=x_fin, y=y_fin)]))
setkey(postes, codeNat)
postes <- unique(postes)

spl_400 <- apply(lignes[tension=="400kV",], 1, function(ligne){
  ligne <- as.list(ligne)
  Line(cbind(as.numeric(c(ligne$x_debut, ligne$x_fin)),
             as.numeric(c(ligne$y_debut, ligne$y_fin))))
}) %>% Lines(ID = "400kV")

spl_225 <- apply(lignes[tension=="225kV",], 1, function(ligne){
  ligne <- as.list(ligne)
  Line(cbind(as.numeric(c(ligne$x_debut, ligne$x_fin)),
             as.numeric(c(ligne$y_debut, ligne$y_fin))))
}) %>% Lines(ID = "225kV")

spl_lines <- SpatialLines(list(spl_400, spl_225), proj4string=CRS("+init=epsg:2154"))
spl_lines <- spTransform(spl_lines, "+init=epsg:4269")

spl_postes <- SpatialPointsDataFrame(cbind(postes$x, postes$y), postes, proj4string=CRS("+init=epsg:2154"))
spl_postes <- spTransform(spl_postes, "+init=epsg:4269")
