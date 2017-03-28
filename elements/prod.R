
groupes <- fread("groupes.csv")

postes_racco <- subset(spl_postes, spl_postes$codeNat %in% groupes$poste_racco)
postes_racco <- spTransform(postes_racco, "+init=epsg:2154")
fr <- spTransform(fr, "+init=epsg:2154")


match(groupes$poste_racco, postes_racco$codeNat)

postes_racco$nom[26]

infos_groupes <- extract(fr, postes_racco, sp=TRUE, method="bilinear")

postes_racco <- spTransform(postes_racco, "+init=epsg:4326")
leaflet(postes_racco) %>% addTiles() %>% addMarkers(popup=~nom)