
regions <- unique(inter$region )

# lecture des fichiers eco2mix
eco2mix <- lapply(regions, function(rg){
  fread(paste0("data/eCO2mix_RTE_", rg, "_En-cours-Consolide.xls"), fill=TRUE, sep="\t", dec=",")
})

names(eco2mix) <- regions
eco2mix <- rbindlist(eco2mix, idcol = "region")
eco2mix <- eco2mix[!is.na(Consommation)]
eco2mix[,Date:=as.POSIXct(paste(Date, Heures), format="%Y-%m-%d %H:%M")]

eco2mix <- eco2mix[,c(1,4,6:14), with=FALSE]

eco2mix <- melt(eco2mix, id.vars =  1:2, measure.vars = 3:11)
eco2mix[,value := as.numeric(value)]

date <- as.POSIXct("2017-02-13 19:00:00")

conso <- eco2mix[Date==date & variable=="Consommation",.(region, conso=value)]

inter$conso <- conso$conso[match(inter$region, conso$region)]*inter$cle_conso

poly_conso <- as.data.table(inter)[,.(conso=sum(conso)),
                                   by=.(nom,codnat)]

voronoi$conso <- poly_conso$conso[match(voronoi$codnat, poly_conso$codnat)]

pal <- colorNumeric(
  palette = "Blues",
  domain = depts$conso)

leaflet(voronoi) %>% addTiles() %>% 
  addPolygons(popup=~nom, color=~pal(conso), stroke=FALSE,
              fillOpacity = .5)


