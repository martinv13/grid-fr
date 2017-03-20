
regions <- c("Grand-Est",
             "Nouvelle-Aquitaine",
             "Auvergne-Rhône-Alpes",
             "Bourgogne-Franche-Comté",
             "Bretagne",
             "Centre-Val-de-Loire",
             "Ile-de-France",
             "Occitanie",
             "Hauts-de-France",
             "Normandie",
             "Pays-de-la-Loire",
             "PACA")

eco2mix <- lapply(regions, function(rg){
  # url <- URLencode(paste0("http://eco2mix.rte-france.com/download/eco2mix/eCO2mix_RTE_",
  #               rg, "_En-cours-Consolide.zip"))
  # tpfile <- tempfile()
  # download.file(url, destfile = tpfile)
  # tpdir <- tempdir()
  # unzip(tpfile, exdir = tpdir)
  # unlink(tpfile)
  fread(paste0("data/eCO2mix_RTE_", rg, "_En-cours-Consolide.xls"), fill=TRUE, sep="\t", dec=",")
})
names(eco2mix) <- regions
eco2mix <- rbindlist(eco2mix, idcol = "region")
eco2mix <- eco2mix[!is.na(Consommation)]
eco2mix[,Date:=as.POSIXct(paste(Date, Heures), format="%Y-%m-%d %H:%M")]


dir(tpdir)

