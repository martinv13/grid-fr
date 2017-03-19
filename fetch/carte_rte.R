

layers <- lapply(0:10, function(l) {
  
  print(paste("fetching layer",l))
  
  url <- paste0("https://services1.arcgis.com/x2oYdXVT265NhFqt/arcgis/rest/",
                "services/Carte_du_r%C3%A9seau_RTE_BV_07_2016/FeatureServer/", l,
                "/query")
  params <- list(where = "OBJECTID IS NOT NULL",
                 returnIdsOnly = TRUE,
                 f = "json")
  quer <- GET(url, query = params )
  ids <- fromJSON(content(quer, "text"))$objectIds
  
  ids <- split(ids,  ceiling(seq_along(ids)/100))
  
  lapply(ids, function(idsc) {
    print("chunk")
    params <- list(where = paste0("OBJECTID IN (",
                                  paste0(idsc, collapse = ","),
                                  ")"),
                   returnGeometry = TRUE,
                   outFields = "*",
                   f = "json")
    quer <- GET(url, query = params )
    fromJSON(content(quer, "text"))
  })
})

save(layers, file="carte_rte.RData")

View(layers[[6]]$`1`$features$attributes)



