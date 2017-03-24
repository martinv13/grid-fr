
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
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
  
  output$carte <- renderLeaflet({
    
    leaflet()  %>% addTiles() %>% 
      addPolygons(data=spl_zones) %>%
      addPolylines(data=spl_lines, color=c("#ff0000","#00ffff"), opacity=1) %>%
      addCircles(data=spl_postes, radius=20, color="#00ff00", opacity=1, fillOpacity = 1, label=~nom )
    
    
  })
  
  observe({x0<<-input$x0})
  observe({y0<<-input$y0})
  observe({scaleX<<-input$scaleX})
  observe({scaleY<<-input$scaleY})
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
