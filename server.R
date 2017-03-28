

library(shiny)

shinyServer(function(input, output) {
  
  
  output$carte <- renderLeaflet({
    
    leaflet()  %>% addTiles() %>% 
      addPolylines(data=spl_lines, color=c("#ff0000","#00ffff"), opacity=1) %>%
      addCircles(data=spl_postes, radius=20, color="#00ff00", 
                 opacity=1, fillOpacity = 1, popup=~nom )
    
  })
  
})
