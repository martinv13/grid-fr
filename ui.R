
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(3,
           sliderInput("scaleX",NULL,
                  min = 0.1,
                  max = 2,
                  value = scaleX),
           sliderInput("scaleY",NULL,
                       min = 0.1,
                       max = 2,
                       value = scaleY),
           sliderInput("x0",NULL,
                       min = -100,
                       max = 100,
                       value = x0),
           sliderInput("y0",NULL,
                       min = -100,
                       max = 100,
                       value = y0)
           
    ),

    # Show a plot of the generated distribution
    column(9,
      leafletOutput("carte", height = 900)
    )
  )
))
