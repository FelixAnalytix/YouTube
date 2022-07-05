# title: Make your R Shiny buttons look better with these 3 tips
# author: FelixAnalytix.com
# https://youtube.com/shorts/_zfdVvMywi4

library(shiny)
library(bslib)

ui <- navbarPage(
  title = "How to customize R Shiny buttons",
  theme = bs_theme(
    bootswatch = "united",
    success = "lightgreen",
    danger = "coral"
    ),
  actionButton(
    inputId = "my-input-id",
    label = "button name",
    class = "btn-danger",
    icon = icon("refresh")
  )
) 

server <- function(input, output){}

shinyApp(ui, server)
