# Customize Shiny Layout
# 
# DOWNLOAD ALL THE R SCRIPTS: FelixAnalytix.com

# Single Page Layout ------------------------------------------------------

library(shiny)
library(tidyverse)
library(gt)

ui <- fluidPage(
  titlePanel("Starwars Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "character", 
        label = "Choose a Character:", 
        choices = unique(starwars$name),
        selected = unique(starwars$name)[1],
        multiple = TRUE
      )
    ),
    mainPanel(
      gt::gt_output("characterInfos")
    )
  )
)
server <- function(input, output) {
  output$characterInfos <- gt::render_gt({
    dplyr::starwars %>% 
      filter(name %in% input$character) %>%
      gt() %>%
      opt_interactive()
  })
}
shinyApp(ui = ui, server = server)


# Multi-rows --------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Starwars Dashboard"),
  fluidRow(
    column(6, plotOutput("plot1")),
    column(6, plotOutput("plot2"))
  ),
  fluidRow(
    column(12, gt::gt_output("table"))
  )
)
server <- function(input, output) {
  output$plot1 <- renderPlot({ 
    starwars %>%
      count(hair_color, gender) %>%
      ggplot(aes(hair_color, n, fill = gender)) + 
      geom_col()
    })
  output$plot2 <- renderPlot({ 
    starwars %>%
      count(eye_color, gender) %>%
      ggplot(aes(eye_color, n, fill = gender)) + 
      geom_col()
    })
  output$table <- gt::render_gt({ starwars })
}
shinyApp(ui = ui, server = server)


# Tabsets -----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Starwars Dashboard"),
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Summary", verbatimTextOutput("summary")),
    tabPanel("Data", gt::gt_output("table"))
  )
)
server <- function(input, output) {
  output$table <- gt::render_gt({ starwars })
  output$summary <- renderPrint({ summary(starwars$height) })
  output$plot <- renderPlot({ ggplot(starwars, aes(x = height, fill = gender)) + 
      geom_density(alpha = 0.5) })
}
shinyApp(ui = ui, server = server)

# Multiple pages with a navbar --------------------------------------------

ui <- navbarPage(
  title = "Starwars Dashboard",
  tabPanel("Introduction", p("An app for analyzing Starwars data.")),
  tabPanel("Data", gt::gt_output("dataTable")),
  navbarMenu("More Analyses",
             tabPanel("Height Analysis", plotOutput("heightPlot")),
             tabPanel("Mass Analysis", plotOutput("massPlot"))
  )
)
server <- function(input, output) {
  output$dataTable <- gt::render_gt({ gt(starwars) |> opt_interactive() })
  output$heightPlot <- renderPlot({ ggplot(starwars, aes(height)) + geom_histogram() })
  output$massPlot <- renderPlot({ ggplot(starwars, aes(mass)) + geom_histogram() })
}
shinyApp(ui = ui, server = server)

# Using Bootstrap with R Shiny --------------------------------------------

library(bslib)
ui <- navbarPage(
  title = "Starwars Dashboard",
  theme = bslib::bs_theme(bootswatch = "cerulean"), # BOOTSWATCH THEME
  tabPanel("Introduction", p("An app for analyzing Starwars data.")),
  tabPanel("Data", gt::gt_output("dataTable")),
  navbarMenu("More Analyses",
             tabPanel("Height Analysis", plotOutput("heightPlot")),
             tabPanel("Mass Analysis", plotOutput("massPlot"))
  )
)
server <- function(input, output) {
  output$dataTable <- gt::render_gt({ gt(starwars) |> opt_interactive() })
  output$heightPlot <- renderPlot({ ggplot(starwars, aes(height)) + geom_histogram() })
  output$massPlot <- renderPlot({ ggplot(starwars, aes(mass)) + geom_histogram() })
}
shinyApp(ui = ui, server = server)

# Theming your R Shiny app with bslib -------------------------------------

library(bslib)
ui <- fluidPage(
  theme = bslib::bs_theme(bg = "#E8E8E8", fg = "#242424", primary = "#D4B836"),
  titlePanel("Starwars Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("massInput", "Select Maximum Mass:",
                  min = min(starwars$mass, na.rm = TRUE),
                  max = max(starwars$mass, na.rm = TRUE),
                  value = 40)
    ),
    mainPanel(
      plotOutput("massPlot")
    )
  )
)
server <- function(input, output) {
  output$massPlot <- renderPlot({
    starwars %>%
      filter(mass <= input$massInput) %>%
      ggplot(aes(x = name, y = mass)) +
      geom_col()
  })
}
shinyApp(ui = ui, server = server)
