# Get the R script: FelixAnalytix.com

# Install R packages if not installed -------------------------------------

if (!require(shiny)) install.packages("shiny")
if (!require(bs4Dash)) install.packages("bs4Dash")

# Empty skeleton ----------------------------------------------------------

library(shiny)
library(bs4Dash)

ui <- dashboardPage(
  title = "Basic Dashboard",
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody()
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

# Full example ------------------------------------------------------------

ui <- dashboardPage(
  title = "Basic Dashboard",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "bs4Dash",
      color = "primary",
      href = "https://felixanalytix.com",
      image = "https://felixanalytix.com/about/image-profile.jpg",
    ),
    skin = "light",
    status = "white"
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarUserPanel(
      image = NULL,
      name = "Welcome Onboard!"
    ),
    sidebarMenu(
      sidebarHeader("Header 1"),
      menuItem(
        "Item 1",
        tabName = "item1",
        icon = icon("sliders")
      ),
      menuItem(
        "Item 2",
        tabName = "item2",
        icon = icon("chart-simple")
      )
    )
  ),
  footer = dashboardFooter(
    left = p("Download the R code: ", a(
      href = "https://felixanalytix.com",
      target = "_blank", "felixanalytix.com"
    ))
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "item1",
        fluidRow(
          lapply(1:3, FUN = function(i) {
            sortable(
              width = 4,
              p(class = "text-center", paste("Column", i)),
              lapply(1:2, FUN = function(j) {
                box(
                  title = paste0("I am the ", j, "-th card of the ", i, "-th column"),
                  width = 12,
                  "Click on my header"
                )
              })
            )
          })
        )
      ),
      tabItem(
        tabName = "item2",
        box(
          title = "Card with charts",
          width = 9
        )
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
