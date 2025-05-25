# Title: Create Beautiful Value Boxes using R
# Youtube video: https://youtu.be/jToYFUhDAxs?feature=shared

# R packages --------------------------------------------------------------

library(bslib)
library(plotly)
library(htmltools)
library(bsicons)
library(shiny)
library(bs4Dash)
library(htmlwidgets)

# Cards with bslib --------------------------------------------------------

# demo: https://bslib.shinyapps.io/build-a-box/

# Value boxes with quarto -------------------------------------------------

layout_columns(
  value_box(
    title = "Package Numbers", value = "409,446", shiny::markdown("asdfsd"),
    theme = "primary", showcase = "Your Plot", showcase_layout = "left center",
    full_screen = TRUE, fill = TRUE, height = NULL
  ),
  value_box(
    title = "AWS Cloud Spending", value = "$3,463", , theme = "success",
    showcase = "Your Plot", showcase_layout = "left center",
    full_screen = TRUE, fill = TRUE, height = NULL
  ),
  value_box(
    title = "Project Stars", value = "5,100", , theme = NULL,
    showcase = bsicons::bs_icon("star"), showcase_layout = "left center",
    full_screen = FALSE, fill = TRUE, height = NULL
  )
)

# https://rstudio.github.io/bslib/articles/value-boxes/

sparkline <- plot_ly(economics) %>%
  add_lines(
    x = ~date, y = ~psavert,
    color = I("white"), span = I(1),
    fill = 'tozeroy', alpha = 0.2
  ) %>%
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = ""),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "white"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent"
  ) %>%
  config(displayModeBar = FALSE) %>%
  htmlwidgets::onRender(
    "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
  )

value_box(
  title = "Personal Savings Rate",
  value = "7.6%",
  p("Started at 12.6%"),
  p("Averaged 8.6% over that period"),
  p("Peaked 17.3% in May 1975"),
  showcase = sparkline,
  showcase_layout = "bottom",
  full_screen = TRUE,
  theme = "success"
)

# Value boxes with shiny --------------------------------------------------

# ref: https://github.com/rstudio/bslib/blob/main/inst/examples-shiny/value_box/app.R

ui <- page_fluid(
  theme = bslib::bs_theme(
    base_font = font_google("Atkinson Hyperlegible")
  ),
  br(),
  layout_column_wrap(
    width = "200px",
    class = "mt-3",
    value_box(
      title = "Unemployment Rate",
      value = "2.7%",
      p("Started at 1.5%"),
      p("Averaging 3%"),
      p("Peaked at 5.2% in Dec 1982"),
      showcase = plotlyOutput("unemploy"),
      full_screen = TRUE
    ),
    value_box(
      title = "Personal Savings Rate",
      value = "7.6%",
      p("Started at 12.6%"),
      p("Averaging 8.6%"),
      p("Peaked at 17.3% in May 1975"),
      showcase = plotlyOutput("psavert"),
      showcase_layout = showcase_top_right(),
      full_screen = TRUE,
      theme = "success"
    ),
    value_box(
      title = "Personal Consumption",
      value = "$3.8B",
      p("Started at $0.25B"),
      p("Averaging $1.7B"),
      showcase = bsicons::bs_icon("piggy-bank", size = "100%"),
      full_screen = TRUE,
      theme = "danger"
    )
  )
)

server <- function(input, output) {
  output$unemploy <- renderPlotly({
    plotly_time_series(
      economics,
      x = ~date,
      y = ~ 100 * unemploy / pop
    )
  })
  
  output$psavert <- renderPlotly({
    plotly_time_series(
      economics,
      x = ~date,
      y = ~psavert
    )
  })
  
  output$pce <- renderPlotly({
    plotly_time_series(
      economics,
      x = ~date,
      y = ~ 100 * pce / pop
    )
  })
  
  plotly_time_series <- function(d, x, y) {
    info <- getCurrentOutputInfo()
    large <- isTRUE(info$height() > 200)
    
    plot_ly(d, x = x, y = y) %>%
      add_lines(
        color = I(info$fg()),
        span = I(1),
        #hoverinfo = if (!large) "none",
        fill = 'tozeroy',
        alpha = 0.2
      ) %>%
      layout(
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = info$fg()),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE)
  }
}

shinyApp(ui, server)

# Boxes with bs4Dash ------------------------------------------------------

# reference: https://bs4dash.rinterface.com/reference/valuebox

## advanced example with server side rendering
ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  title = "test",
  body = dashboardBody(
    fluidRow(
      valueBoxOutput("vbox")
    )
  )
)

server = function(input, output) {
  output$vbox <- renderValueBox({
    valueBox(
      value = 150,
      subtitle = "New orders",
      color = "primary",
      icon = icon("shopping-cart"),
      href = "#"
    )
  })
}

shinyApp(ui, server)

## advanced example with sparkline
ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  title = "bs4Dash value boxes",
  body = bs4DashBody(
    fluidRow(
      valueBox(
        value = 150,
        subtitle = "New orders",
        color = "primary",
        icon = icon("cart-shopping")
      ),
      valueBox(
        value = "53%",
        subtitle = "New orders",
        color = "indigo",
        icon = icon("gears"),
        footer = plotlyOutput("salesSparkline", height = "50px"),
        width = 4,
        elevation = 4
      ),
      valueBox(
        value = "44",
        subtitle = "User Registrations",
        color = "teal",
        icon = icon("sliders")
      )
    )
  )
)

server <- function(input, output) {
  
  output$salesSparkline <- renderPlotly({
    
    econ_data <- economics %>%
      tail(24) %>%  # Using last 24 months for visualization
      mutate(
        month_name = format(date, "%b %Y"),
        unemployment_millions = unemploy / 1000  # Convert to millions for cleaner display
      )
    
    plot_ly(
      data = econ_data, 
      x = ~date, 
      y = ~unemployment_millions, 
      type = "scatter", mode = "lines",
      line = list(color = "#ffffff", width = 2),
      text = ~paste("Date:", month_name, 
                    "<br>Unemployment:", round(unemployment_millions, 2), "M",
                    sep = ""),
      hoverinfo = "text") %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
        xaxis = list(
          showgrid = FALSE, 
          zeroline = FALSE, 
          showticklabels = FALSE,
          title = ""  # Removed x-axis title
        ),
        yaxis = list(
          showgrid = FALSE, 
          zeroline = FALSE, 
          showticklabels = FALSE,
          title = ""  # Removed y-axis title
        ),
        showlegend = FALSE,
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)

