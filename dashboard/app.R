#
# SODA
# Spanish Occupational Accidents Data Analysis
# Shiny Dashboard
# 
# Author: Luis Gentner
# Created: 2023-05-20
#
# bslib 0.4.2.9000+ required
# Install with devtools::install_github("rstudio/bslib")


library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(leaflet)
library(shinyWidgets)

plotly_widget <- plot_ly(x = diamonds$cut) %>%
  config(displayModeBar = FALSE) %>%
  layout(margin = list(t = 0, b = 0, l = 0, r = 0))

leaflet_widget <- leafletOptions(attributionControl = FALSE) %>%
  leaflet(options = .) %>%
  addTiles()

# Defining value boxes on Analysis page
boxes <- layout_column_wrap(
  width = "250px",
  value_box(
    title = "1st value", 
    value = "123",
    showcase = bs_icon("bar-chart"),
    p("The 1st detail")
  ),
  value_box(
    title = "2nd value", 
    value = "456",
    showcase = bs_icon("graph-up"),
    p("The 2nd detail"),
    p("The 3rd detail")
  ),
  value_box(
    title = "3rd value", 
    value = "789",
    showcase = bs_icon("pie-chart"),
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)

# Defining the CCAA picker
ccaa_picker <- selectInput(
  "ccaa_select",
  label = "Select autonomous community",
  choices = c(
    "Choice 1",
    "Choice 2",
    "Choice 3"
  ),
  selected = 1
)

# Defining the Year picker
year_picker <- sliderTextInput(
  inputId = "year_select",
  label = "Select a year:", 
  choices = month.name
)

# Defining the sector picker
sector_picker <- selectInput(
  "sector_select",
  label = "Select sector",
  choices = c(
    "Choice 1",
    "Choice 2",
    "Choice 3"
  ),
  selected = 1
)

# Defining the conditional sidebar
cond_sidebar <- sidebar(
  conditionalPanel(
    "input.nav === 'Analysis'",
    ccaa_picker
  ),
  conditionalPanel(
    "input.nav === 'Map'",
    year_picker,
    sector_picker
  )
)

analysis_view <- page_fillable(
  padding = 0,
  h4("Analyzing the work accidents in Spain"),
  boxes,
  p(),
  layout_column_wrap(
    width = 1/2,
    height = 300,
    card(full_screen = TRUE, card_header("A filling plot"), plotly_widget),
    card(full_screen = TRUE, card_header("A filling map"), card_body(class = "p-0", leaflet_widget))
  )
)

map_view <- page_fillable(
  padding = 0,
  h4("Analyzing the work accidents in Spain"),
  p(),
  card(full_screen = TRUE, card_header("A filling map"), card_body(class = "p-0", leaflet_widget))
)


ui <- page_navbar(
    title = "SODA Dashboard",
    id = "nav",
    theme = bs_theme(
      version = 5,
      bootswatch = "minty"
    ),
    sidebar = cond_sidebar,
    nav_panel("Analysis",
      analysis_view),
    nav_panel("Map",
      map_view)
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)