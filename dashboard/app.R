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

library(conflicted)
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(sf)
library(mapSpain)
library(ESdata)
library(leaflet)

conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::lag,
  plotly::layout
)

# Get ATR data
load("../data/ATR/ATR-I.1.3.RData")


# Get Spanish ACs as simple features
ccaa <- esp_get_ccaa() |> st_cast("MULTIPOLYGON")
can_box <- esp_get_can_box()

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
  label = "Select autonomous community:",
  choices = atr$ccaa_name |> levels(),
  selected = 1
)

# Defining the Year picker
year_picker <- sliderTextInput(
  inputId = "year_select",
  label = "Select year:", 
  choices = atr$year |> unique(),
  selected = 2021
)

# Defining the sector picker
sector_picker <- selectInput(
  "sector_select",
  label = "Select sector:",
  choices = atr$sector |> levels(),
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
    card(full_screen = TRUE,
      card_header("Number of work accidents in Spain"),
      plotOutput("index_plot")
    ),
    card(full_screen = TRUE,
      card_header("Annual change of work accidents in Spain"),
      card_body(class = "p-0",
        "Plot"
      )
    )
  )
)

map_view <- page_fillable(
  padding = 0,
  h4("Analyzing the work accidents in Spain"),
  p(),
  card(full_screen = TRUE,
    card_header("A filling map"),
    card_body(class = "p-0",
      plotlyOutput("map_plot")
    )
  )
)


ui <- page_navbar(
    title = "SODA Dashboard",
    id = "nav",
    theme = bs_theme(
      version = 5,
      bootswatch = "minty"
    ),
    sidebar = cond_sidebar,
    nav_panel("Analysis", analysis_view),
    nav_panel("Map", map_view)
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map_plot <- renderPlotly({
    atr_filtered <- atr |>
      filter(year == input$year_select,
             ccaa != "ES",
             sector == input$sector_select) |>
      mutate(ccaa = as.character(ccaa))
    ccaa_atr <- ccaa |>
      inner_join(atr_filtered, by = join_by(iso2.ccaa.code == ccaa))
    p <- ggplot(ccaa_atr) +
      geom_sf(aes(fill = accidents),
                  #text = paste0(ccaa_name, ":\n", round(accidents))),
              color = "grey70",
              linewidth = .3) +
      geom_sf(data = can_box, color = "grey70") +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      theme_minimal() +
      labs(fill = "Accidents per\n100k workers")
    
    ggplotly(p) |> style(hoveron = "fill")
  })
  
  output$index_plot <- renderPlot({
    atr |>
      filter(ccaa_name == input$ccaa_select) |>
      ggplot(aes(x = date, y = accidents, color = sector)) +
      geom_line() +
      geom_point(size=2) +
      labs(x = "Year",
           y = "Work accidents per 100k workers")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)