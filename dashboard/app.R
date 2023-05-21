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
library(fontawesome)
library(thematic)
library(tidyverse)
library(scales)
library(sf)
library(mapSpain)
library(ggrepel)

conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::lag,
  plotly::layout
)

# Get ATR data
load("atr_joined.RData")

# Defining color scale
soda_colors <- okabe_ito(5)[order(c(5, 2, 4, 3, 1))]
# Create color look-up
sectors <- atr$sector |> levels()
color_lu <- setNames(soda_colors, sectors)
# Create bootswatch class look-up
soda_classes <- c("bg-light", "bg-success", "bg-danger", "bg-info", "bg-warning")
class_lu <- setNames(soda_classes, sectors)

# Defining the theme
soda_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  light = color_lu[["Total"]],
  success = color_lu[["Agriculture"]],
  info = color_lu[["Construction"]],
  warning = color_lu[["Services"]],
  danger = color_lu[["Industry"]]
)

# Set global ggplot theme
theme_set(theme_minimal())
# Enable auto theming
thematic_shiny(font = "auto", qualitative = soda_colors)

# Get Spanish ACs as simple features
ccaa <- esp_get_ccaa() |> st_cast("MULTIPOLYGON")
can_box <- esp_get_can_box()

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
  selected = max(atr$year)
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
    "input.nav === 'CCAA Analysis'",
    ccaa_picker
  ),
  conditionalPanel(
    "input.nav === 'Map'",
    year_picker,
    sector_picker,
  )
)

analysis_view <- page_fillable(
  padding = 0,
  uiOutput("boxes"),
  p(),
  layout_column_wrap(
    width = 1/2,
    height = "600px",
    card(card_header("Number of work accidents"),
      plotOutput("atr_plot")
    ),
    card(card_header("Annual change of work accidents"),
      plotOutput("atr_yoy_plot")
    )
  )
)

map_view <- page_fillable(
  padding = 0,
  h4("Analyzing the work accidents in Spain"),
  p(),
  card(full_screen = TRUE,
    card_header("Map of work accidents in Spain"),
    card_body(
      height = "600px",
      class = "p-0",
      plotOutput("map_plot")
    )
  )
)

ui <- page_navbar(
    title = "SODA Dashboard",
    id = "nav",
    theme = soda_theme,
    sidebar = cond_sidebar,
    nav_panel("CCAA Analysis", analysis_view),
    nav_panel("Map", map_view)
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Map plot
  output$map_plot <- renderPlot({
    # Filter work accidents data
    atr_filtered <- atr |>
      filter(year == input$year_select,
             ccaa != "ES",
             sector == input$sector_select) |>
      mutate(ccaa = as.character(ccaa))
    
    # Join with SF data
    ccaa_atr <- ccaa |>
      inner_join(atr_filtered, by = join_by(iso2.ccaa.code == ccaa))
    
    # Create plot
    ggplot(ccaa_atr) +
      # Plot ACs
      geom_sf(aes(fill = accidents),
              color = "grey70",
              linewidth = .3) +
      # Plot canaries box
      geom_sf(data = can_box, color = "grey70") +
      # Plot labels with accident number
      geom_label_repel(
        aes(label = round(accidents), geometry = geometry),
        stat = "sf_coordinates",
        fill = alpha(c("white"), 0.5),
        color = "black",
        size = 3,
        label.size = 0
      ) +
      # Adjust color scale
      scale_fill_gradient(low = color_lu[[input$sector_select]],
                          high = "white") +
      # theme_void() +
      theme(legend.position = c(0.12, 0.6)) +
      labs(x = NULL,
           y = NULL,
           fill = "Accidents per\n100k workers")
  })
  
  # ATR plot
  output$atr_plot <- renderPlot({
    atr |>
      filter(ccaa_name == input$ccaa_select) |>
      ggplot(aes(x = date, y = accidents, color = sector)) +
      geom_line() +
      geom_point(size=2) +
      labs(x = "Year",
           y = "Work accidents per 100k workers")
  })
  
  # ATR YoY change plot
  output$atr_yoy_plot <- renderPlot({
    atr |>
      filter(ccaa_name == input$ccaa_select) |>
      drop_na() |>
      ggplot(aes(x = date, y = acc_yoy, color = sector)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = label_percent()) +
      labs(x = "Year",
           y = "Year over year change")
  })
  
  output$boxes <- renderUI({
    description <- p("Annual change from last year")
    yoy_latest <- atr |>
      # Get named vector of latest yoy
      filter(year == max(year),
             ccaa_name == input$ccaa_select) |>
      select(sector, acc_yoy) |>
      deframe()
    
    layout_column_wrap(
      width = "350px",
      value_box(
        title = "Total",
        class = class_lu[["Total"]],
        value = percent(yoy_latest[["Total"]]),
        showcase = fa("chart-line", height = "50px"),
        description
      ),
      value_box(
        title = "Agriculture",
        class = class_lu[["Agriculture"]],
        value = percent(yoy_latest[["Agriculture"]]),
        showcase = fa("wheat-awn", height = "50px"),
        description
      ),
      value_box(
        title = "Industry",
        class = class_lu[["Industry"]],
        value = percent(yoy_latest[["Industry"]]),
        showcase = fa("screwdriver-wrench", height = "50px"),
        description
      ),
      value_box(
        title = "Construction",
        class = class_lu[["Construction"]],
        value = percent(yoy_latest[["Construction"]]),
        showcase = fa("helmet-safety", height = "50px"),
        description
      ),
      value_box(
        title = "Services",
        class = class_lu[["Services"]],
        value = percent(yoy_latest[["Services"]]),
        showcase = fa("store", height = "50px"),
        description
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)