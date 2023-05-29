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
library(ggfx)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select()
)

# Get ATR data
load("atr_joined.RData")

# Defining color scale
soda_colors <- okabe_ito(5)[order(c(5, 2, 4, 3, 1))]
# Create color look-up
sectors <- atr$sector_en |> levels()
color_lu <- setNames(soda_colors, sectors)
# Create bootswatch class look-up
soda_classes <-
  c("bg-light", "bg-success", "bg-danger", "bg-info", "bg-warning")
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
  selected = "Comunidad de Madrid"
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
  choices = atr$sector_en |> levels(),
  selected = 1
)

# Defining the sidebars
page_sidebar <- sidebar(ccaa_picker)
map_sidebar <- sidebar(year_picker, sector_picker, position = "right")

# Defining the layout of the two accident plots side-by-side
acc_plots <- layout_column_wrap(
  width = "400px",
  height = "300px",
  card(card_header("Number of work accidents"),
       card_body(plotOutput("atr_plot"))
  ),
  card(card_header("Annual change of work accidents"),
       card_body(plotOutput("atr_yoy_plot"))
  )
)

# Defining the map plot
map_plot <- card(
  full_screen = TRUE,
  card_header("Number of work accidents"),
  layout_sidebar(
    sidebar = map_sidebar,
    plotOutput("map_plot")
  )
)

# Defining the page
page_view <- page_fillable(
  h2("Work accidents in Spain after autonomous community"),
  uiOutput("boxes"),
  p(),
  layout_column_wrap(
    width = "600px",
    acc_plots,
    map_plot
  )
  
)

# Defining the footer
foot <- page_fillable(
  hr(),
  p("2023 Luis Gentner | Data from the Spanish",
    a("Ministry of Labour and Social Economy",
      href = "https://www.mites.gob.es/estadisticas/eat/welcome.htm"),
    ".", style = "font-size:14px"
  )
)

ui <- page_navbar(
    title = "SODA Dashboard",
    fillable = "Dashboard",
    theme = soda_theme,
    inverse = FALSE,
    sidebar = page_sidebar,
    footer = foot,
    nav_panel("Regional analysis", page_view)
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Map plot
  output$map_plot <- renderPlot({
    # Filter accident data according to selection
    atr_filtered <- atr |>
      filter(year == input$year_select,
             sector_en == input$sector_select) |>
      mutate(ccaa = as.character(ccaa))
    # Merge SF with accidents
    ccaa_atr <- ccaa |>
      inner_join(atr_filtered, by = join_by(iso2.ccaa.code == ccaa))
    # SF of selected AC (empty if ccaa_name == España)
    ccaa_atr_select <- ccaa_atr |> filter(ccaa_name == input$ccaa_select)
    ggplot() +
      # Plot AC and fill according to accident rate
      geom_sf(data = ccaa_atr,
              aes(fill = accidents),
              color = "grey70",
              linewidth = .3) +
      # Highlight selected AC
      with_outer_glow(
        expand = 1,
        geom_sf(data = ccaa_atr_select,
                aes(fill = accidents))
      ) +
      # Plot Canaries box
      geom_sf(data = can_box, color = "grey70") +
      # Plot label
      geom_label_repel(
        data = ccaa_atr_select,
        aes(label = paste0(input$ccaa_select, ":\n", round(accidents)),
            geometry = geometry),
        stat = "sf_coordinates",
        fill = alpha(c("white"), 0.8),
        color = "black",
        size = 3,
        # label.size = 0,
        nudge_x = 1.5,
        nudge_y = 1,
        min.segment.length = 0
      ) +
      scale_fill_gradient(high = color_lu[[input$sector_select]],
                          low = "white") +
      theme(legend.position = c(0.12, 0.6)) +
      labs(x = NULL,
           y = NULL,
           fill = "Accidents per\n100k workers")
  })

  # ATR plot
  output$atr_plot <- renderPlot({
    atr |>
      filter(ccaa_name == input$ccaa_select) |>
      ggplot(aes(x = date, y = accidents, color = sector_en)) +
      geom_line() +
      geom_point(size = 2) +
      scale_x_date(date_minor_breaks = "years") +
      theme(legend.position = "bottom") +
      labs(x = "Year", color = "Sector",
           y = "Work accidents per 100k workers")
  })

  # ATR YoY change plot
  output$atr_yoy_plot <- renderPlot({
    atr |>
      filter(ccaa_name == input$ccaa_select) |>
      drop_na() |>
      ggplot(aes(x = date, y = acc_yoy, color = sector_en)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_line() +
      geom_point() +
      scale_x_date(date_minor_breaks = "years") +
      scale_y_continuous(labels = label_percent()) +
      theme(legend.position="bottom") +
      labs(x = "Year", color = "Sector",
           y = "Year over year change")
  })

  # Value boxes
  output$boxes <- renderUI({
    description <- p("Annual change from last year")
    yoy_latest <- atr |>
      # Get named vector of latest yoy
      filter(year == max(year),
             ccaa_name == input$ccaa_select) |>
      select(sector_en, acc_yoy) |>
      deframe()

    layout_column_wrap(
      width = "250px",
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