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
library(scales)
library(plotly)
library(sf)
library(mapSpain)

conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::lag,
  plotly::layout
)

# Get ATR data
load("atr_joined.RData")


# Get Spanish ACs as simple features
ccaa <- esp_get_ccaa() |> st_cast("MULTIPOLYGON")
can_box <- esp_get_can_box()

# Defining value boxes on Analysis page
boxes <- layout_column_wrap(
  width = "250px",
  value_box(
    title = "Total", 
    value = textOutput("yoy_2021"),
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
  uiOutput("boxes"),
  p(),
  layout_column_wrap(
    width = 1/2,
    height = 600,
    card(full_screen = TRUE,
      card_header("Number of work accidents in Spain"),
      plotOutput("atr_plot")
    ),
    card(full_screen = TRUE,
      card_header("Annual change of work accidents in Spain"),
      card_body(class = "p-0",
        plotOutput("atr_yoy_plot")
      )
    )
  )
)

map_view <- page_fillable(
  padding = 0,
  h4("Analyzing the work accidents in Spain"),
  p(),
  card(full_screen = TRUE,
    card_header("Map of work accidents in Spain"),
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
  # Map plot
  output$map_plot <- renderPlotly({
    atr_filtered <- atr |>
      filter(year == input$year_select,
             ccaa != "ES",
             sector == input$sector_select) |>
      mutate(ccaa = as.character(ccaa))
    ccaa_atr <- ccaa |>
      inner_join(atr_filtered, by = join_by(iso2.ccaa.code == ccaa))
    p <- ggplot(ccaa_atr) +
      geom_sf(aes(fill = accidents,
                  text = paste0(ccaa_name, ":\n", round(accidents))),
              color = "grey70",
              linewidth = .3) +
      geom_sf(data = can_box, color = "grey70") +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      theme_minimal() +
      labs(fill = "Accidents per\n100k workers")

    ggplotly(p, tooltip = "text") |> style(hoveron = "fill")
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
    yoy <- atr |>
      filter(year == 2021,
             sector == "Total",
             ccaa_name == input$ccaa_select) |>
      pull(acc_yoy)
    layout_column_wrap(
      width = 1/2,
      value_box(
        title = "Total", 
        value = percent(yoy[1]),
        showcase = bs_icon("bar-chart"),
        p("Year over year change")
      ),
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Total", 
          value = percent(yoy[2]),
          showcase = bs_icon("bar-chart"),
          p("Year over year change")
        ),
        value_box(
          title = "Total", 
          value = percent(yoy[3]),
          showcase = bs_icon("bar-chart"),
          p("Year over year change")
        ),
        value_box(
          title = "Total", 
          value = percent(yoy[4]),
          showcase = bs_icon("bar-chart"),
          p("Year over year change")
        ),
        value_box(
          title = "Total", 
          value = percent(yoy[5]),
          showcase = bs_icon("bar-chart"),
          p("Year over year change")
        ))
    )
  })
  
  # Output of Last year changes
  output$yoy_2021 <- renderText({
    atr |>
      filter(year == 2021,
             sector == "Total",
             ccaa_name == input$ccaa_select) |>
      pull(acc_yoy)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)