library(bslib)
library(shiny)
library(crosstalk)
library(plotly)
library(leaflet)

load("data/ATR/ATR-I.1.3.RData")
ds_atr <- SharedData$new(atr)
filters_atr <- list(
  filter_slider("year", "Year", ds_atr, ~year),
  filter_select("ccaa", "CCAA", ds_atr, ~ccaa_name, multiple = FALSE)
)
plot_atr <- plot_ly(ds_atr) |> add_lines(x = ~date, y = ~accidents, color = ~sector)


atr_view <- layout_sidebar(
  sidebar = sidebar(
    title = "ATR filters",
    !!!filters_atr
  ),
  fillable = TRUE,
  border = FALSE,
  border_radius = FALSE,
  plot_atr
)

page_fillable(
  padding = 0,
  atr_view
)

# map filter and visual
quake_dat <- SharedData$new(quakes)
map_filter <- filter_slider("mag", "Magnitude", quake_dat, ~mag)
map_quakes <- leaflet(quake_dat) |>
  addTiles() |>
  addCircleMarkers()

page_navbar(
  title = "Sidebar demo",
  fillable = "Diamonds",
  nav_panel("ATR", atr_view)
)

