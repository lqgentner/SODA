# ATR-I.1.3 data import

library(tidyverse)
library(readxl)
library(ESdata)

# Import data
atr <- read_excel("../data/atr_raw/ATR_2009-2021_clean.xlsx",
                  sheet = "ATR-I.1.3",
                  range = "A7:O102")

# Autonomous communities according to ESdata::ccaa_iso
renamed_ccaa = c(
  "España" = "total",
  "Principado de Asturias" = "Asturias (Principado de)",
  "Illes Balears" = "Balears (Illes)",
  "Castilla - La Mancha" = "Castilla-La Mancha",
  "Catalunya" = "Cataluña",
  "Comunidad Valenciana" = "Comunitat Valenciana",
  "Comunidad de Madrid" = "Madrid (Comunidad de)",
  "Región de Murcia" = "Murcia (Región de)",
  "Comunidad Foral de Navarra" = "Navarra (Comunidad Foral de)",
  "La Rioja" = "Rioja (La)"
)
# Data frame with translated sector names
sector_trans = tribble(
  ~sector_en, ~sector,
  "Total", "total",
  "Agriculture", "agricultura",
  "Industry", "industria",
  "Construction", "construccion",
  "Services", "servicios"
) |> mutate(across(everything(), as_factor))

atr <- atr |>
  # Factorize
  mutate(across(c(sector, region), as_factor)) |>
  # Drop autonomous cities
  filter(region != "Ceuta y Melilla") |>
  # Rename regions
  mutate(region = fct_recode(region, !!!renamed_ccaa)) |>
  # Add English sector names
  full_join(sector_trans, by = "sector") |>
  # Join with ISO names
  left_join(ccaa_iso,
            by = join_by(region == nombres)) |>
  # Rename region columns
  rename(ccaa = iso,
         ccaa_name = region,
         ccaa_label = label) |>
  # Long format like the other ESdata tables
  pivot_longer(where(is.numeric),
               names_to = "year",
               values_to = "accidents") |>
  # Format year as date, use middle of the year
  mutate(year = as.numeric(year),
         date = make_date(year, 6, 30)) |>
  # Change order
  relocate(sector,
           ccaa,
           date,
           accidents) |>
  # Drop empty factors
  mutate(across(starts_with("ccaa"), fct_drop))

# Save
save(atr, file = "../data/atr_clean.RData")

atr
