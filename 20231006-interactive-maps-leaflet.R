# title: interactive map with leaflet
# author: FelixAnalytix.com, YouTube tutorial: https://youtu.be/U0aJeaCnMeE

# GOAL: reproduce this map: https://www.bfs.admin.ch/asset/en/27205601

# Install and load R packages ---------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("BFS")) install.packages("BFS")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("sf")) install.packages("sf")
if (!require("knitr")) install.packages("knitr")
if (!require("htmltools")) install.packages("htmltools")

library(tidyverse)
library(BFS) # Get Swiss Federal Statistical Office data
library(leaflet)
library(leaflet.extras)
library(sf)
library(knitr) # for kable()
library(htmltools) # for HTLM()


# Get commun last names by commune ----------------------------------------

tables_names <- BFS::bfs_get_catalog_tables(
  language = "en", 
  title = "last+names")

asset_names_commune <- tables_names |>
  filter(str_detect(title, "commune")) |>
  pull(number_asset)

# only 1st asset
asset_names_commune <- asset_names_commune[1]

meta <- BFS::bfs_get_asset_metadata(
  number_asset = asset_names_commune, 
  language = "en")

# download and read file
tmp <- tempfile()

BFS::bfs_download_asset(
  number_asset = asset_names_commune, 
  destfile = tmp)

df <- read_csv(file = tmp) |>
  select(-TIME_PERIOD, -OBS_STATUS)


# Data cleaning -----------------------------------------------------------

# top 5 by commune by rank and pct
df_top_5 <- df |>
  arrange(desc(PCT_GDE)) |>
  group_by(GDENAME) |>
  slice(1:5) |>
  mutate(RANK = row_number()) |> 
  ungroup()

# create html table for each commune
create_table <- function(name) {
  df <- df_top_5 |> 
    filter(GDENAME == name)

  table <- df |> 
    select("Rank" = RANK, "Last name" = LASTNAME, 
           "Number" = VALUE, "Percentage" = PCT_GDE) |>
    mutate(Rank = paste0(Rank, "."),
           Percentage = paste0(Percentage, "%")) |>
    kableExtra::kable(format = "html", align = "llrr")
  
  paste0(
    "<b>", unique(df$GDENAME), "</b><br>",
    table
  )
}

create_table(name = "Aadorf")

# html table code in the `table` column
df_with_tables <- df_top_5 |> 
  filter(RANK == 1) |>
  mutate(table = map_chr(GDENAME, create_table, .progress = TRUE))

df_with_tables


# Get official Swiss base maps --------------------------------------------

communes_sf <- bfs_get_base_maps(geom = "polg")
lakes_sf <- bfs_get_base_maps(geom = "seen", category = "11") |>
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")

# join data with geodata
sf_communes_joined <- communes_sf |>
  left_join(df_with_tables, by = c("id" = "GDENR")) |>
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")

sf_communes_joined

# Create interactive map with leaflet -------------------------------------

# get bounding box of Switzerland
bbox <- st_bbox(sf_communes_joined) |>
  as.vector()

# customize legend 
bins = c(0, 1, 2.5, 5, 10, 20, 100)
col_custom = c("#f5b3bb", "#ef8894","#e85767","#dc0018","#a60013","#73000d")
pal <- colorBin(col_custom, 
                domain = sf_communes_joined$PCT_GDE, 
                bins = bins)
legend_labels <- c("0 – 1", "1 – 2.5", "2.5 – 5", 
                   "5 – 10", "10 – 20", "20 – 100")

map <- leaflet(sf_communes_joined, height = 600, width = 900) |>
  addPolygons(
    weight = 0.3,
    opacity = 1,
    color = "white",
    fillOpacity = 1,
    fillColor = ~pal(PCT_GDE),
    label = ~lapply(table, htmltools::HTML),
  ) |>
  addPolygons(
    data = lakes_sf, 
    label = ~name,
    stroke = FALSE,
    color = "gray70"
    ) |> 
  addLegend(
    title = "Share of the most<br>common surname, in %", 
    labFormat = function(type, cuts, p) { paste0(legend_labels) },
    values = ~PCT_GDE,
    pal = pal, 
    opacity = 1
  ) |>
  addTiles(urlTemplate = "", # empty background
           options = providerTileOptions(minZoom = 8, maxZoom = 12)) |>
  setMaxBounds(lng1 = bbox[1], lat1 = bbox[2], 
               lng2 = bbox[3], lat2 = bbox[4]) |>
  leaflet.extras::setMapWidgetStyle(style = list(background = "transparent"))

map

htmlwidgets::saveWidget(map, "map.html")


# Add map in HTML card ----------------------------------------------------

if (!require("bslib")) install.packages("bslib")
library(bslib)

card <- card(
  tags$h5("The Five Most Frequent Last Names by Commune, 2022"),
  tags$i("Hover to display the five most common surnames (actual number and percentage) by commune."),
  map,
  tags$p(
    "Source: FSO – STATPOP, inspired by:", 
    tags$a("https://www.bfs.admin.ch/asset/en/27205601", 
           href = "https://www.bfs.admin.ch/asset/en/27205601")),
  tags$p("Get the R code:", 
         tags$a("felixanalytix.com",
                href = "https://felixanalytix.com")),
  min_height = 800, 
  max_height = 800
)

htmltools::save_html(card, "card.html")
