# title: "Interactive Maps of Country Regions in Europe using R programming"
# author: FelixAnalytix.com


# Install and Load R packages ---------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("BFS")) install.packages("BFS")
if (!require("giscoR")) install.packages("giscoR")
if (!require("mapview")) install.packages("mapview")
if (!require("sf")) install.packages("sf")
if (!require("leafsync")) install.packages("leafsync")
if (!require("leaflet.extras2")) install.packages("leaflet.extras2")

library(tidyverse) # data wrangling and viz ecosystem
library(BFS) # Search and Download Data from the Swiss Statistical Office
library(giscoR) # Access Eurostat Mapping API
library(mapview) # Create Interactive Maps easily
library(sf) # mapping with Simple Feature
library(leaflet) # interactive maps
library(leafsync) # plugin for leaflet


# Get Swiss dataset -------------------------------------------------------

# Swiss dataset: https://www.bfs.admin.ch/asset/de/px-x-1502000000_101
swiss_students <- BFS::bfs_get_data(number_bfs = "px-x-1502000000_101", language = "de", clean_names = TRUE)

swiss_students_gender <- swiss_students %>%
  pivot_wider(names_from = geschlecht, values_from = lernende) %>%
  mutate(share_woman = round(Frau/`Geschlecht - Total`*100, 1))

swiss_students_gender


# Get mapping data --------------------------------------------------------

switzerland_sf <- gisco_get_nuts(
  country = "Switzerland", 
  nuts_level = 3, 
  resolution = "01")

switzerland_sf


# Join dataset with mapping data ------------------------------------------

# Preferably using NUTS-3 code if possible
swiss_student_map <- swiss_students_gender %>%
  filter(schulkanton != "Schweiz") %>%
  mutate(schulkanton = str_remove(schulkanton, ".*/"),
         schulkanton = str_trim(schulkanton),
         schulkanton = recode(schulkanton, "Berne" = "Bern", "Grischun" = "Graubünden", "Wallis" = "Valais")) %>%
  left_join(switzerland_sf, by = c("schulkanton" = "NUTS_NAME"))

swiss_student_map


# Interactive map of Switzerland ------------------------------------------

swiss_student_map_bildungsstufe <- swiss_student_map %>%
  filter(jahr == "2001/02",
         schulkanton != "Schweiz",
         staatsangehorigkeit_kategorie == "Schweiz") %>%
  select(schulkanton, jahr, bildungsstufe, share_woman, geometry) %>%
  pivot_wider(names_from = "bildungsstufe", values_from = "share_woman") %>%
  sf::st_as_sf()

swiss_student_map_bildungsstufe %>%
  mapview(zcol = "Bildungsstufe - Total", layer.name = "Total education level, % Woman")


# Synchronize multiple maps -----------------------------------------------

leafsync::sync(
  swiss_student_map_bildungsstufe %>%
    mapview(zcol = "Tertiärstufe", layer.name = "Tertiary level, % Woman"),
  swiss_student_map_bildungsstufe %>%
    mapview(zcol = "Sekundarstufe II", layer.name = "Secondary level II, % Woman"),
  swiss_student_map_bildungsstufe %>%
    mapview(zcol = "Obligatorische Schule", layer.name = "Mandatory school, % Woman"),
  swiss_student_map_bildungsstufe %>%
    mapview(zcol = "Nicht auf Stufen aufteilbare Ausbildungen", layer.name = "Training that cannot be divided into levels, % Woman")
)


# Comparing maps with a slider --------------------------------------------

swiss_student_map_bildungsstufe_1999 <- swiss_student_map %>%
  filter(jahr == "1999/00",
         schulkanton != "Schweiz",
         staatsangehorigkeit_kategorie == "Schweiz") %>%
  select(schulkanton, jahr, bildungsstufe, share_woman, geometry) %>%
  pivot_wider(names_from = "bildungsstufe", values_from = "share_woman") %>%
  sf::st_as_sf()

map1 <- mapview(swiss_student_map_bildungsstufe, zcol = "Bildungsstufe - Total")

map2 <- mapview(swiss_student_map_bildungsstufe_1999, zcol = "Bildungsstufe - Total")

map1 | map2
