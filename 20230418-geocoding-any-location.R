# title: Geocoding using R: get latitude and longitude of any location
# author: FelixAnalytix.com
# abstract: Youtube video: https://www.youtube.com/felixanalytix


# Install R packages if not installed -------------------------------------

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(rvest)) install.packages("rvest")
if (!require(usethis)) install.packages("usethis")
if (!require(tidygeocoder)) install.packages("tidygeocoder")
if (!require(leaflet)) install.packages("leaflet")


# Attach R packages -------------------------------------------------------

library(tidyverse)
library(rvest)
library(usethis)
library(tidygeocoder)
library(leaflet)


# Scraping James Bond Locations -------------------------------------------

url <- "https://en.wikipedia.org/w/index.php?title=List_of_James_Bond_film_locations&oldid=1139013770"

table <- url %>%
  read_html() %>%
  html_element(".wikitable") %>%
  html_table()

# Creating a 'movie' variable
table_with_movie <- table %>%
  janitor::clean_names() %>%
  select(-x) %>% # remove empty column
  mutate(movie = if_else(
    condition = str_detect(country_or_region_depicted, "\\([0-9]{4}\\)"),
    true = country_or_region_depicted,
    false = NA_character_)) %>%
  fill(movie, .direction = "down") %>%
  filter(!str_detect(country_or_region_depicted, "\\([0-9]{4}\\)"))

# Extract longitude and latitude from 'coordinates' variable
table_tidy <- table_with_movie %>%
  mutate(coordinates = str_remove(coordinates, ".*/")) %>%
  separate(col = coordinates, into = c("latitude", "longitude"), sep = ";") %>%
  mutate(longitude = str_trim(longitude),
         longitude = as.numeric(longitude),
         latitude = str_trim(latitude),
         latitude = as.numeric(latitude))

glimpse(table_tidy)


# geocoding API keys ------------------------------------------------------

# https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html#overview
tidygeocoder::api_info_reference

#usethis::edit_r_environ()
#usethis::edit_git_ignore() 
# ADD .Renviron in `.gitignore` to avoid leaking tokens using Git


# Get longitude and latitude ----------------------------------------------

table_tidy_geocodes <- table_tidy %>%
  mutate(location_in_story_country = 
           paste(location_in_story, country_or_region_depicted, sep = ", ")) %>%
  geocode(address = location_in_story_country,
          method = "arcgis")

# Save data to avoid query servers again
#write_csv(table_tidy_geocodes, "table_geocodes.csv")
#table_tidy_geocodes <- read_csv("table_geocodes.csv")

table_tidy_geocodes


# Interactive maps using leaflet ------------------------------------------

movie_names <- unique(table_tidy_geocodes$movie)

table_tidy_geocodes %>%
  mutate(popup = paste0("<b>", movie, "</b>", "<br>",
                        country_or_region_depicted, "<br>",
                        location_in_story, "<br>")) %>%
  leaflet() %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~long, ~lat, 
             popup  = ~popup, group = ~movie) %>%
  addLayersControl(overlayGroups = movie_names,
                   options = layersControlOptions(collapsed = FALSE))


# Reverse geocoding -------------------------------------------------------

table_tidy_geocodes_reverse <- table_tidy %>%
  head(20) %>%
  reverse_geocode(lat = latitude, long = longitude, method = "arcgis")


# Comparing geocoding services --------------------------------------------

list_bond_movies <- c("Casino Royale (2006)", "Quantum of Solace (2008)", "Skyfall (2012)", "Spectre (2015)", "No Time to Die (2021)")

table_tidy_geocode_compare <- table_tidy %>%
  select(-latitude, -longitude) %>%
  filter(movie %in% list_bond_movies) %>%
  mutate(location_in_story_country = 
           paste(location_in_story, country_or_region_depicted, sep = ", ")) %>%
  geocode_combine(
    queries = list(
      list(method = "osm"),
      list(method = "arcgis"),
      list(method = "mapbox"),
      list(method = "bing"),
      list(method = "google")
    ),
    global_params = list(address = "location_in_story_country"),
    cascade = FALSE
  )

table_tidy_geocode_compare

# how many queries per service
table_tidy_geocode_compare %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  count(query)

# Interactive map using leaflet

query <- unique(table_tidy_geocode_compare$query)

colors <- RColorBrewer::brewer.pal(length(query), name = "Set1")

table_tidy_geocode_compare %>%
  mutate(popup = paste0("<b>", movie, "</b>", "<br>",
                        country_or_region_depicted, "<br>",
                        location_in_story, "<br>", "<b>", query, "</b>")) %>%
  mutate(color = case_when(query == "arcgis" ~ colors[1],
                           query == "osm" ~ colors[2],
                           query == "mapbox" ~ colors[3],
                           query == "bing" ~ colors[4],
                           query == "google" ~ colors[5])) %>%
  leaflet() %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  leaflet::addCircleMarkers(~long, ~lat, 
             popup  = ~popup, group = ~query,  color = ~color) %>%
  addLayersControl(overlayGroups = query,
                   options = layersControlOptions(collapsed = FALSE))


# Distance between two coordinates -----------------------------------------

table_tidy_geocode_compare_sf <- table_tidy_geocode_compare %>%
  mutate(long = replace_na(long, 0), lat = replace_na(lat, 0)) %>% # NA as (0, 0) bet st_as_sf cannot handle NAs
  sf::st_as_sf(coords = c("long", "lat"), remove = F, crs = 4326)

distance <- table_tidy_geocode_compare_sf %>%
  filter(query == "arcgis") %>%
  sf::st_distance(
    table_tidy_geocode_compare_sf %>%
      filter(query == "osm"),
    by_element = TRUE
  )

table_tidy_geocode_compare %>%
  pivot_wider(names_from = query, values_from = c(long, lat)) %>%
  mutate(distance_arcgis_osm = distance) %>%
  #na.omit() %>% # remove locations where a geocoder gave a missing value
  arrange(distance_arcgis_osm)

