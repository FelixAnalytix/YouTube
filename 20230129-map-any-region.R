# title: Map any region of the world
# author: FelixAnalytix.com


## ----Install and load R packages-----------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("sf")) install.packages("sf")
if (!require("wbstats")) install.packages("wbstats")

library(tidyverse)
library(rnaturalearth) # World Map Data from Natural Earth
library(sf) # Geographic Simple Features in R
library(wbstats) # access World Bank API


## ----Download world data-------------------------
world <- ne_countries(scale="medium", returnclass="sf") %>%
  filter(admin != "Antarctica")


## ----Change World map projection-----------------
# Mollweide proj
target_crs <- "+proj=moll"

world_moll <- world %>%
  st_transform(crs = target_crs)


## ----Get data from the World Bank----------------
# Example: Unemployment (% of labor force)
# https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
ind <- "SL.UEM.TOTL.ZS"

indicator_info <- wb_cachelist$indicators %>%
  filter(indicator_id == ind)

indicator_info$indicator


## ----Download data from World Bank---------------
df <- wb_data(ind, start_date = 2020) %>%
  filter(date == 2020)

glimpse(df)


## ----Exploring data distribution-----------------
# sqrt transformation to improve color palette
df %>%
  ggplot() +
  geom_histogram(aes(SL.UEM.TOTL.ZS)) +
  theme_minimal() +
  scale_x_sqrt()


## ----Plot world map------------------------------
world_moll %>%
  left_join(df, by = c("iso_a3" = "iso3c")) %>%
  ggplot() +
  geom_sf(aes(fill = SL.UEM.TOTL.ZS)) +
  scale_fill_viridis_c(
    trans = "sqrt",
    labels = scales::percent_format(scale = 1),
    breaks = c(1:5)^2) +
  # fix labels if needed: https://stackoverflow.com/a/60733863
  scale_x_continuous(
    labels = function(x) paste0(x, '\u00B0', "W")
    ) +
  scale_y_continuous(
    labels = function(x) paste0(x, '\u00B0', "N")
    ) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  labs(
    title = paste(unique(df$date), indicator_info$indicator),
    fill = NULL,
    caption = paste("Source:", indicator_info$source_org) 
  )


## ----Create bounding box-------------------------
# Choose manually the region to plot with Open Street Map
# https://www.openstreetmap.org/export
window_coord <- st_sfc(
  st_point(c(-18, 32.5)), #left, bottom
  st_point(c(40.4, 72.3)), #right, top
  crs = 4326 #the EPSG identifier of WGS84 (used in GPS)
)

window_coord_sf <- window_coord %>%
  st_transform(crs = target_crs) %>%
  st_coordinates() # retrieve coordinates


## ----Plot regional map---------------------------
world_moll %>%
  left_join(df, by = c("iso_a3" = "iso3c")) %>% 
  ggplot() + 
  geom_sf(aes(fill = SL.UEM.TOTL.ZS)) +
  # window of the map
  coord_sf(
    xlim = window_coord_sf[, "X"],
    ylim = window_coord_sf[, "Y"],
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    trans = "sqrt", 
    labels = scales::percent_format(scale = 1),
    breaks = c(1:5)^2
  ) +
  # fix labels if needed: https://stackoverflow.com/a/60733863
  scale_x_continuous(
    labels = function(x) paste0(x, '\u00B0', "W")
    ) +
  scale_y_continuous(
    labels = function(x) paste0(x, '\u00B0', "N")
    ) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  labs(
    title = indicator_info$indicator,
    fill = NULL,
    caption = paste("Source:", indicator_info$source_org) 
  )


## ----Plot European countries only----------------
world_moll %>%
  left_join(df, by = c("iso_a3" = "iso3c")) %>% 
  filter(continent == "Europe") %>%
  ggplot() + 
  geom_sf(aes(fill = SL.UEM.TOTL.ZS)) +
  # window of the map
  coord_sf(
    xlim = window_coord_sf[, "X"],
    ylim = window_coord_sf[, "Y"],
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    #trans = "sqrt", 
    labels = scales::percent_format(scale = 1)#,
    #breaks = c(1:5)^2
  ) +
  # fix labels if needed: https://stackoverflow.com/a/60733863
  scale_x_continuous(
    labels = function(x) paste0(x, '\u00B0', "W")
    ) +
  scale_y_continuous(
    labels = function(x) paste0(x, '\u00B0', "N")
    ) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  labs(
    title = indicator_info$indicator,
    fill = NULL,
    caption = paste("Source:", indicator_info$source_org) 
  )

ggsave(filename = "europe-with-world-scale.png",
       plot = gg,
       width = 10, height = 8, dpi = 700)
