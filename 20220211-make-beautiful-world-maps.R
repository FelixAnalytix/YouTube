# title: How to make world maps using R and RStudio
# author: FelixAnalytix.com
# https://youtu.be/FoqiFR5ZCic


# Install R packages if not installed -------------------------------------

if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("sf")) install.packages("sf", dependencies = TRUE)
if (!require("rnaturalearth")) install.packages("rnaturalearth", dependencies = TRUE)
if (!require("countrycode")) install.packages("countrycode", dependencies = TRUE)
if (!require("ggrepel")) install.packages("ggrepel", dependencies = TRUE)

# for MacOS and Linux, to install "sf" see https://r-spatial.github.io/sf/#installing


# Load R packages ---------------------------------------------------------

library("tidyverse") # load dplyr, ggplot2, stringr, etc.
library("sf") # working with geographic simple features in R
library("rnaturalearth") # World map data from Natural Earth
library("countrycode") # get ISO code from country names
library("ggrepel") # "ggplot2" extension for overlapping text labels


# Get world data ----------------------------------------------------------

world <- ne_countries(scale = "small", returnclass = "sf")


# Plot an empty world map -------------------------------------------------

world %>%
  ggplot() +
  geom_sf()


# Change map projection ---------------------------------------------------

# projection list: https://proj.org/operations/projections/
# examples: "+proj=robin", "+proj=moll", "+proj=aeqd", "+proj=goode"
world %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf() +
  theme_minimal()

# fixing some proj bugs by removing the graticule
world %>%
  st_transform(crs = "+proj=wintri") %>%
  ggplot() +
  geom_sf() +
  coord_sf(datum = NA) + # no graticule
  theme_minimal()


# Prepare data ------------------------------------------------------------

# read data
data_raw <- read_csv("https://raw.githubusercontent.com/FelixAnalytix/Youtube/master/data/jamesbond.csv")

# Tidy dataset in long format
data <- data_raw %>%
  select(Movie, Bond, Depicted_Film_Loc) %>%
  separate_rows(Depicted_Film_Loc, sep = ", ") %>%
  mutate(Depicted_Film_Loc = recode(Depicted_Film_Loc, 
                                    "England" = "United Kingdom", 
                                    "Scotland" = "United Kingdom")) %>%
  mutate(Visited = TRUE)

# add iso3 country code
data_with_iso <- data %>%
  mutate(Iso3 = countrycode::countrycode(
    sourcevar = Depicted_Film_Loc, 
    origin = "country.name", 
    destination = "iso3c")
  )

data_with_iso


# Join datasets -----------------------------------------------------------

countries_visited <- world %>%
  select(geometry, name, iso_a3) %>%
  left_join(data_with_iso, by = c("iso_a3" = "Iso3")) %>%
  filter(Visited == TRUE)

countries_visited


# Countries visited -------------------------------------------------------

world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "darkgrey") +
  geom_sf(data = countries_visited, aes(fill = Visited)) +
  scale_fill_manual(values = "royalblue") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "All countries visited in the movie saga",
       x = NULL, y = NULL,
       caption = "FelixAnalytix.com")


# Countries visited in Casino Royale --------------------------------------

countries_visited_casino_royale <- countries_visited %>%
  filter(Movie == "Casino Royale")

world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "darkgrey") +
  geom_sf(data = countries_visited_casino_royale, 
          aes(fill = Visited)) +
  ggrepel::geom_label_repel(
    data = countries_visited_casino_royale,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0
  ) +
  scale_fill_manual(values = "royalblue") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "Countries visited in Casino Royale",
       x = NULL, y = NULL,
       caption = "FelixAnalytix.com")


# Countries visited by Daniel Craig's James Bond ------------------

countries_visited_craig <- countries_visited %>%
  filter(Bond == "Daniel Craig")

world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "darkgrey") +
  geom_sf(data = countries_visited_craig, 
          aes(fill = Visited)) +
  facet_wrap(~ Movie) +
  scale_fill_manual(values = "royalblue") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "Countries visited, Daniel Craig's movies",
       x = NULL, y = NULL,
       caption = "FelixAnalytix.com")


# Countries visited by James Bond actor -----------------------------------

world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "darkgrey") +
  geom_sf(data = countries_visited, 
          aes(fill = Visited)) +
  facet_wrap(~ Bond) +
  scale_fill_manual(values = "royalblue") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "Countries visited in movies, by actor",
       x = NULL, y = NULL,
       caption = "FelixAnalytix.com")


# ADDITIONAL CODE --------------------------------------------------------

# QUESTION FROM VIEWER "A P":
# "what if I say should plot supra-national entities, EU, ASEAN or Mercosur etc....?"

# MY ANSWER: you could create a dataset with a "supra-national" variable.
# Here, as an example, a world map with the countries by continent
countries_by_region <- world %>%
  select(geometry, name, iso_a3) %>%
  left_join(countrycode::codelist %>%
              distinct(region, iso3c), by = c("iso_a3" = "iso3c")) %>%
  filter(region != "Antartica")

world %>%
  filter(admin != "Antarctica") %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "darkgrey") +
  geom_sf(data = countries_by_region, aes(fill = region)) +
  #scale_fill_manual(values = "royalblue") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "Countries by region",
       #subtitle = "All countries visited in the movie saga",
       x = NULL, y = NULL,
       caption = "FelixAnalytix.com")
