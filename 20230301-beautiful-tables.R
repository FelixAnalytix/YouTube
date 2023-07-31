# title: Make Beautiful Tables using R
# author: FelixAnalytix.com


## ----Install and load R packages------------------------------------
if (!require(spotifyr)) install.packages("spotifyr")
if (!require(gt)) install.packages("gt")
if (!require(gtExtras)) install.packages("gtExtras")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(scales)) install.packages("scales")

library(spotifyr) # R Wrapper for the 'Spotify' Web API
library(gt) # Create Beautiful HTML tables
library(gtExtras) # Extending 'gt' for more customization
library(tidyverse) # set of R packages for data wrangling and viz
library(scales) # Scale Functions for Visualization


## ----Connect to Spotify API-----------------------------------------
# Sign up and log in: https://developer.spotify.com/dashboard/
# Click on "Create App", and fill the necessary fields
# NOTE: "Redirect URI" doesn't matter, you can fill it with "http://localhost:1410/"
# In the "Settings" of the app, copy paste below "Client ID" and "Client secret"
Sys.setenv(SPOTIFY_CLIENT_ID = "your_spotify_client_id")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "your_spotify_client_secret")

access_token <- get_spotify_access_token()


## ----Download Spotify Global Top Tracks-----------------------------
# get tracks from "Top 50 Global" playlist
top_global <- get_playlist(playlist_id = "37i9dQZEVXbMDoHDwVN2tF") %>%
  spotifyr::tidy() %>%
  head(10) %>%
  rowid_to_column(var = "rank") %>%
  mutate(rank = paste0(rank, "."))

# download audio metrics
top_global_features <- get_track_audio_features(ids = top_global$id) %>%
  select(danceability:id, -type)

artists <- bind_rows(top_global$artists)

# download artist images
get_artist_image <- function(id) {
  df <- spotifyr::get_artist(id)
  tibble(artist_name = df$name, img = df$images[1,2])
}

artists_image <- map_dfr(artists$id, get_artist_image)

# join all datasets
top_songs <- top_global %>%
  left_join(top_global_features, by = "id") %>%
  mutate(artist_first = map_chr(artist_names, first)) %>%
  left_join(artists_image %>% distinct_all(), #fix if duplicated images
            by = c("artist_first" = "artist_name"))

#write_csv(top_songs, "data/top_christmas_songs.csv")
top_songs


## ----Create a customized table--------------------------------------
top_songs %>%
  select(rank, track_name, artist_first, danceability, valence, energy) %>%
  gt() %>%
  cols_label(track_name = "song", 
             artist_first = "artist",
             rank = "") %>%
  tab_style(
    style = list(cell_text(style = "italic")),
    locations = cells_body(columns = track_name)
    ) %>%
  tab_options(heading.title.font.size = px(24)) %>%
  tab_header(
    title = "The Most Popular Songs",
    subtitle = "Today's top 10 global tracks on Spotify."
    ) %>%
  tab_source_note(md("Author: *FelixAnalytix.com*")) %>%
  gt_theme_pff() # change table theme

## ----Table with images----------------------------------------------
top_songs %>%
  select(rank, track_name, img, artist_first, danceability, valence, energy) %>%
  gt() %>%
  gt_img_circle(img, height = 25) %>%
  cols_label(track_name = "song", 
             artist_first = "artist",
             rank = "", 
             img = "") %>%
  cols_width(rank~px(10),
             img~px(35)) %>%
  tab_style(
    style = list(cell_text(style = "italic")),
    locations = cells_body(columns = track_name)
    ) %>%
  tab_options(heading.title.font.size = px(24)) %>%
  tab_header(
    title = "The Most Popular Songs",
    subtitle = "Today's top 10 global tracks on Spotify."
    ) %>%
  tab_source_note(md("Author: *FelixAnalytix.com*")) %>%
  gt_theme_pff()


## ----Color gradient numeric cells-----------------------------------
colors <- hue_pal(direction = -1)(3)

top_songs %>%
  select(rank, track_name, img, artist_first, danceability, valence, energy) %>%
  gt() %>%
  gt_img_circle(img, height = 25) %>%
  gt_color_rows(danceability, palette = c("white", colors[1])) %>%
  gt_color_rows(valence, palette = c("white", colors[2])) %>%
  gt_color_rows(energy, palette = c("white", colors[3])) %>%
  cols_label(track_name = "song", 
             artist_first = "artist",
             rank = "") %>%
  tab_style(
    style = list(cell_text(style = "italic")),
    locations = cells_body(columns = track_name)
    ) %>%
  tab_options(heading.title.font.size = px(24)) %>%
  tab_header(
    title = "The Most Popular Songs",
    subtitle = "Today's top 10 global tracks on Spotify."
    ) %>%
  tab_source_note(md("Author: FelixAnalytix.com | Data source: Spotify's playlist *Top 50 - Global*.")) %>%
  gt_theme_pff()


## ----Percent bar in tables------------------------------------------
top_songs %>%
  select(rank, track_name, img, artist_first, danceability, valence, energy) %>%
  gt() %>%
  gt_img_circle(img, height = 25) %>%
  gt_plt_bar_pct(danceability, fill = colors[1]) %>%
  gt_plt_bar_pct(valence, fill = colors[2]) %>%
  gt_plt_bar_pct(energy, fill = colors[3]) %>%
  cols_label(track_name = "song", 
             artist_first = "artist",
             rank = "", 
             img = "") %>%
  cols_width(rank~px(10),
             img~px(35)) %>%
  tab_style(
    style = list(cell_text(style = "italic")),
    locations = cells_body(columns = track_name)
    ) %>%
  tab_options(heading.title.font.size = px(24)) %>%
  tab_header(
    title = "The Most Popular Songs",
    subtitle = "Today's top 10 global tracks on Spotify."
    ) %>%
  tab_source_note(md("Author: *FelixAnalytix.com*")) %>%
  gt_theme_pff()


## ----Merging two columns in one-------------------------------------
top_songs %>%
  select(rank, track_name, img, artist_first, danceability, valence, energy) %>%
  gt() %>%
  gt_merge_stack(col1 = track_name, col2 = artist_first) %>%
  gt_img_circle(img, height = 25) %>%
  gt_plt_bar_pct(danceability, fill = colors[1]) %>%
  gt_plt_bar_pct(valence, fill = colors[2]) %>%
  gt_plt_bar_pct(energy, fill = colors[3]) %>%
  cols_label(track_name = "song",
             artist_first = "artist",
             rank = "",
             img = "") %>%
  cols_width(rank~px(10),
             img~px(35)) %>%
  tab_style(
    style = list(cell_text(style = "italic")),
    locations = cells_body(columns = track_name)
    ) %>%
  tab_options(heading.title.font.size = px(24)) %>%
  tab_header(
    title = "The Most Popular Songs",
    subtitle = "Today's top 10 global tracks on Spotify."
    ) %>%
  tab_source_note(md("Author: *FelixAnalytix.com*")) %>%
  gt_theme_guardian()
