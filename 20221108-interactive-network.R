# title: Network analysis of the Marvel Cinematic Universe
# author: Felix Analytix
# abstract: https://www.youtube.com/felixanalytix

# Install R packages if not installed -------------------------------------

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(rvest)) install.packages("rvest")
if (!require(tidygraph)) install.packages("tidygraph")
if (!require(visNetwork)) install.packages("visNetwork")


# Attach R packages -------------------------------------------------------

library(tidyverse)
library(rvest) # web scraping
library(tidygraph) # network analysis
library(visNetwork) # interactive network visualization


# Get Marvel Characters Data from Wikipedia -------------------------------

# Recurring characters of Phases 1, 2, 3 of the Infinity Saga
url1 <- "https://en.wikipedia.org/w/index.php?title=Marvel_Cinematic_Universe:_Phase_One&oldid=1096778874"
url2 <- "https://en.wikipedia.org/w/index.php?title=Marvel_Cinematic_Universe:_Phase_Two&oldid=1097878759"
url3 <- "https://en.wikipedia.org/w/index.php?title=Marvel_Cinematic_Universe:_Phase_Three&oldid=1096776335"

urls <- c(url1, url2, url3)

# Get recurring characters table
get_table <- function(wiki_url) {
  df <- wiki_url %>%
    read_html() %>%
    html_nodes(".wikitable") %>%
    html_table(na.strings = "") %>%
    .[[3]] %>%
    janitor::row_to_names(row_number = 1)
  
  df_tidy <- df %>%
    pivot_longer(cols = names(.)[-1],
                 names_to = "Movie",
                 values_to = "Actor") %>%
    # clean Actor and Character names
    mutate(Actor = str_remove_all(Actor, "\\[|\\]"),
           Actor = str_remove_all(Actor, "[:digit:]"),
           Character = str_remove_all(Character, '\\\"')) %>%
    # add space if missing
    mutate(Movie = str_replace_all(Movie, "([a-z]|:)([A-Z])", "\\1 \\2"),
           Character = str_replace_all(Character, "([a-z])([A-Z])", "\\1 \\2"))
}

# loop on each wikipedia URL
df <- purrr::map_dfr(urls, get_table)

# Actor name as NA if ends with C
# "C" indicates an uncredited cameo role.
# WHEN ACTOR HAS A MISSING VALUE, IT MEANS CHARACTER NOT IN THE MOVIE
df_cleaned <- df %>%
  mutate(Actor = if_else(condition = str_detect(Actor, "C$"), 
                         true = NA_character_, 
                         false = Actor)) %>%
  mutate(Character = str_remove(Character, "OS$"),
         Character = str_trim(Character),
         Movie = str_trim(Movie),
         Movie = recode(Movie, "Thor" = "Thor 1", "Iron Man" = "Iron Man 1")) %>%
  filter(!is.na(Actor)) %>% # REMOVE CHARACTERS NOT APPEARING IN THE MOVIE
  select(-Actor)
  
df_cleaned

# Exploratory Data Analysis -----------------------------------------------

# Most recurring characters
df_cleaned %>%
  count(Character, sort = TRUE)

# Movies with the most characters
df_cleaned %>%
  count(Movie, sort = TRUE)

# create a "tbl_graph" object
network <- df_cleaned %>%
  as_tbl_graph()

network

# "tbl_graph" and "igraph" object
class(network)

# centrality measures, for example degree
network %>%
  activate(nodes) %>% # change context
  mutate(degree = centrality_degree()) %>% 
  as_tibble() %>%
  arrange(desc(degree))


# Interactive Network -----------------------------------------------------

vis_network <- network %>%
  mutate(group = if_else(condition = name %in% unique(df_cleaned$Character), 
                         true = "Character", 
                         false = "Movie")) %>%
  toVisNetworkData()

# Full config see docs: https://datastorm-open.github.io/visNetwork/

# interactive network
visNetwork(nodes = vis_network$nodes, edges = vis_network$edges, 
           width = "100%", height = "600px",
           main = "The Marvel Cinematic Universe Network") %>%
  visLayout(randomSeed = 1000) %>%
  addFontAwesome() %>%
  visGroups(groupname = "Movie", shape = "icon",
            icon = list(code = "f008", color = "darkblue")) %>%
  visGroups(groupname = "Character", shape = "icon",
            icon = list(code = "f007", color = "red")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), 
             nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)
