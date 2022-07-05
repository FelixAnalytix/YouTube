# title: How to filter data in R using dplyr
# author: FelixAnalytix.com
# https://youtu.be/PUJXa5zc-dY


# Install R packages if not installed -------------------------------------

if (!require(dplyr)) install.packages("dplyr")
if (!require(stringr)) install.packages("stringr")


# Attach R packages -------------------------------------------------------

library(dplyr) # for data transformation
library(stringr) # working with strings

starwars


# Filter with relational operators ----------------------------------------

# `eye_color` equal "blue"
filter(.data = starwars, eye_color == "blue") # `==` for "equal"

# equivalent with the pipe operator `%>%`
starwars %>% filter(eye_color == "blue")

# Remove one element using `!=`
starwars %>% 
  filter(eye_color != "blue") # `!=` for "not equal"

# Keep multiple elements using `%in%`
starwars %>% 
  filter(eye_color %in% c("blue", "yellow")) # `c()` combines values in a vector

# Remove multiple elements using `!` and `%in%`
starwars %>% 
  filter(!eye_color %in% c("blue", "yellow"))

# filter using logical operator
starwars %>% 
  filter(height > 200) # `>` for "greater than"

starwars %>% 
  filter(height < 100) # `<` for "less than"

# relational operators documentation
?base::Comparison

# Filter with logical operators -------------------------------------------

# filter using logical operator "and"
starwars %>% 
  filter(eye_color == "blue" & hair_color == "blond") # `&` for "and"

starwars %>% 
  filter(eye_color == "blue", hair_color == "blond") # `,` is equivalent to `&`

starwars %>% 
  filter(eye_color == "blue") %>% 
  filter(hair_color == "blond") # also working

# filter using logical operator "or"
starwars %>% 
  filter(eye_color == "blue" | hair_color == "blond") # `|` for "or"

# logical operators documentation
?base::Logic


# filter with other functions ---------------------------------------------

# filter using str_detect from stringr R package
starwars %>% 
  filter(str_detect(eye_color, "blue")) %>% # all containing string "blue"
  filter(eye_color != "blue")

# get all missing values
starwars %>% 
  filter(is.na(hair_color))

# remove missing values in a variable
starwars %>% 
  filter(!is.na(hair_color))

# greater than the height mean
starwars %>% 
  filter(height > mean(height, na.rm = TRUE))

# greater than the height mean by species
starwars %>% 
  group_by(species) %>% 
  filter(height > mean(height, na.rm = TRUE)) %>%
  ungroup()

