# title: How to detect and analyse emotions from texts in R
# author: FelixAnalytix
# abstract: Youtube video: https://www.youtube.com/felixanalytix


# Install R packages if not installed -------------------------------------

if (!require(tidytext)) install.packages("tidytext")
if (!require(textdata)) install.packages("textdata")
if (!require(tidyverse)) install.packages("tidyverse")


# Attach R packages -------------------------------------------------------

library(tidytext) # text mining using tidy tools
library(textdata) # get sentiment lexicons
library(tidyverse)


# Get data ----------------------------------------------------------------

text_raw <- read_lines(
  file = "http://gutenberg.net.au/ebooks01/0100021.txt", #also at "data/orwell-text.txt"
  skip_empty_rows = TRUE,
  skip = 38, # remove metadata about book
  n_max = 8500) # remove appendix

head(text_raw)


# Create chapter variable -------------------------------------------------

text_df <- tibble(text = text_raw) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [1-9]", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^chapter [1-9]", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)

text_df


# Get sentiment lexicon ---------------------------------------------------

bing <- tidytext::get_sentiments("bing")
afinn <- tidytext::get_sentiments("afinn")
nrc <- tidytext::get_sentiments("nrc")


# Sentiment Lexicons Limitations ------------------------------------------

text_pos <- "This is my favorite book. I like it."
text_neg <- "This is not my favorite book. I don't like it."

df_limitations <- tibble(
  text = c(text_pos, text_neg),
  examples = c("text_pos", "text_neg")
)

df_limitations %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word") %>%
  inner_join(bing, by = "word")


# Tokenize and join lexicons ----------------------------------------------

text_df_bing <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(bing, by = "word")

text_df_nrc <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word")

text_df_afinn <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word")


# General statistics --------------------------------------------

# number of words by pos/neg binary sentiment
text_df_bing %>%
  count(sentiment, sort = TRUE)

# number of words by emotion
text_df_nrc %>%
  count(sentiment, sort = TRUE)

# overall sentiment score
text_df_afinn %>%
  summarise(overall_score = sum(value, na.rm = TRUE))


# Top words by emotion --------------------------------------------------

text_df_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Negative/Positive Words in Orwell's 1984",
       subtitle = "Top 10 words, BING lexicon",
       x = "Number of words",
       y = NULL)

text_df_nrc %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(!word %in% c("words", "feeling")) %>% #remove tokens
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Emotional Words in Orwell's 1984",
       subtitle = "Top 5 words, NRC lexicon",
       x = NULL, y = NULL,
       caption = "FelixAnalytix.com")


# Emotional score by chapter ----------------------------------------------

text_df_afinn_chapter_score <- text_df_afinn %>%
  group_by(chapter) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

text_df_afinn_chapter_score %>%
  mutate(positive = sentiment > 0,
         chapter = as.factor(chapter)) %>%
  ggplot(aes(chapter, sentiment)) +
  geom_col(aes(fill = positive), show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Sentiment analysis of Orwell's 1984",
       subtitle = "Score by chapter, afinn lexicon",
       caption = "FelixAnalytix.com")


# Index score by chapter -----------------------------------------------

text_df_index_score <- text_df_afinn %>%
  group_by(index) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

text_df_index_score %>%
  mutate(positive = sentiment > 0) %>%
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill = positive), show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Sentiment analysis of Orwell's 1984",
       subtitle = "Score for each 50 lines, afinn lexicon",
       caption = "FelixAnalytix.com")

