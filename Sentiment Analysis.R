library(tidytext)
library(stringr)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(tidyr)

sentiments
get_sentiments("bing")


tidy_data <- austen_books() |> group_by(book) |> 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) |>
  ungroup() |> unnest_tokens(word, text)

positive_senti <- get_sentiments("bing") |> filter(sentiment == "positive")

tidy_data |> filter(book == "Emma") |> semi_join(positive_senti) |> count(word, sort = TRUE)


bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data |> inner_join(bing) |> count(book = "Emma" , index = linenumber %/% 80, sentiment) |> 
  spread(sentiment, n, fill = 0) |> mutate(sentiment = positive - negative)


library(ggplot2)

ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


counting_words <- tidy_data |> inner_join(bing) |> count(word, sentiment, sort = TRUE)

head(counting_words)


counting_words |> filter(n > 150) |>
  mutate(n = ifelse(sentiment == "negative", -n, n)) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

