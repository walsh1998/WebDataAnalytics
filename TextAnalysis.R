##########################Text Analysis#######################
  #load data
library(readxl)
text <- read_excel("~/Documents/MastersProgram/MGMT 590-WDA/FinalProjectText.xlsx")

  #sentiment scores by dictionary
library(dplyr)
library(tidytext)
library(tidyr)
library(tm)
library(stringr)

afinn <- text %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, `Future Plans to Add New Devices`) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarise(afinnsentiment = sum(value)) %>%
  mutate(method = "AFINN")

text_bing <- text %>% 
    mutate(id = row_number()) %>%
    unnest_tokens(word, `Future Plans to Add New Devices`) %>%
    inner_join(get_sentiments("bing")) %>%
    count(id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    mutate(method = "Bing et al.")

text_nrc <- text %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, `Future Plans to Add New Devices`) %>%
  inner_join(get_sentiments("nrc") %>% 
        filter(sentiment %in% c("positive", "negative"))) %>%
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
  
  #join scores from all of the different tables
text_scores <- text %>%
  left_join(afinn)

text_scores <- text_scores %>%
  left_join(text_bing, by = "id")

text_scores <- text_scores %>%
  left_join(text_nrc, by = "id")

  #delete extra columns and rename for easy analysis
text_scores <- text_scores %>%
  dplyr::select(-c(id, method.x, negative.x, positive.x, method.y,
                   negative.y, positive.y)) %>%
  rename(AFINN = sentiment.x, Bing = sentiment.y,NRC = sentiment)

text_scores$final_score <- rowMeans(text_scores[,c("AFINN", "Bing", "NRC")], na.rm=TRUE)

  
