library(ggplot2)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(tm)
library(syuzhet)
library(ggpubr)
library(RColorBrewer)
library(ggpubr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(tm)
library(syuzhet)
library(ggpubr)



data <- read.csv("rest1.csv")
h <- tibble(text = str_to_lower(data$text))

corpus <- VCorpus(VectorSource(h$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
#corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

#transform to dtm
dtm <- DocumentTermMatrix(corpus)
dtm <- as.matrix(dtm)
w <- rowSums(dtm)
w <- subset(w, w >= 25)

#adding sentiment
sentiment_data <- iconv(h$text)
sentiment <- get_sentiment(sentiment_data, method = "bing")
sentiment <- tibble(sentiment)

sentiment1 <- sentiment %>%
  mutate(id = 1:2230)

sentiment1 <- tibble(sentiment1)



#merging data into single tibble
h1 <- h %>%
  mutate(id = 1:2230)

h2 <- merge(sentiment1,h1, all.x=TRUE)
h2 <- tibble(h2)

h2_negative <- filter(h2, sentiment > 5)

Review_words <- unnest_tokens(h2_negative, word, text, token = "words", to_lower = TRUE) 

Word_count <- Review_words %>% count(word, sort = TRUE)

Review_words_no_sw <- anti_join(Review_words, stop_words)

Digits <- str_detect(Review_words_no_sw$word, "\\d")

Review_words_no_sw <- Review_words_no_sw[!Digits, ]

Word_count_no_sw <- Review_words_no_sw %>% count(word, sort = TRUE)

Word_count_no_s <- mutate(Word_count_no_sw, word = reorder(word, n))

p <- ggplot(Word_count_no_sw[1:10, ]) + geom_col(aes(n, word), fill = "lavenderblush2") + theme_minimal()

l <- wordcloud2(Word_count_no_s,
           size = 1, color = "#29abe2",
           minRotation = -pi/60,
           maxRotation = -pi/60,
           rotateRatio = 1)
print(l)

