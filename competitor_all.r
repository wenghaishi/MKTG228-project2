library(ggplot2)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(tm)
library(syuzhet)
library(ggpubr)


data <- read.csv("rest56.csv")
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
sentiment <- as.data.frame(sentiment)


s_plot <- ggplot(sentiment, aes(sentiment)) +
  geom_bar(fill = "#29abe2") +
  theme_pubclean()

print(s_plot)

dev.copy(jpeg,filename="comp_all.jpg");
dev.off ();

#mean score of hygenie
s_m <- mean(sentiment$sentiment)
print(s_m)