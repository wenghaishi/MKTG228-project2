library(caTools)
library(tm)
library(SnowballC)
library(topicmodels)
library(readxl)
library(tidyverse)
library(randomForest)
library(syuzhet)
library(epiDisplay)
library(ggplot2)
library(ggpubr)
library(writexl)


#pre-processing
data <- read.csv("rest1.csv")
text_only <- data[8]
corpus <- VCorpus(VectorSource(data$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
#corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

#creating bag of words model
dtm <- DocumentTermMatrix(corpus)
dtm <- as.matrix(dtm)
w <- rowSums(dtm)
w <- subset(w, w >= 25)
#barplot(w, las = 2, col ="blue")

sentiment_data <- iconv(data$text)
s <- get_nrc_sentiment(sentiment_data)
s$score <- s$positive - s$negative

s[1:10,]

#tab1(s$score, sort.group = "decreasing", cum.percent = TRUE)

s_freq <- data.frame(s$score)

#write_xlsx(s_freq,".\\freq.xlsx")

ggplot(s_freq, aes(s.score)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
