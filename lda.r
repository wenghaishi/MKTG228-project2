library(caTools)
library(tm)
library(SnowballC)
library(topicmodels)
library(readxl)
library(tidyverse)
library(randomForest)
library(tidyverse)
library(readr)
library(tidytext)
library(wordcloud2)
library(textdata)
library(quanteda)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)





#pre-processing
data <- read.csv("rest1.csv")
words <- read_excel("h1.xls")
text_only <- data[8]
corpus <- VCorpus(VectorSource(data$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

as.character(corpus[1])

dtm <- DocumentTermMatrix(corpus)
model_lda <- LDA(dtm, k = 3, control = list(seed = 1234))
model_lda
beta_topics <- tidy(model_lda, matrix = "beta")
beta_top_terms <- beta_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    arrange(topic, -beta)

beta_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(fill = "#29abe2" , show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

Word_topic <- tidy(model_lda, matrix = "beta")
Top_word_topic <- Word_topic %>% group_by(topic) %>% slice_max(beta, n = 5) 


print(Word_topic)
print(Top_word_topic)
print(beta_top_terms)
