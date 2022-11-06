library(caTools)
library(tm)
library(SnowballC)
library(readxl)
library(tidyverse)
library(randomForest)
library(tidyverse)
library(readr)
library(tidytext)
library(wordcloud2)
library(textdata)
library(topicmodels)
library(RColorBrewer)




#pre-processing
data <- read.csv("rest1.csv")

Review_words <- unnest_tokens(data, word, text, token = "words", to_lower = TRUE) 
print(Review_words)