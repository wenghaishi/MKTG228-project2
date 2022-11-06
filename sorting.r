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
library(lubridate)


data <- read.csv("rest1.csv")
data$date = substr(data$date,1,nchar(data$date)-5)
vdate <- as.vector(data['date'])
vstars <- as.vector(data['stars'])
print(vdate)


p <- ggplot(data, aes(x=vdate, y=vstars)) +
  geom_line()
