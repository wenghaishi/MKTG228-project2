library(ggplot2)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(tm)
library(syuzhet)
library(ggpubr)


data <- read.csv("rest1.csv")
h <- tibble(text = str_to_lower(data$text))

h <- filter(h, grepl(pattern = "\\b(clean|cleanliness|unclean|cleaning|dirty|rat|rats|ant|toilet|wash|washing|ants|spotless|immaculate|dirt|rats|
covid|covid-19|soiled|diaper|disease|germ|germs|flies|soap|spotless|vermine|hygiene|hygienic|pest|pests|clutter|cluttered|puddle|wipe|sweep|broom|rubbish|trash|pests|splatter|tidy|
untidy|spotless|dusty|dust|immaculate|contaminated|dust|filth|filthy|stain|stained|grimy|grime|puddle|wipe|wiping|septic|antiseptic|sanitation|washed|well-kept|
sanitary|virus|viruses|coronavirus|aseptic|mopped|neatly)\\b", text))
h$text

corpus <- VCorpus(VectorSource(h$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
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

dev.copy(jpeg,filename="focal_hygenie.jpg");
dev.off ();


#mean score of hygenie
s_m <- mean(sentiment$sentiment)
print(s_m)