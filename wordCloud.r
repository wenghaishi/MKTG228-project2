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
library(ggpubr)


#pre-processing
data <- read.csv("rest1.csv")

Review_words <- unnest_tokens(data, word, text, token = "words", to_lower = TRUE) 

Word_count <- Review_words %>% count(word, sort = TRUE)

Review_words_no_sw <- anti_join(Review_words, stop_words)

Digits <- str_detect(Review_words_no_sw$word, "\\d")

Review_words_no_sw <- Review_words_no_sw[!Digits, ]

Word_count_no_sw <- Review_words_no_sw %>% count(word, sort = TRUE)

Word_count_no_sw <- mutate(Word_count_no_sw, word = reorder(word, n))

p <- ggplot(Word_count_no_sw[1:10, ]) + geom_col(aes(n, word), fill = "lavenderblush2") + theme_minimal()

o <- wordcloud2(Word_count_no_sw, size = 0.6, color = "skyblue")
i <- wordcloud2(Word_count_no_sw, size = 1, minSize = 0, gridSize =  0,
    fontFamily = 'Segoe UI', fontWeight = 'bold',
    color = 'random-dark', backgroundColor = "white",
    minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
    rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
    widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
l <- wordcloud2(Word_count_no_sw,
           size = 1, color = "#29abe2",
           minRotation = -pi/60,
           maxRotation = -pi/60,
           rotateRatio = 1)
print(l)


Review_words_no_sw_sent <- left_join(Review_words_no_sw, afinn)
Review_words_no_sw_sent <- left_join(Review_words_no_sw_sent, bing)
# Rename columns
Review_words_no_sw_sent <- rename(Review_words_no_sw_sent, afinn = value, bing = sentiment)



# Replace all NAs with 0s
Review_words_no_sw_sent[is.na(Review_words_no_sw_sent[, 4]), 4] <- 0
Review_words_no_sw_sent[is.na(Review_words_no_sw_sent[, 5]), 5] <- '0'

# Wrt bing, set positive = 1 and negative = -1
Review_words_no_sw_sent[Review_words_no_sw_sent[, 5] == 'negative', 5] <- '-1'
Review_words_no_sw_sent[Review_words_no_sw_sent[, 5] == 'positive', 5] <- '1'
Review_words_no_sw_sent$bing <- as.numeric(Review_words_no_sw_sent$bing)

# Determine sentiment score for each review
Review_sentiments <- Review_words_no_sw_sent %>% 
		group_by(review_id) %>% 
		summarize(afinn = sum(afinn), bing = sum(bing))

# Add sentiment scores to Boost_reviews
data <- left_join(data, Review_sentiments)

#write.csv(data,".\\File dName.csv", row.names = FALSE)


