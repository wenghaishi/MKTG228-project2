library(ggplot2)


s_freq <- read.csv("d.csv")

ggplot(s_freq, aes(s.score)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()