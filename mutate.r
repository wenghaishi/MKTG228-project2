library(tidyverse)

test <- tibble(x = c(9,5,1), y = c(3,2,7))
test <- mutate(test, z = 3 *x)
test <- 