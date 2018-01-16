library(tidyverse)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(venn)

train_data <- read_csv("train.csv")
head(train_data)
nrow(train_data)

counts <- c(normal=nrow(train_data), (map_int(train_data[3:8], sum)))
barplot(counts)

my_wordcloud <- function(input_data, column) {
  input_data %>% 
    filter(.[column] == 1) %>%
    select(comment_text) %>% 
    paste(sep= '', collapse = '') %>% 
    wordcloud(min.freq = 20, colors = brewer.pal(8, "Set2"))
}

my_wordcloud(train_data, "threat")
my_wordcloud(train_data, "toxic")
my_wordcloud(train_data, "identity_hate")
my_wordcloud(train_data, "obscene")
my_wordcloud(train_data, "insult")
my_wordcloud(train_data, "severe_toxic")

venn(train_data[3:8], ilab=TRUE, zcolor = "style")
