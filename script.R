library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(scales)
library(ggplot2)
library('widyr')
library(stringr)
library(igraph)
library(ggraph)
#setwd("C:/Users/wyver/Desktop/Master/An 2/Sem 1/Text/project")

# Read the text files
ep4 <- read.table("./texts/SW_EpisodeIV.txt")
ep5 <- read.table("./texts/SW_EpisodeV.txt")
ep6 <- read.table("./texts/SW_EpisodeVI.txt")

#Tokenize
tidy_ep4 <-ep4 %>%
  group_by(character) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words)

#NRC Lexicon
nrc_all<-get_sentiments("nrc")
nrc_trust <-get_sentiments("nrc") %>%
  filter(sentiment =="trust")
nrc_fear<-get_sentiments("nrc") %>%
  filter(sentiment =="fear")

characters_ep4<-unique(tidy_ep4$character)
characters_ep4

nr_words_per_ch<-tidy_ep4 %>% 
 summarize(count=n()) %>%
  sort(count, decreasing=TRUE)

#word counts 
tidy_ep4 %>%
  count(word,sort=TRUE)
  
#Trust words for C-3PO
tidy_ep4 %>%
  filter(character=="THREEPIO") %>%
  inner_join(nrc_trust) %>%
  count(word,set=TRUE)