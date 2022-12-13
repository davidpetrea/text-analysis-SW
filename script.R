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
library(wordcloud)
library(wordcloud2) 
library(reshape2) 
library(gdata) 
library(stringr) 
setwd("C:/Users/wyver/Desktop/Master/An 2/Sem 1/Text/project")

# Read the text files
ep4 <- read.table("./texts/SW_EpisodeIV.txt")
ep5 <- read.table("./texts/SW_EpisodeV.txt")
ep6 <- read.table("./texts/SW_EpisodeVI.txt")

#Tokenize each episode
tidy_ep4 <-ep4 %>%
  group_by(character) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words)

tidy_ep5 <-ep5 %>%
  group_by(character) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words)

tidy_ep6 <-ep6 %>%
  group_by(character) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words)

characters_ep4<-unique(tidy_ep4$character)
characters_ep4
characters_ep5<-unique(tidy_ep5$character)
characters_ep5
characters_ep6<-unique(tidy_ep6$character)
characters_ep6

#top 5 word per character
top_5_word_count<-tidy_ep4 %>% 
 summarize(count=n()) %>%
 top_n(5) %>%
 arrange(desc(count))
 
top_5_word_count 

#word counts 
tidy_ep4 %>%
  count(word,sort=TRUE)
tidy_ep5 %>%
  count(word,sort=TRUE)
tidy_ep6 %>%
  count(word,sort=TRUE)


#Combine all 3 episodes
trilogy<-combine(ep4, ep5, ep6) %>%
  rename(episode=source) %>%
  mutate(across('episode',str_replace, 'ep4', 'Episode IV')) %>%
  mutate(across('episode',str_replace, 'ep5', 'Episode V')) %>%
  mutate(across('episode',str_replace, 'ep6', 'Episode VI')) 








