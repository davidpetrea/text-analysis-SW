#NOTE: Run this script first so all required libraries are
#loaded and data is formatted properly for analysis.
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
library(stopwords)
setwd("C:/Users/wyver/Desktop/Master/An 2/Sem 1/Text/project")

# Read the text files
ep4 <- read.table("./texts/SW_EpisodeIV.txt")
ep5 <- read.table("./texts/SW_EpisodeV.txt")
ep6 <- read.table("./texts/SW_EpisodeVI.txt")

#Combine all 3 episodes
trilogy<-combine(ep4, ep5, ep6) %>%
  rename(episode=source) %>%
  mutate(across('episode',str_replace, 'ep4', 'Episode IV')) %>%
  mutate(across('episode',str_replace, 'ep5', 'Episode V')) %>%
  mutate(across('episode',str_replace, 'ep6', 'Episode VI')) 

#Stop words
en_stopwords<-stopwords::stopwords("en", source = "snowball")
mystopwords <- data.frame(word=en_stopwords)







