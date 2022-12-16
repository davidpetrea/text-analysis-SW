#Custom function that prints a general summary (bigrams, wordcloud, top characters) given a text data frame
#with 2 columns: character and dialogue 
text_summary <- function(text) {
  print("Dialogues count:")
  print(length(text$dialogue)) #Replace dialogue with name of column where the text is
  print("Characters count:")
  print(length(unique(text$character)))   #Expects a column containing the character saying the line
  
  top_characters <- as.data.frame(sort(table(text$character), decreasing=TRUE))[1:10,]
  # Visualization 
  top_graph <-ggplot(data=top_characters, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="#56B4E9", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(x="Character", y="Number of dialogues")
  print(top_graph)
  
  #Wordcloud
  custom_wordcloud(text)
  
  #Bigrams
  bigrams_summary(text)
  
  #Trigrams
  trigrams_summary(text)
  
  
}

bigrams_summary <- function(text) {
  bigrams<-text %>%
    unnest_tokens(bigram, dialogue,token="ngrams",n=2) %>%
    separate(bigram,c("word1","word2"),sep=" ") %>%
    filter(!word1 %in% en_stopwords) %>%
    filter(!word2 %in% en_stopwords) %>%
    mutate(bigram = paste(word1,word2," ")) %>%
    drop_na()
  
  bigram_counts <-bigrams %>%
    count(bigram,sort=TRUE) %>%
    top_n(15) %>%
    slice(1:15)
  
  bigram_counts
  
  bigrams_top<-ggplot(data=bigram_counts, aes(x=reorder(bigram, -n), y=n)) +  
    geom_bar(stat="identity", fill="chocolate2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(x="Bigram", y="Frequency")
  print(bigrams_top)
}

trigrams_summary<-function(text) {
  trigrams<-text %>%
    unnest_tokens(trigram, dialogue,token="ngrams",n=3) %>%
    drop_na() %>%
    separate(trigram,c("word1","word2","word3"),sep=" ") %>%
    filter(!word1 %in% en_stopwords,
           !word2 %in% en_stopwords,
           !word3 %in% en_stopwords) %>%
    mutate(trigram = paste(word1,word2,word3," "))
  
  trigrams_counts <-trigrams %>%
    count(trigram,sort=TRUE) %>%
    top_n(15) %>%
    slice(1:15)
  
  trigrams_top<-ggplot(data=trigrams_counts, aes(x=reorder(trigram, -n), y=n)) +  
    geom_bar(stat="identity", fill="chocolate2", colour="black") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(x="Trigram", y="Frequency")
  print(trigrams_top)
}

custom_wordcloud <-function(text) {
  # Transform the text to a tidy data structure with one token per row
  episode_tokens <- text %>%  
    mutate(linenumber=row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, dialogue)
  
  #Wordcloud
  wordcloud_tokens <-episode_tokens %>%
    anti_join(stop_words) %>%
    count(word)
  print(wordcloud2(wordcloud_tokens, size=0.4))
 
}

text_summary(ep4)





