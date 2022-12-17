# Transform the text to a tidy data structure with one token per row
tokens <- trilogy %>%  
  group_by(episode) %>%
  mutate(linenumber=row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, dialogue)

# Positive and negative words -bing lexicon
tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)

#NRC Lexicon
nrc_all<-get_sentiments("nrc")

nrc_trust <-get_sentiments("nrc") %>%
  filter(sentiment =="trust")
#Trust words for C-3PO
tokens %>%
  filter(character=="THREEPIO") %>%
  inner_join(nrc_trust) %>%
  count(word,sort=TRUE)
#Trust words for C-3PO

nrc_fear<-get_sentiments("nrc") %>%
  filter(sentiment =="fear")
#Fear words for Luke
tokens %>%
  filter(character=="LUKE") %>%
  inner_join(nrc_fear) %>%
  count(word,sort=TRUE)
#Fear words for Luke
tokens %>%
  filter(character=="VADER") %>%
  inner_join(nrc_fear) %>%
  count(word,sort=TRUE)


#Sections 10
trilogy_sentiments_10 <-tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(episode,index=linenumber %/% 10, sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positive - negative)

ggplot(trilogy_sentiments_10, aes(index,sentiment,fill=episode)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode,ncol=2,scales="free_x")

#Afinn 20 -tokens
afinn_20<-tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index=linenumber %/% 20) %>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#Graphs
bind_rows(afinn_20) %>%
  ggplot(aes(index,sentiment,fill=method)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~method,ncol=2,scales="free_y")

#Word freq contribution
bing_word_counts <-tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE) %>%
  ungroup()
bing_word_counts

#contribution to negative/positive sentiment
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment,scales="free_y") +
  labs(y="Contribution to sentiment",
       x=NULL) + coord_flip()

#Wordclouds
#Ordered
wordcloud_test<-tokens %>%
  anti_join(stop_words) %>%
  count(word)

wordcloud2(wordcloud_test, size=0.4)


# Sentiments and frequency associated with each word  
sentiments <- tokens %>% 
  inner_join(nrc_all, "word") %>%
  count(word, sentiment, sort=TRUE) 

# Frequency of each sentiment
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw() 

# Top 10 terms for each sentiment
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 

# Sentiment analysis for the Top 10 characters with more dialogues
tokens %>%
  filter(character %in% c("LUKE","HAN","THREEPIO","LEIA","VADER",
                          "BEN","LANDO","YODA","EMPEROR","RED LEADER")) %>%
  inner_join(nrc_all, "word") %>%
  count(character, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~character, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw() 


# Tokens without stopwords
top_chars_tokens <- trilogy %>%
  mutate(dialogue=as.character(trilogy$dialogue)) %>%
  filter(character %in% c("LUKE","HAN","THREEPIO","LEIA","VADER",
                          "BEN","LANDO","YODA","EMPEROR","RED LEADER")) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(mystopwords, by="word")

# Most frequent words for each character - before tf_idf
top_chars_tokens %>%
  count(character, word) %>%
  group_by(character) %>% 
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, character, sep="__"), 
                      levels=rev(paste(word, character, sep="__"))))%>%
  ggplot(aes(x=word2, y=n)) +
  geom_col(aes(fill=character), show.legend=FALSE) +
  facet_wrap(~character, scales="free_y") +
  labs(x="Sentiment", y="Frequency") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()


# Most relevant words for each character - tf_idf
top_chars_tokens %>%
  count(character, word) %>%
  bind_tf_idf(word, character, n) %>%
  group_by(character) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, character, sep="__"), 
                      levels=rev(paste(word, character, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=character), show.legend=FALSE) +
  facet_wrap(~character, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()