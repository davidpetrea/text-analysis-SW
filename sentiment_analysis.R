
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
nrc_fear<-get_sentiments("nrc") %>%
  filter(sentiment =="fear")


#Trust words for C-3PO
tokens %>%
  filter(character=="THREEPIO") %>%
  inner_join(nrc_trust) %>%
  count(word,set=TRUE)

#Sections 25
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
  top_n(5) %>%
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

#Group by sentiment
tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var="n",fill=0) %>%
  comparison.cloud(colors=c("gray20","gray80"),
                   max.words=100)


