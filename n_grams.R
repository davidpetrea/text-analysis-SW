#Bigrams

en_stopwords<-stopwords::stopwords("en", source = "snowball")

trilogy_bigrams<-trilogy %>%
  unnest_tokens(bigram, dialogue,token="ngrams",n=2)

trilogy_bigrams %>%
  count(bigram,sort=TRUE)

bigrams_separated<-trilogy_bigrams %>%
  separate(bigram,c("word1","word2"),sep=" ")

bigrams_filtered<-bigrams_separated %>%
  filter(!word1 %in% en_stopwords) %>%
  filter(!word2 %in% en_stopwords)

bigram_counts<-bigrams_filtered %>%
  count(word1,word2,sort=TRUE)
bigram_counts

#tri-grams
triology_trigrams<-trilogy %>%
  unnest_tokens(trigram, dialogue,token="ngrams",n=3) %>%
  separate(trigram,c("word1","word2","word3"),sep=" ") %>%
  filter(!word1 %in% en_stopwords,
         !word2 %in% en_stopwords,
         !word3 %in% en_stopwords) %>%
  count(word1,word2,word3,sort=TRUE)


triology_sections_words<-trilogy%>%
  mutate(section=row_number()%/%10)%>%
  filter(section>0)%>%
  unnest_tokens(word, dialogue)%>%
  filter(!word%in%en_stopwords)

word_pairs <- triology_sections_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "yoda")

#Words often found within the same section as "yoda"
#jedi, time, master, force, training

word_pairs %>%
  filter(item1 == "vader")

#Words often found within the same section as "vader"
#lord, luke, ship

#Coef Phi
word_cors <- triology_sections_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
word_cors

word_cors %>%
  filter(item1 == "master")

#View correlations
word_cors %>%
  filter(item1 %in% c("dark", "princess", "jedi", "master")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~item1, scales = 'free') +
  coord_flip()

set.seed(123)

#nodes
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()