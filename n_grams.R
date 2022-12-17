#Bigrams summary for trilogy using custom function
bigrams_summary(trilogy)

#Trigrams summary for trilogy using custom function
trigrams_summary(trilogy)

#Sections
trilogy_sections_words<-trilogy %>%
  mutate(section=row_number()%/%10) %>%
  filter(section>0) %>%
  unnest_tokens(word, dialogue) %>%
  filter(!word %in% en_stopwords)

word_pairs <- trilogy_sections_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs %>%
  filter(item1 == "yoda")

#Words often found within the same section as "yoda"
#jedi, time, master, force, training

word_pairs %>%
  filter(item1 == "dark")

#Words often found within the same section as "vader"
#lord, luke, ship

#Coef Phi
word_cors <- trilogy_sections_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
word_cors

word_cors %>%
  filter(item1 == "force")

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
  geom_node_point(color = "orange", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
