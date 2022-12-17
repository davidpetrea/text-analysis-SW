# Transform the text to a tidy data structure with one token per row
episode_words <- trilogy %>%  
  unnest_tokens(word, dialogue) %>%
  count(episode, word, sort = TRUE) %>% 
  ungroup()

total_words <- episode_words %>% 
  group_by(episode) %>% 
  summarize(total = sum(n))

episode_words <- left_join(episode_words, total_words) 

#Distributions
ggplot(episode_words, aes(n/total, fill = episode)) + 
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) + 
  facet_wrap(~episode, ncol = 2, scales = "free_y")


#Zipf's law
freq_by_rank <- episode_words %>% 
  group_by(episode) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank 

#Plot frequencies by rank
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = episode)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() 

#Linear model with a subset of ranks 10-600
rank_subset <- freq_by_rank %>%
  filter(rank < 600,
         rank > 10) 
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset) 

#Plot linear model over freq using estimated intercept and slope from above
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = episode)) + 
  geom_abline(intercept = -0.6319, slope = -1.0635 , color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() + 
  scale_y_log10()

#bind_tf_idf 
episode_words <- episode_words %>% 
  bind_tf_idf(word, episode, n) 
episode_words

#Desc sorting by tf_idf & remove total column
episode_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

#Plot highest tf_idf words for each episode
episode_words %>%
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(episode) %>%
  top_n(15) %>%
  ungroup %>% 
  ggplot(aes(word, tf_idf, fill = episode)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~episode, ncol = 2, scales = "free") + 
  coord_flip() 


