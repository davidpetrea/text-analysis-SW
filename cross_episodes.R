
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

#Frequencies
frequency<-bind_rows(mutate(tidy_ep4,episode="Ep4"),
                     mutate(tidy_ep5,episode="Ep5"),
                     mutate(tidy_ep6,episode="Ep6")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(episode,word) %>%
  group_by(episode) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(episode,proportion) %>%
  gather(episode, proportion, `Ep4`:`Ep5`)

#Scales
ggplot(frequency, aes(x = proportion, y = `Ep6`,
                      color = abs(`Ep6`- proportion))) +
  geom_abline(color="gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~episode,ncol=2) +
  theme(legend.position="none") +
  labs(y="Ep6",x=NULL)

#Correlation tests
cor.test(data=frequency[frequency$episode=="Ep4",], ~proportion + `Ep6`)

cor.test(data=frequency[frequency$episode=="Ep5",], ~proportion + `Ep6`)
#There is a stronger correlation between episode 5 & 6
