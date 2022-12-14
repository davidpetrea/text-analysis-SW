
# How many dialogues?
length(ep4$dialogue)

# How many characters?
length(levels(ep4$character))

top.ep4.chars <- as.data.frame(sort(table(ep4$character), decreasing=TRUE))[1:20,]

# Visualization 
ggplot(data=top.ep4.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")
