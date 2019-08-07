## @knitr load_data

url <- './six-napoleons.html'

# Load the html file for use.
html_data <- read_html(url)



## @knitr clean_data

# Extract the character associated with each paragraph.
character_names <- html_nodes(html_data, 'p') %>%
  html_attr('character')

# Clean the paragraphs
chunks <- html_nodes(html_data, 'p') %>%
  html_text() %>%
  tolower() %>%
  # Replace new lines characters and unicode dashes with spaces.
  str_replace_all(pattern = "\n    |â\u0080\u0094|\n", replacement = " ") %>%
  # Replace unicode apostrophes with ascii counterparts.
  str_replace_all(pattern = "â\u0080\u0099|â\u0080\u0098", replacement = "'") %>%
  # Drop all punctuation
  str_replace_all(pattern = "[.,!?;:'&^#]|'s", replacement = "") %>%
  # Remove whitespace at the start and end of sentences.
  str_trim()

# Extract tokens from within quotation marks for characters.
chunks[which("Narration" != character_names)] <- chunks[which("Narration" != character_names)] %>%
  str_extract_all(pattern = '"([^"]*)"')

# Join all tokens collections that are produced through str_extract_all(...)
chunks <- lapply(chunks, function(l) {
  l %>%
    paste(collapse = ' ') %>%
    # Remove all quotation marks which are left in through the extraction process.
    str_replace_all(pattern = "\"", replacement = "")
  })


## @knitr tokenization

characters <- unique(character_names)

metadata <- c()
# A map containing a list of the tokens for each character.
tokens <- list()
tokens.frequency <- c()

for (name in characters) {
  # Combine all the distinct chunks into a single large string.
  tokens[name] <- paste(chunks[which(name == character_names)], collapse = ' ') %>%
    str_trim() %>%
    text_tokens()
  
  freq_frame <- as.data.frame(table(tokens[name]))
  token_frequencies <- cbind.data.frame(rep(name, length(freq_frame$Freq)), freq_frame)
  
  tokens.frequency <- tokens.frequency %>% 
    rbind.data.frame(token_frequencies)
  
  metadata <- rbind.data.frame(metadata, cbind.data.frame(character=name, tokens=length(tokens[[name]])))
}

colnames(tokens.frequency) <- c("character", "word", "frequency")

metadata <- metadata %>%
  cbind.data.frame(total_tokens=rep(sum(metadata$tokens), length(metadata$tokens)))

metadata.characters <- metadata %>%
  filter(character %ni% c("Narration"))

metadata.characters <- metadata.characters %>%
  mutate(total_tokens = sum(tokens))

metadata.plot <- ggplot(data=metadata.characters, aes(x=reorder(character, -tokens), y=tokens, fill=character)) +
  geom_bar(stat="identity", position = position_dodge(preserve = "single")) +
  geom_text(aes(label = sprintf("%s\n(%1.1f%%)", tokens, (tokens/total_tokens) * 100)), position=position_stack(vjust=0.5), size = 3) +
  labs(title = "Conversational Contribution by Character",
       subtitle = "The number and proportion of tokens in dialogue for each character.",
       caption = "Figure 1: Examining the conversational contributions of each character through the number of tokens in dialogue.",
       x = "Character Name",
       y = "Tokens") +
  theme_light() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0, face = "italic"))

## @knitr sentiment
sentiment <- tokens.frequency %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  filter(sentiment %ni% c("positive", "negative")) %>%
  filter(character %ni% c("Narration"))

sentiment._totals <- sentiment %>%
  distinct(character, word) %>%
  group_by(character) %>%
  tally(name = "total_count")

sentiment <- sentiment %>%
  group_by(character, sentiment) %>%
  tally(name="count")

sentiment <- sentiment %>%
  # Fill in missing sentiment counts.
  complete(character, sentiment, fill = list(count = 0)) %>%
  filter(character %ni% c("Narration")) %>%
  # Consider using metadata counts here.
  inner_join(sentiment._totals, by="character")

sentiment.plot <- ggplot(data=sentiment, aes(x=reorder(sentiment, -(count/total_count)), y=(count/total_count), fill=character)) +
  geom_bar(stat="identity", position = "dodge", width = 0.9, color = "grey40", size=0.2) +
  geom_text(aes(label = sprintf("   %s - %1.1f%%", character, (count/total_count) * 100), group = character), 
            angle = 90, size = 3,
            hjust = 0,
            position = position_dodge(width = 0.9)) +
  scale_y_continuous(labels=percent_format(), limits = c(0, 1)) +
  labs(title = "Conversational Sentiments by Character",
       subtitle = "The proportion of character's dialoge with a given sentiment.",
       caption = "Figure 2: Visualizing the sentiments expressed by each character in dialogue",
       x = "Sentiment",
       y = "Proportion of tokens") +
  theme_light() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, face = "italic"))

## @knitr sentiment_summary

sentiment.summary <- sentiment %>%
  group_by(sentiment) %>%
  summarise(average = mean(count/total_count))

sentiment.summary.plot <- ggplot(data=sentiment.summary, aes(x=reorder(sentiment, -average), y=average, fill=sentiment)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = sprintf("%1.1f%%", average * 100)), position=position_stack(vjust=0.5), size = 3) +
  scale_y_continuous(labels=percent_format(), limits = c(0, 1)) +
  labs(x = "",
       y = "",
       title = "Average Dialogue Sentiment",
       subtitle = "The average proportion of dialogue with a given sentiment.",
       caption = "Figure 3: The individual sentiment proportions within dialogue, with the clear leaders being trust, fear, and anticipation.") +
  theme_light() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, face = "italic"))

## @knitr overall_proportion
metadata.prop.total <- (metadata %>%
                             summarise(prop = sum(tokens)))[[1]]

metadata.prop.dialogue <- (metadata %>%
                             filter(character %ni% c("Narration")) %>%
                             summarise(prop = sum(tokens)))[[1]]

metadata.prop.narration <- (metadata %>%
                              filter(character %in% c("Narration")) %>%
                              summarise(prop = sum(tokens)))[[1]]

metadata.prop <- cbind.data.frame(type="Narration", proportion=metadata.prop.narration/metadata.prop.total)
metadata.prop <- metadata.prop %>%
  rbind.data.frame(cbind.data.frame(type="Dialogue", proportion=metadata.prop.dialogue/metadata.prop.total))

metadata.prop.plot <- ggplot(data=metadata.prop, aes(x="", y=proportion, fill=type)) +
  # Convert to pie chart.
  geom_col() +
  geom_text(aes(label = sprintf("%1.1f%%", proportion * 100)), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  labs(x = "",
       y = "",
       title = "Proportion of Tokens in Narration vs. Character Speech",
       subtitle = "Comparing the proportion of tokens in speech.") +
  theme_light() +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        axis.text.x=element_blank(),
        panel.border = element_rect(color = NA, fill = NA),
        panel.grid = element_line(color = NA),
        plot.caption = element_text(hjust = 0, face = "italic"))




