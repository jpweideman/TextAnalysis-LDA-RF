# Load necessary libraries
library(tidyverse)
library(tidytext)
library(sentimentr)
library(qdap)

# Read the dataset
cleaned_df <- read.csv("Data/cleaned_df.csv")
cleaned_df

# Perform sentiment analysis for each Label
sentiment_scores <- cleaned_df %>%
    inner_join(get_sentiments("bing")) %>%
    count(Label, sentiment, sort = TRUE) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative)
# View the sentiment scores
print(sentiment_scores)

# Aggregate tokenized words back into sentences
cleaned_df <- read.csv("Data/cleaned_df.csv", stringsAsFactors = FALSE)
reconstructed_text <- cleaned_df %>%
  group_by(Label, Text_number) %>%
  summarize(
    full_text = paste(word, collapse = " "), # Combine words into full text
    .groups = "drop"
  )
reconstructed_text
# Save the reconstructed text
write.csv(reconstructed_text, "Data/reconstructed_cleaned_df.csv", row.names = FALSE)

# Visualize the sentiment scores (average polarity)
# Sentiment analysis using qdap::polarity()
sentiment_results <- reconstructed_text %>%
  rowwise() %>%
  mutate(
    polarity_score = polarity(full_text)$all$polarity,
    word_count = polarity(full_text)$all$wc
  ) %>%
  group_by(Label) %>%
  summarize(
    avg_polarity = mean(polarity_score, na.rm = TRUE),
    total_word_count = sum(word_count, na.rm = TRUE)
  )
# Results
print(sentiment_results)
# Visualize sentiment scores by Category
library(ggplot2)
ggplot(sentiment_results, aes(x = Label, y = avg_polarity, fill = Label)) +
  geom_col(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Average Polarity by Category",
    x = "Category",
    y = "Average Polarity Score",
    fill = "Category"
  )
# Save the plot
ggsave("Plots/sentiment_scores_plot.png", width = 10, height = 6, bg = "white")

# Visualize the distribution of polarity with box plot
# Sentiment analysis using qdap::polarity()
sentiment_results <- reconstructed_text %>%
  rowwise() %>%
  mutate(
    polarity_score = polarity(full_text)$all$polarity,
    word_count = polarity(full_text)$all$wc
  ) %>%
  ungroup()
# Convert Label to a factor for proper grouping and aesthetic handling
sentiment_results <- sentiment_results %>%
  mutate(Label = as.factor(Label))  # Convert Label to factor
# Box plot
ggplot(sentiment_results, aes(x = Label, y = polarity_score, fill = Label)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.4) +  # Default includes outliers
  theme_minimal() +
  labs(
    title = "Polarity Score Box Plot by Category",
    x = "Category",
    y = "Polarity Score",
    fill = "Category"
  ) +
  theme(
    legend.position = "none",  # Hide legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
  )
# Save the box plot
ggsave("Plots/sentiment_box_plot.png", width = 10, height = 6, bg = "white")

# Perform sentiment analysis using the Bing lexicon for each category in the Label column
# Read the reconstructed cleaned data
cleaned_df <- read.csv("Data/reconstructed_cleaned_df.csv")
category_sentiment <- cleaned_df %>%
  unnest_tokens(word, full_text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Label, word, sentiment, sort = TRUE) %>%
  ungroup()
# Visualize the top words contributing to sentiment for each category
category_sentiment %>%
  group_by(Label, sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, Label)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Label + sentiment, scales = "free_y") +
  scale_x_reordered() +
  labs(y = "Contribution to sentiment",
       x = NULL,
       title = "Top Words Contributing to Sentiment by Category") +
  coord_flip()
# Save the plot
ggsave("Plots/top_words_contributing_to_sentiment_by_category.png", width = 10, height = 6, bg = "white")
