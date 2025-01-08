# Load necessary libraries
library(tidyverse)
library(tidytext)
library(sentimentr)

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

# Visualize the sentiment scores
# Aggregate tokenized words back into sentences
cleaned_df <- read.csv("Data/cleaned_df.csv", stringsAsFactors = FALSE)
reconstructed_text <- cleaned_df %>%
  group_by(Label, Text_number) %>%
  summarize(
    full_text = paste(word, collapse = " "), # Combine words into full text
    .groups = "drop"
  )
reconstructed_text
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
# Visualize sentiment scores by label
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

