# Load necessary libraries
library(tidyverse)
library(tidytext)
library(sentimentr)
library(qdap)

# Read the dataset
cleaned_df <- read.csv("Data/cleaned_df.csv")
head(cleaned_df)


## Sentiment Analysis

# Perform sentiment analysis for each Label
sentiment_scores <- cleaned_df %>%
    inner_join(get_sentiments("bing")) %>%
    count(Label, sentiment, sort = TRUE) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative)
# View the sentiment scores
print(sentiment_scores)
# Visualize the sentiment scores
ggplot(sentiment_scores, aes(x = Label, y = sentiment_score, fill = Label)) +
    geom_col(alpha = 0.8) +
    theme_minimal() +
    labs(
        title = "Sentiment Score by Category",
        x = "Category",
        y = "Sentiment Score",
        fill = "Category"
    )
# Save the plot
ggsave("Plots/sentiment_scores_plot.png", width = 6, height = 6, bg = "white")

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
ggsave("Plots/sentiment_scores_avg_polarity_plot.png", width = 6, height = 6, bg = "white")

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
  )
# Save the box plot
ggsave("Plots/sentiment_polarity_box_plot.png", width = 6, height = 6, bg = "white")

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


## Readability Analysis

library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(dplyr)
library(stringi)
library(quanteda)
library(RColorBrewer)

# Read the reconstructed cleaned data
cleaned_df <- read.csv("Data/reconstructed_cleaned_df.csv")

# Perform readability analysis
readability_results <- cleaned_df %>%
  mutate(readability = textstat_readability(as.character(full_text), 
                                            remove_hyphens = TRUE, 
                                            measure = c("Flesch", "Flesch.Kincaid", "Flesch.PSK", "Danielson.Bryan")))

# Define the Gulpease function
gulpease <- function(x, intermediate = FALSE) {
  dati.x = quanteda.textstats::textstat_readability(x, intermediate = TRUE)
  TotP = dati.x$W
  LP = 10 * dati.x$C
  FR = 300 * dati.x$St
  gulpease = 89 + (FR - LP) / TotP
  istruzione = dplyr::case_when(
    gulpease > 80 ~ "elementare",
    gulpease > 60 & gulpease < 80 ~ "media",
    gulpease > 40 & gulpease < 60 ~ "superiore"
  )
  if (intermediate == TRUE)
    tibble::tibble(gulpease(x), (dati.x)[, 3:5])
  else
    tibble::tibble("document" = dati.x$document, gulpease, istruzione)
}

# Apply the Gulpease function to the dataset
cleaned_df <- cleaned_df %>%
  mutate(gulpease = gulpease(as.character(full_text))$gulpease,
         Label = as.factor(Label))

# Calculate the average Gulpease Index for each label
label_averages <- cleaned_df %>%
  group_by(Label) %>%
  summarize(avg_gulpease = mean(gulpease, na.rm = TRUE))  # Calculate averages
# Plot readability scores with averages
ggplot(cleaned_df, aes(x = Label, y = gulpease, color = Label)) +
  geom_point(alpha = 0.6, size = 2) +  # Add individual points
  geom_smooth(se = TRUE, color = "black", alpha = 0.3) +  # Add trend line
  geom_point(data = label_averages, aes(x = Label, y = avg_gulpease), 
             color = "black", size = 6, shape = 4) +  # Add averages as black X marks
  geom_text(data = label_averages, 
            aes(x = as.numeric(Label) + 0.1, y = avg_gulpease, 
                label = paste0("Avg: ", round(avg_gulpease, 1))), 
            hjust = 0, color = "black", size = 6) +  # Add text to the side
  labs(
    title = "Gulpease Index by Category",
    x = "Category",
    y = "Gulpease Index"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Hide legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")  # Use a color palette for points
# Save the plot
ggsave("Plots/readability.png", width = 12, height = 8, bg = "white")
