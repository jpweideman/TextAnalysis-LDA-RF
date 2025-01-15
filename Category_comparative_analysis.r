# Load necessary libraries
library(tidyverse)
library(tidytext)
library(sentimentr)
library(qdap)
library(dplyr)
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
ggsave("Plots/sentiment_scores_plot.png", width = 7, height = 6, bg = "white")

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
ggsave("Plots/sentiment_scores_avg_polarity_plot.png", width = 7, height = 6, bg = "white")

# Visualize polarity with box plot
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


## TF and TF-IDF Analysis

# Read the dataset
cleaned_df <- read.csv("Data/reconstructed_cleaned_df.csv", stringsAsFactors = FALSE)

# Term Frequency distribution by category
# Tokenize the words
category_words <- cleaned_df %>%
  unnest_tokens(word, full_text) %>%
  count(Label, word, sort = TRUE) %>%
  ungroup()
# Calculate the total number of words for each category
total_words <- category_words %>%
  group_by(Label) %>%
  summarize(total = sum(n))
category_words <- left_join(category_words, total_words, by = "Label")
head(category_words)
# Plot the term frequency distribution
ggplot(category_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE, bins = 30, fill = "steelblue", color = "white") +
  xlim(NA, 0.01) +  # Adjust the limit to focus on small term frequencies
  facet_wrap(~Label, ncol = 2, scales = "free_y") +
  labs(
    title = "Term Frequency Distribution by Category",
    x = "Term Frequency (n/total)",
    y = "Count"
  ) +
  theme_minimal()
# Save the plot
ggsave("Plots/term_frequency_distribution.png", width = 7, height = 6, bg = "white")

# Term Frequency-Inverse Focument Frequency distribution by category
# Tokenize and calculate TF-IDF
tf_idf_words <- cleaned_df %>%
  unnest_tokens(word, full_text) %>%
  count(Label, word, sort = TRUE) %>%  # Count word occurrences by category
  bind_tf_idf(word, Label, n)          # Calculate TF-IDF
# View the top TF-IDF words
tf_idf_words %>%
  arrange(desc(tf_idf)) %>%
  head(10)  # Show the top 10 words for preview
# Visualize the top 15 TF-IDF words for each category
tf_idf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%  # Order words by TF-IDF
  group_by(Label) %>%
  top_n(15, tf_idf) %>%  # Select top 15 TF-IDF words for each category
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Label)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~Label, ncol = 2, scales = "free") +  # Create a faceted plot for each category
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal()
# Save the plot
ggsave("Plots/top_tf_idf_words_by_category.png", width = 9, height = 8, bg = "white")


## Most relevant words and bigrams analysis

# Read the dataset
cleaned_df <- read.csv("Data/reconstructed_cleaned_df.csv", stringsAsFactors = FALSE)

# Tokenize the words and create bigrams. Also, remove stop words
word_bigrams <- cleaned_df %>%
  unnest_tokens(bigram, full_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)  # Remove stop words
# Count the bigrams
bigram_counts <- word_bigrams %>%
  count(Label, word1, word2, sort = TRUE) %>%
  ungroup()
head(word_bigrams)
head(bigram_counts)

# Unite the bigrams for TF-IDF
word_bigrams_united <- word_bigrams %>%
  unite(bigram, word1, word2, sep = " ")

# TF-IDF for bigrams
bigram_tf_idf <- word_bigrams_united %>%
  count(Label, bigram) %>%
  bind_tf_idf(bigram, Label, n) %>%
  arrange(desc(tf_idf))
# View the top TF-IDF bigrams
head(bigram_tf_idf)
# Visualize the top 15 TF-IDF bigrams for each category
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%  # Order bigrams by TF-IDF
  group_by(Label) %>%
  top_n(15, tf_idf) %>%  # Select top 15 TF-IDF bigrams for each category
  ungroup() %>%
  ggplot(aes(bigram, tf_idf, fill = Label)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Bigrams_TF-IDF") +
  facet_wrap(~Label, ncol = 2, scales = "free") +  # Create a faceted plot for each category
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal()
# Save the plot
ggsave("Plots/top_tf_idf_bigrams_by_category.png", width = 9, height = 8, bg = "white")

# Visualize the bigram network with network graph
library(igraph)
library(ggraph)

visualize_bigrams <- function(bigrams, categories) {
  set.seed(1234)  # For reproducibility
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    filter(n > 19) %>%  # Filter for more frequent bigrams
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(
      aes(
        label = name,
        fontface = ifelse(name %in% categories, "bold", "plain"),  # Bold for categories
        size = ifelse(name %in% categories, 6, 4)  # Larger size for categories
      ),
      show.legend = FALSE,  # Remove size legend
      vjust = 1, hjust = 1
    ) +
    scale_size_identity() +  # Ensure size is used as defined
    theme_void() +
    labs(title = "Bigram Network")
}
categories <- unique(bigram_counts$Label)
# Visualize the bigram network
visualize_bigrams(bigram_counts,categories)
# Save the plot
ggsave("Plots/bigram_network.png", width = 15, height = 15, bg = "white")


## Pairwise correlation analysis for word networks

library(SnowballC)
library(widyr)
library(igraph)
library(ggraph)
library(patchwork)
cleaned_df <- read.csv("Data/reconstructed_cleaned_df.csv", stringsAsFactors = FALSE)

# List of categories 
categories <- unique(cleaned_df$Label)
# Create a list to store the plots
plots <- list()
# Loop through each category and plot
for (category in categories) {
  cleaned_df_words <- cleaned_df %>%
    filter(Label == category) %>%
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0) %>%
    unnest_tokens(word, full_text) %>%
    filter(!word %in% stop_words$word) %>%
    mutate(word = wordStem(word))
  
  word_cors <- cleaned_df_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  # Remove NaN and Inf values
  word_cors <- word_cors[!is.na(word_cors$correlation),]
  word_cors <- word_cors[!is.infinite(word_cors$correlation),]

  # Generate the plot
  plot <- word_cors %>%
    filter(correlation > .70) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void() +
    labs(title = paste(" Word Correlation Network for", category))
  
  # Store the plot in the list
  plots[[category]] <- plot
}
# Combine the plots of different catgories into one plot 
combined_plot <- wrap_plots(plots, ncol = 2) + 
  plot_layout(guides = "collect") & 
  theme(plot.background = element_rect(color = "black", linewidth = 1))
# Save the combined plot
ggsave("Plots/combined_word_correlation_network.png", plot = combined_plot, width = 15, height = 15, bg = "white")
