# Load necessary libraries
library(tm)
library(SnowballC)
library(ggraph)
library(igraph)

# Read the dataset
data <- read.csv("Data/reconstructed_cleaned_df.csv", stringsAsFactors = FALSE)
head(data)
# Create a Corpus
corpus <- Corpus(VectorSource(data$full_text))
inspect(corpus)
# Preprocess the text data
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
# Create the Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)


## Building LDA model with 5 topics to determine if model can identify the categories

# Fit the LDA model
library(topicmodels)
lda <- LDA(dtm, k = 5, control = list(seed = 1234))
# Get the topics
topics <- terms(lda, 10)
print(topics)

# Get the topic probabilities for each document
topic_probabilities <- posterior(lda)$topics

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assign dominant topic to each document
dominant_topic <- apply(topic_probabilities, 1, which.max)
data$dominant_topic <- dominant_topic

# Prepare data for heatmap
heatmap_data <- data %>%
  group_by(Label, dominant_topic) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Label) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# Create the heatmap
ggplot(heatmap_data, aes(x = as.factor(dominant_topic), y = Label, fill = percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red", name = "% of Assignments") +
  labs(
    title = "Heatmap of Topic Assignments",
    x = "Assigned Topics",
    y = "True Categories"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    panel.grid = element_blank()
  )
#save the plot
ggsave("Plots/heatmap.png", width = 8, height = 6, bg = "white")
# We see that each topic identifies a category


## Building LDA model with k topics. Optimize k. 

library(tidyr)
library(widyr)

for (curr_k in 6:12) {
  
  cat("\nFitting LDA for k =", curr_k, "...\n")
  
  #Fit the LDA model
  lda_model <- LDA(dtm, k = curr_k, control = list(seed = 1234))
  
  # Extract top-10 words per topic
  #    We compute a "beta_matrix" = exp(lda_model@beta)
  beta_matrix <- exp(lda_model@beta)  # Dimensions: k x vocabulary
  terms_vec   <- Terms(dtm)          # All terms in DTM
  
  # Build a data frame of top-10 words for each topic
  top_words_list <- lapply(seq_len(nrow(beta_matrix)), function(topic_idx) {
    beta_vals   <- beta_matrix[topic_idx, ]
    top_indices <- order(beta_vals, decreasing = TRUE)[1:10]
    data.frame(
      Topic = topic_idx,
      Rank  = 1:10,
      Word  = terms_vec[top_indices],
      Beta  = beta_vals[top_indices]
    )
  })
  
  # Combine into a single data frame
  top_words_df <- do.call(rbind, top_words_list) %>%
    arrange(Topic, -Beta) %>%
    as_tibble()
  
  # For each topic, compute pairwise correlations among these top-10 words
  #    Accumulate them into one data frame
  all_topic_corrs_k <- data.frame()  # Will store item1, item2, correlation, n, topic, k
  
  # Loop over each topic
  for (t in seq_len(curr_k)) {
    
    # Get the top-10 words for topic t
    top_words_for_t <- top_words_df %>%
      filter(Topic == t) %>%
      pull(Word)
    
    # Filter df_tokens to only those top-10 words
    corr_topic_t <- df_tokens %>%
      filter(word %in% top_words_for_t) %>%
      pairwise_cor(
        item    = word,      # the 'item' to correlate
        feature = doc_id,    # the grouping variable (documents)
        sort    = TRUE
      )
    
    # To be safe, remove any NaN correlations
    corr_topic_t <- corr_topic_t %>%
      filter(!is.na(correlation))
    
    # Add columns for topic and k
    corr_topic_t <- corr_topic_t %>%
      mutate(topic = t, k = curr_k)
    
    # Row-bind to data frame for this k
    all_topic_corrs_k <- bind_rows(all_topic_corrs_k, corr_topic_t)
  }
  
  # Save the combined data frame 
  output_file <- sprintf("LDA_results/LDA_results_k_%d.csv", curr_k)
  write.csv(all_topic_corrs_k, file = output_file, row.names = FALSE)
  
  cat("Saved pairwise correlations for k =", curr_k, "to", output_file, "\n")
}


library(purrr)
library(readr)

results_df <- map_dfr(6:12, function(curr_k) {
  input_file <- paste0("LDA_results/LDA_results_k_", curr_k, ".csv")
  corr_data  <- read_csv(input_file)  
  
  # Return a tibble with columns: k, avg_correlation
  tibble(
    k = curr_k,
    avg_correlation = mean(corr_data$correlation, na.rm = TRUE)
  )
})
results_df <- results_df %>%
  arrange(desc(avg_correlation))
results_df
# Bar plot of results
ggplot(results_df, aes(x = factor(k), y = avg_correlation)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Average Internal Correlation by Number of Topics",
    x = "Number of Topics",
    y = "Average Internal Correlation"
  ) +
  theme_minimal()
# Save the plot
ggsave("Plots/avg_internal_correlation.png", width = 6, height = 8, bg = "white")

# We see that k=6 produces topics with the highest average internal correlation. 
# We will use k=6 for the final LDA model


# Network plots for each topic in LDA model with k=6

library(patchwork)
df6 <- read.csv("LDA_results/LDA_results_k_6.csv", stringsAsFactors = FALSE)

# Function to plot one topic
plot_topic_network <- function(topic_num, data = df6, corr_threshold = 0) {
  # Filter for one topic and threshold
  df_topic <- data %>%
    filter(topic == topic_num, correlation > corr_threshold)
  
  # If no edges are left, return a fallback blank ggplot
  if (nrow(df_topic) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste("No edges for Topic", topic_num)) +
        theme_void()
    )
  }
  
  # Otherwise, build an igraph and plot
  g <- graph_from_data_frame(df_topic, directed = FALSE)
  
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void() +
    ggtitle(paste("Topic", topic_num))
}

# Generate plots for topics 1 to 6
plot_list <- lapply(1:6, plot_topic_network, data = df6, corr_threshold = 0)
# Arrange plots in a grid (2x3)
final_plot <- wrap_plots(plot_list, ncol = 3) + 
  plot_layout(guides = "collect") & 
  theme(plot.background = element_rect(color = "black", linewidth = 1))
final_plot
# Save the plot
ggsave("Plots/network_plots_topic_1_to_6.png", final_plot, width = 12, height = 8)