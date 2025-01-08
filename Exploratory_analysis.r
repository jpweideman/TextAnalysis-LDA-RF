# Load necessary libraries
library(tidyverse)
library(tidytext)
library(sentimentr)

# Read the dataset
cleaned_df <- read.csv("Data/cleaned_df.csv")

# Perform sentiment analysis for each Label
sentiment_scores <- cleaned_df %>%
    inner_join(get_sentiments("bing")) %>%
    count(Label, sentiment, sort = TRUE) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_score = positive - negative)
# View the sentiment scores
print(sentiment_scores)







