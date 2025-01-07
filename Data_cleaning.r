# Load necessary libraries
library(tidyverse)
library(tidytext)

# Read in your data (replace 'df_file.csv' with your file path)
df <- read_csv("df_file.csv")

# Tokenize the combined text into words
cleaned_df <- df |>
  unnest_tokens(word, combined_text) |>
  # Remove stop words
  anti_join(stop_words, by = "word") |>
  # Remove numbers
  filter(!str_detect(word, "^[0-9]+$")) |>
  # Remove punctuation
  filter(!str_detect(word, "[[:punct:]]"))

# Count word frequencies (optional)
word_freq <- cleaned_df |>
  count(word, sort = TRUE)
word_freq
# The word "Â " appears as the second most frequent word.
# We see that it occurs before every monetary value. 
# The Â character is an artifact and should be removed

# Remove the Â character from the text
cleaned_df <- cleaned_df %>%
    mutate(word = str_replace_all(word, "â", "")) %>%
    filter(word != "")


cleaned_df |>
  count(word, sort = TRUE) |>
  filter(n > 600) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
