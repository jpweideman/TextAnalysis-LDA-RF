# Load necessary libraries
library(tidyverse)
library(tidytext)

# Read in the data
df <- read_csv("Data/df_file.csv")
df
# Create a column that numbers the rows
df <- df %>%
  mutate(Text_number = row_number())
df
#replace the catgories with their respective labels
df$Label <- factor(df$Label, levels = c(0, 1, 2, 3, 4), labels = c("Politics", "Sport", "Technology", "Entertainment", "Business"))
df
# Tokenize the text column into words, but differentiate between different texts
cleaned_df <- df |>
  unnest_tokens(word, Text) |>
  # Remove stop words
  anti_join(stop_words, by = "word") |>
  # Remove numbers
  filter(!str_detect(word, "^[0-9]+$")) |>
  # Remove punctuation
  filter(!str_detect(word, "[[:punct:]]"))
cleaned_df

# Count word frequencies 
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

word_freq <- cleaned_df |>
  count(word, sort = TRUE)

cleaned_df |>
  count(word, sort = TRUE) |>
  filter(n > 600) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#save the plot
ggsave("Plots/word_freq_plot.png", width = 10, height = 6, units = "in")

# Word cloud to visualize word frequencies
library(wordcloud)
word_freq %>%
  with(wordcloud(word, n, min.freq = 300, colors = rainbow(4), random.order = FALSE))
#save the plot
dev.copy(png, "Plots/word_cloud.png", width = 800, height = 800)
dev.off()

library(gridExtra)
# Word cloud for each category
wordcloud_list <- cleaned_df %>%
  count(Label, word, sort = TRUE) %>%  # Count word frequencies by category
  group_split(Label) %>%              # Split data by category
  map(function(data) {
    category <- unique(data$Label)    # Get the category name

    # Save each word cloud as a temporary image
    temp_file <- tempfile(fileext = ".png")
    png(temp_file, width = 400, height = 400)  # Increase the image size for larger word clouds
    with(data, wordcloud(words = word, freq = n, min.freq = 150, random.order = FALSE, colors = rainbow(4)))
    dev.off()

    # Create a ggplot object with the word cloud as an image and a title
    ggplot() +
      annotation_custom(
        grid::rasterGrob(png::readPNG(temp_file), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) +
      labs(title = paste(category)) +  # Add a title below each word cloud
      theme_void() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, vjust = -2)  # Center and position the title
      )
  })
# Combine all word clouds into a single grid
combined_plot <- grid.arrange(grobs = wordcloud_list, ncol = 2)  # Adjust ncol for layout
# Save the combined grid as a single image
ggsave(filename = "Plots/all_wordclouds_combined.png", plot = combined_plot, width = 12,  height = 12)

#save the cleaned data
write_csv(cleaned_df, "Data/cleaned_df.csv")
