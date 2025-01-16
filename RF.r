# Load required libraries
library(dplyr)
library(tidyr)
library(stringi)
library(tidytext)
library(stopwords)
library(randomForest)
library(caret)

# Load Dataset
dataset <- read.csv('Data/reconstructed_cleaned_df.csv', stringsAsFactors = FALSE)
head(dataset)
#view dataset as a tibble
dataset <- as_tibble(dataset)
head(dataset)
# Create a column to use in the model
dataset_RF <- dataset   %>%
  unite(Merged,Label,Text_number,remove=F)
dataset_RF

# Training and Testing split for dataset
set.seed(191)
id_train <- sample(dataset_RF$Merged, nrow(dataset_RF)*0.80)
id_train

# Select only the top 500 TF-IDF words for each category in the training dataset
tf_idf <- dataset_RF %>%
    filter(Merged %in% id_train) %>%
    unnest_tokens(word, full_text, token = 'words') %>%
    count(Label, word, sort = TRUE) %>%
    bind_tf_idf(word, Label, n) %>%
    group_by(Label) %>%
    top_n(500)
tf_idf

# Filter the dataset to only include the top 500 TF-IDF words
Filtered_counts <- dataset_RF %>% 
    unnest_tokens(word, full_text, token = 'words') %>% 
    filter(word %in% tf_idf$word) %>% 
    count(Merged, word, sort = TRUE)
Filtered_counts

# Document Term Matrix
DTM_byhand <- Filtered_counts %>%
    spread(key = word, value = n, fill = 0) %>%
    tibble()
DTM_byhand

# - TRAINING SET
# Explanatory variables
x <- DTM_byhand %>%
    filter(Merged %in% id_train) %>%
    select(-Merged)

# Response
y <- DTM_byhand %>%
    filter(Merged %in% id_train) %>%
    separate(Merged, c("Label", "Text_number"), sep = "_", convert = TRUE) %>%
    pull(Label) %>%
    as.factor()

# - TEST SET
# Explanatory variables
xtest <- DTM_byhand %>%
    filter(!(Merged %in% id_train)) %>%
    select(-Merged)

# Response
ytest <- DTM_byhand %>%
    filter(!(Merged %in% id_train)) %>%
    separate(Merged, c("Label", "Text_number"), sep = "_", convert = TRUE) %>%
    pull(Label) %>%
    as.factor()


## RF MODEL with default parameters

library(randomForest)
set.seed(123)
opt_forest<- randomForest(x=x, y=y, xtest=xtest, ytest=ytest, importance = TRUE, ntree = 500, mtry = if (!is.null(y) && !is.factor(y))max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))))

library(caret)

confusionMatrix(data = unname(opt_forest$test$predicted), reference = ytest)
varImpPlot(opt_forest)
plot(1:500, opt_forest$test$err.rate[,"Test"],type="l")


## SINGLE TREE for Comparison

library(rpart)
set.seed(123)
# Fit the decision tree model
tree_0 <- rpart(y ~ ., 
                data = cbind(x, y), 
                method = "class", 
                parms = list(split = "Gini"), 
                cp = 0.0001)
# Predict on the test set
test_tree_1 <- predict(tree_0, 
                       newdata = xtest, 
                       type = "class")

# Generate the confusion matrix
confusionMatrix(data = test_tree_1, reference = ytest)



## GRID SEARCH to find optimal ntree and mtry 

library(randomForest)
library(caret)
library(dplyr)

# Define the parameter grid
ntree_values <- c(10, 100, 250, 500)              # Range of `ntree` values 
floor(sqrt(ncol(x)))
mtry_values <- seq(9, floor(sqrt(ncol(x)))+1, by = 20) # Range of `mtry` values

# Initialize a data frame to store results
results <- data.frame(
  ntree = integer(),
  mtry = integer(),
  Accuracy = numeric()
)

# Perform grid search
set.seed(123) # For reproducibility
iteration <- 1
for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    # Train the Random Forest model
    rf_model <- randomForest(
      x = x,
      y = y,
      ntree = ntree,
      mtry = mtry,
      xtest = xtest,
      ytest = ytest
    )
    
    # Calculate accuracy on the test set
    accuracy <- sum(rf_model$test$predicted == ytest) / length(ytest)
    
    # Append results
    results <- results %>%
      add_row(ntree = ntree, mtry = mtry, Accuracy = accuracy)
    
    # Save the Random Forest model as an RDS file
    saveRDS(
      rf_model,
      file = file.path("RF_results", paste0("rf_model_ntree_", ntree, "_mtry_", mtry, ".rds"))
    ) 
    # Increment iteration counter
    iteration <- iteration + 1
  }
}

# Save full results to a summary file
write.csv(
  results,
  file = file.path("RF_results", "full_results.csv"),
  row.names = FALSE
)

# Find the best combination of ntree and mtry
best_result <- results %>% arrange(desc(Accuracy)) %>% slice(1)
print(best_result)
# Best model has ntree = 250 and mtry = 29

best_model <- readRDS(file = file.path("RF_results", "rf_model_ntree_250_mtry_29.rds"))

# Variable Importance Plot
varImpPlot(best_model)
# Final Confusion Matrix
confusionMatrix(data = unname(best_model$test$predicted), reference = ytest)

# Plot the results in full_results.csv
results <- read.csv('RF_results/full_results.csv')
# Bar plot the results
library(ggplot2)
ggplot(results, aes(x = factor(mtry), y = Accuracy, fill = factor(ntree))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Random Forest Grid Search Results",
       x = "mtry",
       y = "Accuracy",
       fill = "ntree") +
  theme_minimal() +
  theme(legend.position = "top")
# Save the plot
ggsave("Plots/grid_search_results.png", width = 6, height = 9, dpi = 300, bg = "white")


# Generate the confusion matrix
conf_matrix <- confusionMatrix(data = unname(best_model$test$predicted), reference = ytest)


## ROC Curves

# Extract the votes matrix
votes <- best_model$test$votes

# View the votes
head(votes)

library(pROC)

# Initialize an empty list to store ROC data
roc_list <- list()

# Loop through each class to generate an ROC curve
for (i in 1:ncol(votes)) {
  # Extract true labels (binary: 1 if the sample belongs to the class, 0 otherwise)
  true_labels <- as.numeric(ytest == colnames(votes)[i])
  
  # Generate ROC curve using the votes for the class
  roc_list[[i]] <- roc(true_labels, votes[, i], plot = FALSE)
}

library(ggplot2)

# Combine ROC data for all classes
roc_data <- do.call(rbind, lapply(1:length(roc_list), function(i) {
  data.frame(
    TPR = roc_list[[i]]$sensitivities,
    FPR = 1 - roc_list[[i]]$specificities,
    Class = colnames(votes)[i]
  )
}))

# Calculate AUC values for each class
auc_values <- sapply(roc_list, function(x) auc(x))

# Combine AUC values with class names
roc_data <- do.call(rbind, lapply(1:length(roc_list), function(i) {
  data.frame(
    TPR = roc_list[[i]]$sensitivities,
    FPR = 1 - roc_list[[i]]$specificities,
    Class = paste0(colnames(votes)[i], " (AUC = ", sprintf("%.2f", auc_values[i]), ")")
  )
}))

# Plot ROC curves with AUC in the legend
ggplot(roc_data, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves by Class",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Class (AUC)"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
# Save the plot
ggsave("Plots/roc_curves_with_auc.png", width = 9, height = 8, dpi = 300, bg = "white")













# ## Cross-Validation


# # Define cross-validation method
# control <- trainControl(
#   method = "cv",    # k-fold cross-validation
#   number = 10,      # Number of folds
#   verboseIter = TRUE # Print training progress
# )

# # Define the grid with optimal parameters
# tune_grid <- expand.grid(
#   mtry = 29  # Optimal mtry value
# )
# # For cross validation we will use the full dataset
# # Combine explanatory variables and response
# x_full <- DTM_byhand %>% select(-Merged)
# y_full <- DTM_byhand %>%
#   separate(Merged, c("Label", "Text_number"), sep = "_", convert = TRUE) %>%
#   pull(Label) %>%
#   as.factor()

# # Train Random Forest with cross-validation on the entire dataset
# set.seed(1234)
# rf_cv <- train(
#   x = x_full,
#   y = y_full,
#   method = "rf",
#   metric = "Accuracy",        # Optimize for accuracy
#   tuneGrid = tune_grid,       # Use the optimal mtry
#   ntree = 100,                # Optimal ntree value
#   trControl = control         # Cross-validation settings
# )

# # Output cross-validation results
# print(rf_cv)
# print(paste("Cross-validated Accuracy:", max(rf_cv$results$Accuracy)))

# # Save the cross-validated model
# saveRDS(rf_cv, file = file.path("RF_results", "cross_validated_rf_model_full_data.rds"))



