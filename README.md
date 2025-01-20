# TEXT-MINING-WITH-LDA-AND-RANDOM-FOREST

## Overview

This project performs text mining on a dataset of 2,225 text documents categorized into five themes: Politics, Sport, Technology, Entertainment, and Business. The primary objectives include:

1. **Data Cleaning**:
   - Data cleaning and tokenization.

2. **Exploratory Data Analysis: Category Comparison**:
   - Analysis of word frequency, bigrams, sentiment, and readability.

3. **Latent Dirichlet Allocation (LDA)**:
   - Topic modeling with LDA to identify thematic structures.
   - Optimization of the number of topics.

4. **Random Forest Classification**:
   - Building and optimizing a Random Forest model to classify documents by category.
   - Analyzing model performance and interpreting sensitivity, specificity, and AUC.

Each step offers insights into the structure and patterns of the dataset, demonstrating how exploratory, unsupervised, and supervised methods complement each other.

## Project Files

- `Data_cleaning.r`: Script for cleaning and preparing the dataset.
- `Category_comparative_analysis.r`: Script for comparative analysis of the categories, including sentiment, readability, and word relevance.
- `LDA.r`: Script for conducting LDA-based topic modeling and optimization.
- `RF.r`: Script for implementing and optimizing the Random Forest classification model.
- `Report.pdf`: A report of the analysis, findings, and methodologies used.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/jpweideman/TextAnalysis-LDA-RF
