# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)

# 1. Data Loading and Initial Checks

# Load the dataset
df <- read.csv("/Users/syedreehan/Desktop/Sample.csv")

# Display the count of missing values for each column
missing_values_count <- sapply(df, function(x) sum(is.na(x)))
cat("Missing Values Count:\n")
print(missing_values_count)
cat("\n")

# Display the count of duplicated rows
duplicates_count <- sum(duplicated(df))
cat("Number of Duplicated Rows:", duplicates_count, "\n\n")

# Display the data types of each column
data_types <- sapply(df, class)
cat("Data Types for Each Column:\n")
print(data_types)
cat("\n")

# 2. Visualizing Target Variables

# Display the distribution of attack categories
print(
  ggplot(df, aes(x = attack_cat)) + 
    geom_bar() + 
    theme_minimal() + 
    labs(title = "Distribution of Attack Categories", x = "Attack Category", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# Display the distribution of label
print(
  ggplot(df, aes(x = factor(label))) + 
    geom_bar() + 
    theme_minimal() + 
    labs(title = "Distribution of Label", x = "Label", y = "Count")
)

# 3. Analysis of Numeric Columns

# Identify numeric and categorical columns
numeric_cols <- names(df)[sapply(df, is.numeric)]

# Initialize a vector to store columns that rejected the null hypothesis
rejected_hypothesis <- c()

# Analyze each numeric column
for (col in numeric_cols) {
  correlation_test <- cor.test(df[[col]], df$label, method="pearson")
  
  # Display the result of the correlation test
  cat(paste("Correlation Test for column", col, ":\n"))
  print(correlation_test)
  
  # Check the p-value to determine if we reject the null hypothesis
  if (correlation_test$p.value < 0.05) {
    cat("Reject the null hypothesis. Significant correlation between", col, "and label.\n\n")
    rejected_hypothesis <- c(rejected_hypothesis, col)
    
    # Visualize the column's distribution by label
    print(
      ggplot(df, aes_string(x = "factor(label)", y = col)) + 
        geom_boxplot() + 
        theme_minimal() + 
        labs(title = paste("Boxplot of", col, "by Label"), x = "Label", y = col) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  } else {
    cat("Do not reject the null hypothesis. No significant correlation between", col, "and label.\n\n")
  }
}

# 4. Analysis of Categorical Columns

categorical_cols <- setdiff(names(df), numeric_cols)

# Visualize each categorical column's distribution by label
for (col in categorical_cols) {
  print(
    ggplot(df, aes_string(x = col, fill = "factor(label)")) + 
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = paste("Distribution of", col, "with respect to Label"), x = col, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# 5. Summary Visualization

# Display a bar graph showing the number of columns that rejected vs accepted the null hypothesis
df_hypothesis_summary <- data.frame(
  hypothesis = c("Rejected", "Accepted"),
  count = c(length(rejected_hypothesis), length(numeric_cols) - length(rejected_hypothesis))
)

print(
  ggplot(df_hypothesis_summary, aes(x = hypothesis, y = count, fill = hypothesis)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    labs(title = "Number of Columns Rejecting vs Accepting Null Hypothesis", x = "Hypothesis Result", y = "Count of Columns") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# 6. Correlation Visualization

# Compute correlations for numeric columns
correlations <- cor(df[sapply(df, is.numeric)], method = "pearson")

# Display the correlation of each column with 'label'
corr_with_label <- correlations['label',]
sorted_corr_with_label <- sort(corr_with_label, decreasing = TRUE, method = "radix")
print(
  barplot(sorted_corr_with_label, las=2, main="Correlation with Label", col="steelblue", cex.names=0.7)
)
https://drive.google.com/drive/folders/1U7FiQgn7UFTM9Tax1-my7yC1VcQIANE0