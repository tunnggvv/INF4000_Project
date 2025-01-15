getwd()
setwd('/Users/a88698/Desktop/INF4000 Data visualization/report')

dataset <- read.csv("thefinaldataset.csv")
library(tidyverse)

# Ensure 'popularity' is numeric
dataset$popularity <- as.numeric(dataset$popularity)
# Select only numeric variables
numeric_data <- dataset %>%
  select_if(is.numeric)
melted_data <- numeric_data %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Load necessary libraries
library(reshape2)
library(ggplot2)
# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
# Melt the correlation matrix
cor_long <- melt(cor_matrix)

# Create the correlation heatmap with annotations
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add white gridlines
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "darkblue", high = "pink", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +  # Use minimal theme for clarity
  labs(title = "Correlation Matrix for Popularity",
       x = NULL, y = NULL) +  # Remove axis labels for a cleaner look
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  coord_fixed()  # Ensure cells are square

#random forest for feature importance
set.seed(123)
library(caTools)
split <- sample.split(dataset$popularity, SplitRatio = 0.7)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)

install.packages("randomForest", type = "binary")
library(randomForest)
str(train$popularity)
# Fit the model
rf_model <- randomForest(popularity ~ ., data = train, importance = TRUE)
# View feature importance
importance <- importance(rf_model)
print(importance)

library(ggplot2)
# Extract feature importance as a data frame
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)
colnames(importance_df) <- c("MeanDecreaseAccuracy", "MeanDecreaseGini")
importance_df$Feature <- rownames(importance_df)
# Plot the feature importance
# Select only numeric columns from the dataset
numeric_columns <- sapply(dataset, is.numeric)
numeric_data <- dataset[, numeric_columns]

# Create the bar chart
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(MeanDecreaseGini, 0), 
                hjust = ifelse(MeanDecreaseGini > 120000, 1.2, -0.2)), # Adjust horizontal position based on value
            size = 3.5, # Adjust text size
            color = "black") + # Text color
  coord_flip() +
  labs(
    title = "Feature Importance",
    x = "Features",
    y = "Mean Decrease in Gini"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center-align and bold title
    axis.title = element_text(size = 12, face = "bold"), # Style axis titles
    axis.text = element_text(size = 10) # Style axis labels
  )


#Adjust: Create scatter plots with facets
# Pivot dataset into a long format for faceting
long_data <- dataset %>%
  pivot_longer(cols = c(danceability, tempo, duration_ms, acousticness),
               names_to = "Variable",
               values_to = "Value")

# Calculate correlation coefficients for each variable
correlations <- long_data %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, popularity, use = "complete.obs")) %>%
  mutate(correlation_label = paste0("r = ", round(correlation, 2))) # Create labels
print(correlations)
# Create scatter plots with facets and correlation annotations
ggplot(long_data, aes(x = Value, y = popularity)) +
  geom_point(alpha = 0.3, size = 1, color = "orange") + # Adjust transparency and point size
  geom_smooth(method = "loess", color = "black", size = 1) + # Add trend lines
  facet_wrap(~ Variable, scales = "free_x") + # Create facets
  geom_text(
    data = correlations,
    aes(x = -2.5, y = 75, label = correlation_label), # Adjust position of labels
    inherit.aes = FALSE, # Prevent mapping conflicts
    color = "blue", size = 4, hjust = 0
  ) +
  labs(
    title = "Scatter Plots of Variables vs. Popularity",
    x = "Variable Value",
    y = "Popularity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12), # Style facet titles
    axis.title = element_text(size = 12, face = "bold"), # Style axis titles
    axis.text = element_text(size = 10) # Style axis text
  )


set.seed(123)
cluster_result <- kmeans(numeric_data, centers = 4)  # Adjust 'centers' for the number of clusters
dataset$cluster <- as.factor(cluster_result$cluster)

# violin plot:Explicit Content vs Popularity
ggplot(dataset, aes(x = explicit, y = popularity, fill = explicit)) +
  geom_violin(trim = TRUE, alpha = 0.8) +  # Adjust transparency for better layering
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, color = "black") +  # Add a boxplot inside for summaries
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +  # Add mean points
  stat_summary(fun = "median", geom = "text", aes(label = round(..y.., 1)), vjust = -0.5, color = "black") + # Add median value as text
  labs(
    title = "Explicit Content vs Popularity",
    x = "Explicit Content",
    y = "Popularity",
    fill = "Explicit"
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +  # Customize fill colors
  theme_minimal() +
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center-align and style title
    axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
    axis.text = element_text(size = 10)  # Style axis labels
  )

# violin plot:Mood category vs Popularity
ggplot(dataset, aes(x = mood_category, y = popularity, fill = mood_category)) +
  geom_violin(trim = TRUE, alpha = 0.8) +  # Adjust transparency for better layering
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, color = "black") +  # Add a boxplot
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +  # Add mean points
  stat_summary(fun = "median", geom = "text", aes(label = round(..y.., 1)), vjust = -0.5, color = "black") + # Add median value as text
  labs(
    title = "Mood Category vs Popularity",
    x = "Mood Category",
    y = "Popularity",
    fill = "Mood Category"
  ) +
  scale_fill_manual(
    values = c("steelblue", "firebrick", "goldenrod")  # Customize fill colors for all mood categories
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center-align and style title
    axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
    axis.text = element_text(size = 10)  # Style axis labels
  )
# violin plot:Speech category vs Popularity
ggplot(dataset, aes(x = speech_category, y = popularity, fill = speech_category)) +
  geom_violin(trim = TRUE, alpha = 0.8) +  # Adjust transparency for better layering
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, color = "black") +  # Add a boxplot
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +  # Add mean points
  stat_summary(fun = "median", geom = "text", aes(label = round(..y.., 1)), vjust = -0.5, color = "black") + # Add median value as text
  labs(
    title = "Speech Category vs Popularity",
    x = "Speech Category",
    y = "Popularity",
    fill = "Speech Category"
  ) +
  scale_fill_manual(
    values = c("steelblue", "firebrick", "goldenrod")  # Customize fill colors for all mood categories
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Adjust legend position
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center-align and style title
    axis.title = element_text(size = 12, face = "bold"),  # Style axis titles
    axis.text = element_text(size = 10)  # Style axis labels
  )

