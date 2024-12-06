library(tidyr)
library(dplyr)
library(palmerpenguins)
library(ggplot2)

# Load and clean data
data("penguins")

# Check for missing values
penguins %>% summarise(across(everything(), ~sum(is.na(.))))

# Remove rows with missing values for simplicity and data accuracy
penguins <- penguins %>% drop_na()

# Convert categorical variables to factors
penguins <- penguins %>%
  mutate(across(c(species, island, sex), as.factor))

# EDA: Summary statistics by species
penguins %>%
  group_by(species) %>%
  summarise(across(c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
                   list(mean = ~mean(.), sd = ~sd(.), min = ~min(.), max = ~max(.)), .names = "{col}_{fn}"))

# Boxplot of bill length and flipper length by species
ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Bill Length by Species", y = "Bill Length (mm)", x = "Species")

ggplot(penguins, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Flipper Length by Species", y = "Flipper Length (mm)", x = "Species")

# Perform PCA: Select and scale numerical columns
pca_data <- penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  scale()

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# PCA biplot
biplot(pca_result, scale = 0)

# Extract PCA scores for clustering
pca_scores <- as.data.frame(pca_result$x[, 1:2])

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(pca_scores, centers = 3, nstart = 25)

# Add cluster assignments to the dataset
penguins <- penguins %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Plot clusters using PCA scores
ggplot(pca_scores, aes(x = PC1, y = PC2, color = penguins$cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering (k = 3) on PCA Results",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()
