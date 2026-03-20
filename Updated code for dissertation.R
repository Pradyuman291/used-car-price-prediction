install.packages("caret")
install.packages("readxl")
install.packages("tidyverse")
library(dplyr)
library(readxl)
library(openxlsx)
library(dlookr)
library(dplyr)
library(pROC)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(glmnet)
library(caret)
library(MASS)
library(tidyverse)
library(caret)
library(e1071)
library(gbm)
library(xgboost)
library(nnet)
# Read the dataset
file_path <- "cardekho_dataset.csv"
data <- read.csv("D:/Queens Dissertation/cardekho_dataset.csv")
# Check the structure of the dataset
str(data)

# View the first few rows
head(data)

# Summary statistics
summary(data)

# Check which columns have missing values
colSums(is.na(data))

# Remove rows with missing values
data <- na.omit(data)

# Remove duplicate rows
data <- data[!duplicated(data), ]

# Check for duplicate rows in the dataset
duplicate_rows <- data[duplicated(data), ]

# Print the duplicate rows
print(duplicate_rows)
# Convert text data to lowercase
data$fuel_type <- tolower(data$fuel_type)
data$seller_type <- tolower(data$seller_type)
data$transmission_type <- tolower(data$transmission_type)
set.seed(40425764)
# Function to detect and remove outliers based on IQR method
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Remove rows with outliers
  df <- df %>%
    filter(df[[column_name]] >= lower_bound & df[[column_name]] <= upper_bound)
  
  return(df)
}
set.seed(40425764)
# List of numeric columns to check for outliers
numeric_columns <- c("vehicle_age", "km_driven", "mileage", "engine", "max_power", "selling_price")

# Loop through each numeric column and remove outliers
for (col in numeric_columns) {
  data1 <- remove_outliers(data, col)
}
set.seed(40425764)
# Save the cleaned dataset to a new CSV file
write.csv(data1, "cleaned_cardekho_dataset11.csv", row.names = FALSE)
set.seed(40425764)

# Confirm the dataset has been saved
cat("Cleaned dataset saved as 'cleaned_cardekho_dataset.csv'\n")
print(str(data1))
print(str(data))
df <- data1

set.seed(40425764)
# Identify categorical features (columns with factor data type)
categorical_features <- names(df)[sapply(df, is.factor)]

set.seed(40425764)

# Display cleaned dataframe information again
cat("\nCleaned dataframe information after outlier removal:\n")
print(str(df))

# Summary statistics of numerical columns
cat("\nSummary statistics of numerical columns:\n")
print(summary(df))

summary(df)
set.seed(40425764)
# Display the first 5 rows of the cleaned dataframe
cat("\nFirst 5 rows of the cleaned dataframe:\n")
print(head(df, 5))
set.seed(40425764)
# Unique values for categorical columns
cat("\nUnique values for categorical columns:\n")
cat("fuel_type:\n")
print(unique(df$fuel_type))
cat("seller_type:\n")
print(unique(df$seller_type))
cat("transmission_type:\n")
print(unique(df$transmission_type))

set.seed(40425764)
# Distribution of numerical columns
cat("\nDistribution of numerical columns:\n")
numeric_columns <- sapply(df, is.numeric)
for (col in names(df)[numeric_columns]) {
  cat(paste("\n", col, " distribution:\n", sep = ""))
  print(summary(df[[col]]))
}
########################################################################
# Create histograms and bar plots for the columns
# 1. Distribution of Selling Price
ggplot(df, aes(x = selling_price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle('Distribution of Selling Price') +
  xlab('Selling Price') +
  ylab('Density') +
  theme_minimal()
set.seed(40425764)
# 2. Distribution of Vehicle Age
ggplot(df, aes(x = vehicle_age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "green", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle('Distribution of Vehicle Age') +
  xlab('Vehicle Age') +
  ylab('Density') +
  theme_minimal()
set.seed(40425764)
# 3. Distribution of KM Driven
ggplot(df, aes(x = km_driven)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "purple", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle('Distribution of KM Driven') +
  xlab('KM Driven') +
  ylab('Density') +
  theme_minimal()
set.seed(40425764)
# 4. Distribution of Engine Size
ggplot(df, aes(x = engine)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle('Distribution of Engine Size') +
  xlab('Engine Size') +
  ylab('Density') +
  theme_minimal()
set.seed(40425764)
# 5. Distribution of Max Power
ggplot(df, aes(x = max_power)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "red", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "blue") +
  ggtitle('Distribution of Max Power') +
  xlab('Max Power') +
  ylab('Density') +
  theme_minimal()
set.seed(40425764)
# 6. Distribution of Seats
ggplot(df, aes(x = seats)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "brown", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle('Distribution of Seats') +
  xlab('Seats') +
  ylab('Density') +
  theme_minimal()
# 7. Distribution of Seats
ggplot(df, aes(x = mileage)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "brown", alpha = 0.7, color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  ggtitle('Distribution of Mileage') +
  xlab('Mileage') +
  ylab('Density') +
  theme_minimal()
set.seed(40425764)


################CATOGORICAL VARIABLES#############################
##1. Fuel Type Distribution
ggplot(df, aes(x = fuel_type)) +
  geom_bar(aes(y = ..count..), fill = "#69b3a2", color = "black", width = 0.7) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black", size=3.5) +
  ggtitle('Distribution of Fuel Types in the Dataset') +
  xlab('Fuel Type') +
  ylab('Count') +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f5f5f5"),
    plot.background = element_rect(fill = "#f5f5f5")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

set.seed(40425764)

library(ggplot2)

# Identify categorical features
categorical_features <- c("car_name", "brand", "model", "seller_type", "fuel_type", "transmission_type")
set.seed(40425764)
# Loop through each categorical feature and create a bar plot
for (col in categorical_features) {
  p <- ggplot(df, aes_string(x = col)) +
    geom_bar(aes(y = ..count..), fill = "#69b3a2", color = "black", width = 0.7) +
    geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black", size=3.5) +
    ggtitle(paste('Distribution of', col, 'in the Dataset')) +
    xlab(col) +
    ylab('Count') +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f5f5f5"),
      plot.background = element_rect(fill = "#f5f5f5")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  
  print(p)
}
set.seed(40425764)
######################Correlation#################
# Plot: orrelation Heatmap
library(reshape2)

# Select only numeric columns from the data frame
numeric_df <- df[, sapply(df, is.numeric)]

# Calculate the correlation matrix for numeric columns
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Melt the correlation matrix into a format suitable for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Plot the correlation heatmap with text annotations
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add correlation values
  theme_minimal(base_size = 15) + 
  ggtitle("Enhanced Correlation Heatmap") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed()
set.seed(40425764)
# Assuming 'df' is your dataframe, and you want to calculate the correlation matrix for numeric features
numeric_columns <- sapply(df, is.numeric)  # Identify numeric columns
correlation_matrix <- cor(df[, numeric_columns], use = "complete.obs")  # Calculate the correlation matrix

# Print the correlation matrix
print(correlation_matrix)

## 2.Transmission Type Distribution
ggplot(df, aes(x = transmission_type)) +
  geom_bar(fill = "yellow", color = "black") +
  ggtitle('Transmission Type Distribution') +
  xlab('Transmission Type') +
  ylab('Count') +
  theme_minimal()
##################################Chi-Sqaured########################
set.seed(40425764)
# Initialize a vector to store the Chi-Squared test results
chi2_test <- vector()

# Perform Chi-Squared test for each categorical feature
for (feature in categorical_features) {
  
  # Create a contingency table between the target variable and the feature
  contingency_table <- table(df[[feature]], df$selling_price)
  
  # Perform the Chi-Squared test
  test_result <- chisq.test(contingency_table)
  
  # Store the result based on the p-value
  if (test_result$p.value < 0.05) {
    chi2_test <- c(chi2_test, 'Reject Null Hypothesis')
  } else {
    chi2_test <- c(chi2_test, 'Fail to Reject Null Hypothesis')
  }
}
set.seed(40425764)
# Create a data frame to hold the results
test_result_df <- data.frame(
  "Categorical Features" = categorical_features,
  "Hypothesis Result" = chi2_test,
  stringsAsFactors = FALSE
)
set.seed(40425764)
# Print the results
cat(rep("-", 100), "\n", sep = "")
cat("Chi-Squared Test (Checking Multicollinearity for Categorical Features) results are as follows:\n")
cat(rep("-", 100), "\n", sep = "")

print(test_result_df)
#############################Insight and Visuluzation#####################
set.seed(40425764)
###Plotting the Selling Price Distribution with Histogram and Density Plot
ggplot(df, aes(x = selling_price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.4) +
  geom_density(color = "blue", size = 1) +
  ggtitle("Selling Price Distribution") +
  xlab("Selling price in millions") +
  ylab("Count") +
  xlim(0, 3000000) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10)
  )

# 1. Selling Price vs Age of Vehicle
ggplot(df, aes(x = fuel_type, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.size = 2, notch = TRUE) +
  ggtitle('Selling Price vs Fuel Type') +
  xlab('Fuel Type') +
  ylab('Selling Price (in currency)') +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
set.seed(40425764)

# 2. Selling Price vs Brand
ggplot(df, aes(x = reorder(brand, -selling_price), y = selling_price)) +
  geom_boxplot(fill = "cyan", alpha = 0.7) +
  coord_flip() +
  ggtitle('Selling Price vs Brand') +
  xlab('Brand') +
  ylab('Selling Price') +
  theme_minimal()

# 3. Selling Price vs Seller Type
ggplot(df, aes(x = seller_type, y = selling_price)) +
  geom_boxplot(fill = "magenta", alpha = 0.7) +
  ggtitle('Selling Price vs Seller Type') +
  xlab('Seller Type') +
  ylab('Selling Price') +
  theme_minimal()

# 4. Selling Price vs Fuel Type
ggplot(df, aes(x = fuel_type, y = selling_price)) +
  geom_boxplot(fill = "yellow", alpha = 0.7) +
  ggtitle('Selling Price vs Fuel Type') +
  xlab('Fuel Type') +
  ylab('Selling Price') +
  theme_minimal()

# 5. Selling Price vs Seats
ggplot(df, aes(x = factor(seats), y = selling_price)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  ggtitle('Selling Price vs Seats') +
  xlab('Seats') +
  ylab('Selling Price') +
  theme_minimal()
set.seed(40425764)
##6.Calculate the top 10 most sold cars
top_10_cars <- df %>%
  count(car_name, sort = TRUE) %>%
  top_n(10, n) %>%
  arrange(desc(n))
ggplot(top_10_cars, aes(x = reorder(car_name, -n), y = n, fill = car_name)) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  ggtitle("Top 10 Most Sold Cars") +
  xlab("Car Name") +
  ylab("Count") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#e0e0e0"),
    legend.position = "none"  # Remove legend since colors are self-explanatory
  ) +
  ylim(0, max(top_10_cars$n) + 500)  # Adjust the ylim to add space above the highest bar
set.seed(40425764)
## 7.Top Expensive Brands 
top_costly_brands <- df %>%
  group_by(brand) %>%
  summarise(average_price = mean(selling_price, na.rm = TRUE)) %>%
  top_n(10, average_price) %>%  # Select the top 10 costliest brands
  arrange(desc(average_price))

# Plot the top costliest brands with different colors for each bar
ggplot(top_costly_brands, aes(x = reorder(brand, -average_price), y = average_price, fill = brand)) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  ggtitle("Top 10 Costliest Car Brands") +
  xlab("Brand") +
  ylab("Average Selling Price (in millions)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#e0e0e0"),
    legend.position = "none"  # Remove legend since colors are self-explanatory
  ) +
  ylim(0, max(top_costly_brands$average_price) + 500000)  # Adjust the ylim to add space above the highest bar
set.seed(40425764)
##8.top 10 most expensive cars
library(ggplot2)
library(dplyr)
set.seed(40425764)
# Check the column names to ensure they are correct
print(colnames(df))

# Calculate the top 10 most expensive cars
top_expensive_cars <- df %>%
  arrange(desc(selling_price)) %>%  # Sort by selling price in descending order
  slice_max(order_by = selling_price, n = 10) %>%  # Select the top 10 most expensive cars
  dplyr::select(car_name, selling_price)  # Explicitly use dplyr::select to avoid conflicts

# Print the data to check the top 10 most expensive cars
print(top_expensive_cars)
set.seed(40425764)
# Plot the top expensive cars with different colors for each bar
ggplot(top_expensive_cars, aes(x = reorder(car_name, -selling_price), y = selling_price, fill = car_name)) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  ggtitle("Top 10 Most Expensive Cars") +
  xlab("Car Name") +
  ylab("Selling Price (in millions)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#e0e0e0"),
    legend.position = "none"  # Remove legend since colors are self-explanatory
  ) +
  ylim(0, max(top_expensive_cars$selling_price) * 1.1)  # Adjust the ylim to add space above the highest bar
set.seed(40425764)
##9.Milage vs Brands
brand_mileage <- df %>%
  group_by(brand) %>%
  summarise(average_mileage = mean(mileage, na.rm = TRUE)) %>%
  arrange(desc(average_mileage))

# Print the data to check the average mileage for each brand
print(brand_mileage)

# Plot the average mileage for each brand
ggplot(brand_mileage, aes(x = reorder(brand, -average_mileage), y = average_mileage, fill = brand)) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  ggtitle("Average Mileage by Brand") +
  xlab("Brand") +
  ylab("Average Mileage (in km/l)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#e0e0e0"),
    legend.position = "none"  # Remove legend since colors are self-explanatory
  ) +
  ylim(0, max(brand_mileage$average_mileage) * 1.1)  # Adjust the ylim to add space above the highest bar
##10.Kilometers Driven vs. Selling Price
library(ggplot2)
set.seed(40425764)
ggplot(df, aes(x = km_driven, y = selling_price, color = fuel_type)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_x_continuous(
    breaks = seq(0, 800000, by = 50000),  # Set custom breaks on the x-axis
    limits = c(-10000, 800000),  # Set x-axis limits
    labels = scales::comma  # Format the labels with commas for thousands
  ) +
  scale_y_continuous(
    limits = c(-10000, 10000000),  # Set y-axis limits
    labels = scales::comma  # Format the labels with commas for thousands
  ) +
  ggtitle("Kilometers Driven vs Selling Price") +
  xlab("Kilometers Driven") +
  ylab("Selling Price") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels for better readability
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#e0e0e0"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )
set.seed(40425764)
# 11.Fuel Type vs. Mileage
ggplot(df, aes(x = fuel_type, y = mileage, fill = fuel_type)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, notch = TRUE) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for the boxplots
  ggtitle("Fuel Type vs Mileage") +
  xlab("Fuel Type") +
  ylab("Mileage (km/l)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # Hide the legend as the fill colors match the x-axis categories
  )
##12 Mileage vs. Selling Price
ggplot(df, aes(x = mileage, y = selling_price)) +
  geom_point(alpha = 0.6, color = "blue", size = 2) +  # Scatter plot points with transparency
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add a linear regression line
  ggtitle("Mileage vs Selling Price") +
  xlab("Mileage (km/l)") +
  ylab("Selling Price") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#e0e0e0")
  )

##13Vehicle Age vs. Mileage

ggplot(df, aes(x = as.factor(vehicle_age), y = mileage, fill = as.factor(vehicle_age))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, notch = TRUE) +
  scale_fill_brewer(palette = "Set2") +  # Use a different color palette
  ggtitle("Mileage Distribution by Vehicle Age") +
  xlab("Vehicle Age (years)") +
  ylab("Mileage (km/l)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # Hide legend since colors match the x-axis categories
  )
#################Model Building ###################
###########################################################
# Initialize the results data frame to store the results of all models
results <- data.frame(Model = character(),
                      MSE = numeric(),
                      RMSE = numeric(),
                      MAE = numeric(),
                      R_Squared = numeric(),
                      stringsAsFactors = FALSE)

set.seed(40425764)
# Split the dataset into training and testing sets
trainIndex <- createDataPartition(df$selling_price, p = .8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

# Build the linear regression model
linear_model <- lm(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = train)

# Make predictions on the test set
predictions <- predict(linear_model, newdata = test)

# Calculate performance metrics for Linear Regression
mse <- mean((test$selling_price - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test$selling_price - predictions))
r_squared <- summary(linear_model)$r.squared
set.seed(40425764)
# Append Linear Regression results
results <- rbind(results, data.frame(
  Model = "Linear Regression",
  MSE = mse,
  RMSE = rmse,
  MAE = mae,
  R_Squared = r_squared
))
set.seed(40425764)
# Fit the Support Vector Regression model
library(e1071)  # For Support Vector Regression
svr_model <- svm(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = train)
svr_predictions <- predict(svr_model, test)

# Calculate evaluation metrics for SVR
mse_svr <- mean((test$selling_price - svr_predictions)^2)
rmse_svr <- sqrt(mse_svr)
mae_svr <- mean(abs(test$selling_price - svr_predictions))
r_squared_svr <- cor(test$selling_price, svr_predictions)^2
set.seed(40425764)
# Append SVR results
results <- rbind(results, data.frame(
  Model = "Support Vector Regression",
  MSE = mse_svr,
  RMSE = rmse_svr,
  MAE = mae_svr,
  R_Squared = r_squared_svr
))
set.seed(40425764)
# Fit the Decision Tree Regressor model
library(rpart)
dt_model <- rpart(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = train)
dt_predictions <- predict(dt_model, test)

# Calculate evaluation metrics for Decision Tree
mse_dt <- mean((test$selling_price - dt_predictions)^2)
rmse_dt <- sqrt(mse_dt)
mae_dt <- mean(abs(test$selling_price - dt_predictions))
r_squared_dt <- cor(test$selling_price, dt_predictions)^2
set.seed(40425764)
# Append Decision Tree results
results <- rbind(results, data.frame(
  Model = "Decision Tree Regressor",
  MSE = mse_dt,
  RMSE = rmse_dt,
  MAE = mae_dt,
  R_Squared = r_squared_dt
))
set.seed(40425764)
# Fit the Random Forest Regressor model
library(randomForest)
rf_model <- randomForest(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = train)
rf_predictions <- predict(rf_model, test)

# Calculate evaluation metrics for Random Forest
mse_rf <- mean((test$selling_price - rf_predictions)^2)
rmse_rf <- sqrt(mse_rf)
mae_rf <- mean(abs(test$selling_price - rf_predictions))
r_squared_rf <- cor(test$selling_price, rf_predictions)^2

# Append Random Forest results
results <- rbind(results, data.frame(
  Model = "Random Forest Regressor",
  MSE = mse_rf,
  RMSE = rmse_rf,
  MAE = mae_rf,
  R_Squared = r_squared_rf
))
set.seed(40425764)
## Check the column names of the existing results data frame
print(colnames(results))

# Ensure that all column names are standardized before appending
expected_colnames <- c("Model", "MSE", "RMSE", "MAE", "R_Squared")

# Set column names for the existing 'results' data frame to match
colnames(results) <- expected_colnames
set.seed(40425764)
# Check the column names of the existing results data frame
if (exists("results")) {
  print(colnames(results))
  expected_colnames <- colnames(results)
} else {
  # If 'results' does not exist yet, define the expected column names
  expected_colnames <- c("Model", "MSE", "RMSE", "MAE", "R_Squared")
}
set.seed(40425764)
# Ridge Regression
library(glmnet)
X_train_matrix <- model.matrix(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = train)[,-1]
X_test_matrix <- model.matrix(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = test)[,-1]
ridge_model <- cv.glmnet(X_train_matrix, train$selling_price, alpha = 0)
ridge_predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = X_test_matrix)

# Calculate evaluation metrics for Ridge Regression
mse_ridge <- mean((test$selling_price - ridge_predictions)^2)
rmse_ridge <- sqrt(mse_ridge)
mae_ridge <- mean(abs(test$selling_price - ridge_predictions))
r_squared_ridge <- cor(test$selling_price, ridge_predictions)^2
set.seed(40425764)
# Create a data frame for Ridge Regression results
ridge_result <- data.frame(
  Model = "Ridge Regression",
  MSE = mse_ridge,
  RMSE = rmse_ridge,
  MAE = mae_ridge,
  R_Squared = r_squared_ridge
)

# Align column names to match the existing results dataframe
colnames(ridge_result) <- expected_colnames
set.seed(40425764)
# Check if 'results' exists; if not, create it
if (!exists("results")) {
  results <- ridge_result
} else {
  # Append Ridge results to the results dataframe
  results <- rbind(results, ridge_result)
}

# Lasso Regression
lasso_cv <- cv.glmnet(X_train_matrix, train$selling_price, alpha = 1)
best_lambda <- lasso_cv$lambda.min
lasso_final_model <- glmnet(X_train_matrix, train$selling_price, alpha = 1, lambda = best_lambda)
lasso_predictions <- predict(lasso_final_model, s = best_lambda, newx = X_test_matrix)
set.seed(40425764)
# Calculate evaluation metrics for Lasso
mse_lasso <- mean((test$selling_price - lasso_predictions)^2)
rmse_lasso <- sqrt(mse_lasso)
mae_lasso <- mean(abs(test$selling_price - lasso_predictions))
r_squared_lasso <- cor(test$selling_price, lasso_predictions)^2

# Create a data frame for Lasso Regression results
lasso_result <- data.frame(
  Model = "Lasso Regression",
  MSE = mse_lasso,
  RMSE = rmse_lasso,
  MAE = mae_lasso,
  R_Squared = r_squared_lasso
)
set.seed(40425764)
# Align column names to match the existing results dataframe
colnames(lasso_result) <- expected_colnames

# Append Lasso results to the results dataframe
results <- rbind(results, lasso_result)
set.seed(40425764)
# Print the final results for all models
print(results)

#########################################################
# Load necessary libraries
library(caret)
library(e1071)  # For Support Vector Regression
library(rpart)  # For Decision Tree
library(randomForest)  # For Random Forest
library(glmnet)  # For Ridge and Lasso

# Set seed for reproducibility
set.seed(40425764)

# Define K-Fold Cross Validation parameters
train_control <- trainControl(method = "cv", number = 10)  # 10-Fold Cross Validation

# Prepare data matrices for Ridge and Lasso (they require model.matrix format)
X_matrix <- model.matrix(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, data = df)[, -1]
y <- df$selling_price

# 1. Linear Regression with K-Fold Cross Validation
linear_model <- train(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, 
                      data = df, 
                      method = "lm", 
                      trControl = train_control)

# Print the performance of Linear Regression
cat("Linear Regression with K-Fold Cross Validation:\n")
print(linear_model)

# 2. Support Vector Regression with K-Fold Cross Validation
svr_model <- train(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, 
                   data = df, 
                   method = "svmRadial", 
                   trControl = train_control)

# Print the performance of Support Vector Regression
cat("\nSupport Vector Regression with K-Fold Cross Validation:\n")
print(svr_model)

# 3. Decision Tree with K-Fold Cross Validation
dt_model <- train(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, 
                  data = df, 
                  method = "rpart", 
                  trControl = train_control)

# Print the performance of Decision Tree Regressor
cat("\nDecision Tree with K-Fold Cross Validation:\n")
print(dt_model)

# 4. Random Forest with K-Fold Cross Validation
rf_model <- train(selling_price ~ vehicle_age + km_driven + mileage + engine + max_power, 
                  data = df, 
                  method = "rf", 
                  trControl = train_control)

# Print the performance of Random Forest Regressor
cat("\nRandom Forest with K-Fold Cross Validation:\n")
print(rf_model)

# 5. Ridge Regression with K-Fold Cross Validation
ridge_model <- train(X_matrix, y, 
                     method = "glmnet", 
                     tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 10)), 
                     trControl = train_control)

# Print the performance of Ridge Regression
cat("\nRidge Regression with K-Fold Cross Validation:\n")
print(ridge_model)

# 6. Lasso Regression with K-Fold Cross Validation
lasso_model <- train(X_matrix, y, 
                     method = "glmnet", 
                     tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, length = 10)), 
                     trControl = train_control)

# Print the performance of Lasso Regression
cat("\nLasso Regression with K-Fold Cross Validation:\n")
print(lasso_model)

