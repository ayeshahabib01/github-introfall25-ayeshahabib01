housing <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Inspect the data to confirm it loaded correctly
head(housing)        # View first few rows
str(housing)         # Check data types and structure
summary(housing)     # Get summary statistics

# 21
# Boxplot of housing prices
boxplot(housing$price,
        main = "Boxplot of Housing Prices",
        ylab = "Price ($)",
        col = "lightblue",
        border = "darkblue")

# Identify and count outliers
outliers <- boxplot.stats(housing$price)$out
num_outliers <- length(outliers)

# Display results
num_outliers    # Number of outliers
outliers        # Outlier values

# 22
boxplot(housing$price,
        main = "Boxplot of Housing Prices",
        ylab = "Price ($)",
        col = "lightblue",
        border = "darkblue")
# The upper whisker value can be found in the boxplot statistics
upper_whisker <- boxplot.stats(housing$price)$stats[5]
upper_whisker  # Display the upper whisker value

# 23 + 24
# Scatter plot matrix for numeric variables in the dataset
pairs(~ price + bedrooms + baths + sqft, 
      data = housing,
      main = "Scatter Plot Matrix of Housing Variables",
      col = "steelblue",
      pch = 19)
# Compute correlations between price and numeric predictors
cor(housing[, c("price", "bedrooms", "baths", "sqft")], use = "complete.obs")

# 25
# checking to see if any variables are left skewed 
install.packages("moments")

library(moments)

skew_values <- sapply(housing[, c("price", "bedrooms", "baths", "sqft")], skewness, na.rm = TRUE)
skew_values

# 26
# Boxplot of bedrooms
boxplot(housing$bedrooms,
        main = "Boxplot of Bedrooms",
        ylab = "Number of Bedrooms",
        col = "lightblue",
        border = "darkblue")

# Get summary statistics for bedrooms
bed_stats <- boxplot.stats(housing$bedrooms)
bed_stats
# to check if fewer than 5 houses have 6 bedrooms 
sum(housing$bedrooms == 6)

# 27 
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

max_price <- max(housing$price, na.rm = TRUE)

highest_price_houses <- housing %>%
  filter(price == max_price)

print(highest_price_houses)

# 3. Check statements

# Statement 1: One of the observation numbers is 53
obs_numbers <- which(housing$price == max_price)
statement1 <- 53 %in% obs_numbers

# Statement 2: These homes have more than 1 bathroom
statement2 <- all(highest_price_houses$baths > 1)

# Statement 3: The square footage is greater than 5000
statement3 <- all(highest_price_houses$sqft > 5000)

# Statement 4: There are 3 houses in 3 different locations with the highest price
statement4 <- length(unique(highest_price_houses$City)) == 3

# Output results
cat("Statement 1 (Obs number 53):", statement1, "\n")
cat("Statement 2 (Bathrooms > 1):", statement2, "\n")
cat("Statement 3 (Sqft > 5000):", statement3, "\n")
cat("Statement 4 (3 houses in different cities):", statement4, "\n")

# 28 
oxnard_total_price <- sum(housing$price[housing$City == "Oxnard"], na.rm = TRUE)
oxnard_total_price

# 29
city_totals <- housing %>%
  group_by(City) %>%
  summarise(total_price = sum(price, na.rm = TRUE)) %>%
  arrange(desc(total_price))

# Show results
city_totals

# City with the highest total price
city_totals$City[1]

# 30
# How many levels are there for the City variable?
num_cities <- length(unique(housing$City))
num_cities

# 31 
# Load dataset manually
pima <- read.csv(file.choose(), stringsAsFactors = FALSE)
# Check count of zeros for each variable
sapply(pima, function(x) sum(x == 0, na.rm = TRUE))

# 32
total_missing <- sum(is.na(pima))
total_missing

# 33
# The total percentage of missing data rounded to the nearest % is: 
total_values <- prod(dim(pima))
missing_percentage <- round((total_missing / total_values) * 100)
missing_percentage

# 34
# the most frequently missing variable
missing_counts <- sapply(pima, function(x) sum(is.na(x)))
most_missing_var <- names(which.max(missing_counts))
most_missing_var
most_missing_count <- max(missing_counts)
most_missing_count

# 35
# second most frequently missing variable
sorted_missing <- sort(missing_counts, decreasing = TRUE)
second_most_missing_var <- names(sorted_missing)[2]
second_most_missing_var
second_most_missing_count <- sorted_missing[2]
second_most_missing_count

# 36 
# If we were to remove observations with missing variables, we would still have how observations remaining?
pima_complete <- na.omit(pima)
num_complete_obs <- nrow(pima_complete)
num_complete_obs




  



