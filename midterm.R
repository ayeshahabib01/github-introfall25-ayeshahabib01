housing_prices <- read.csv(file.choose(), stringsAsFactors = FALSE)

head(housing_prices)        
str(housing_prices)         
summary(housing_prices) 

# house with lowest price is 21000, what is the index of that value 
which(housing_prices$Price == min(housing_prices$Price))
housing_prices[which(housing_prices$Price == min(housing_prices$Price)), ]

# give index 
min_price_index <- which(housing_prices$Price == min(housing_prices$Price))
min_price_index



# What is the square footage of the most expensive house?
which(housing_prices$Price == max(housing_prices$Price))
housing_prices[which(housing_prices$Price == max(housing_prices$Price)), "size"]

# make a box plot for the  "Size" variable 
boxplot(housing_prices$Size, main = "Boxplot of House Sizes", ylab = "Size (sq ft)")

# how many outliers does it have
boxplot_stats <- boxplot.stats(housing_prices$Size)
outliers <- boxplot_stats$out
length(outliers)

# What is the maximum value of the outliers in Size?
max(outliers)

# Create a new data frame with the outliers from the Size variable
outlier_houses <- housing_prices[housing_prices$Size %in% outliers, ]
outlier_houses

# What is the mean of Size of these outlier houses (rounded to the nearest integer)?
mean(outlier_houses$Size)
round(mean(outlier_houses$Size))

# what is The maximum price of the outlier houses 
max(outlier_houses$Price)

# determine if All of these outlier houses are New.
all(outlier_houses$New == "Yes")

# what is the average price of the houses identified as outliers in the Size variable 
mean(outlier_houses$Price)
mean(outlier_houses$Price)
round(mean(outlier_houses$Price))

# 7 make a box plot of Bathrooms
boxplot(housing_prices$Bath, main = "Boxplot of Number of Bathrooms", ylab = "Number of Bathrooms")
boxplot_stats_bath <- boxplot.stats(housing_prices$Bath)
outliers_bath <- boxplot_stats_bath$out
length(outliers_bath)
outliers_bath
max(outliers_bath)
outlier_houses_bath <- housing_prices[housing_prices$Bath %in% outliers_bath, ]
outlier_houses_bath
mean(outlier_houses_bath$Bath)
round(mean(outlier_houses_bath$Bath))
max(outlier_houses_bath$Price)
all(outlier_houses_bath$New == "Yes")
mean(outlier_houses_bath$Price)
round(mean(outlier_houses_bath$Price))

# lower whisker
boxplot_stats_bath$stats[1]

# upper whisker
boxplot_stats_bath$stats[5]

# how many houses have 3 or more bathrooms
sum(housing_prices$Bath >= 3)

# 14 is not few 

# what is The median
median(housing_prices$Bath)

# lower quartile
boxplot_stats_bath$stats[2]

# upper quartile
boxplot_stats_bath$stats[4]

# whiskers 
boxplot_stats_bath$stats[c(1, 5)]


# using only numerical variables, make a scatter plot matrix to determine the variable in the data set that has the strongest linear relationship with Size
numeric_vars <- housing_prices[sapply(housing_prices, is.numeric)]
pairs(numeric_vars, main = "Scatter Plot Matrix of Numerical Variables")
cor(numeric_vars)
cor(numeric_vars$Size, numeric_vars$Price)

# make a histogram of Size
hist(housing_prices$Size, main = "Histogram of House Sizes", xlab = "Size (sq ft)", breaks = 20)

# find sum of all Old homes 
sum(old_homes$Price)
sum(old_homes$Price) / 1000000
round(sum(old_homes$Price) / 1000000, 2)

# use as.factor to convert Rooms and Baths to a categorical variable, then Make a frequency heat map of Baths and Rooms.
bath_room_freq <- housing_prices %>%
  group_by(Baths = as.factor(Baths), Rooms = as.factor(Rooms)) %>%
  summarise(Frequency = n())
ggplot(bath_room_freq, aes(x = Baths, y = Rooms, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Frequency Heat Map of Bathrooms and Rooms", x = "Number of Bathrooms", y = "Number of Rooms") +
  theme_minimal()

# 12 
# Install It
install.packages("VIM")
# Load the VIM library
library(VIM)

# Load the airquality dataset
data("airquality")

# "airquality" loaded into your working environment. Display the first few rows of the dataset
head(airquality)

# Which of the variables are 100% complete?
colSums(is.na(airquality))
complete_vars <- colnames(airquality)[colSums(is.na(airquality)) == 0]
complete_vars

# Which month has the highest average temperature across all days?
library(dplyr)
avg_temp_by_month <- airquality %>%
  group_by(Month) %>%
  summarise(Average_Temp = mean(Temp, na.rm = TRUE))
avg_temp_by_month
max_avg_temp_month <- avg_temp_by_month[which.max(avg_temp_by_month$Average_Temp), ]
max_avg_temp_month
max_avg_temp_month$Month

# Create a histogram for Solar.R in the airquality dataset.
hist(airquality$Solar.R, main = "Histogram of Solar Radiation", xlab = "Solar Radiation (Solar.R)", breaks = 20)
hist(airquality$Solar.R, main = "Histogram of Solar Radiation", xlab = "Solar Radiation (Solar.R)", breaks = 20, col = "lightblue", border = "black")

# slightly skewed left, there is majority of data on the right side, especially near 250 

# maximum observed Solar Radiation value
max(airquality$Solar.R, na.rm = TRUE)

# check if it follows normal distribution
shapiro_test <- shapiro.test(airquality$Solar.R[!is.na(airquality$Solar.R)])
shapiro_test
shapiro_test$p.value

# conclude if it does or not
if (shapiro_test$p.value > 0.05) {
  conclusion <- "The Solar.R variable follows a normal distribution."
} else {
  conclusion <- "The Solar.R variable does not follow a normal distribution."
}
conclusion

# count how many Solar Radiation values are below or equal to 100
sum(airquality$Solar.R <= 100, na.rm = TRUE)

# 17 Draw scatterplot matrix for "Ozone", "Solar.R", "Wind", "Temp" in the airquality dataset. 
selected_vars <- airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]
pairs(selected_vars, main = "Scatter Plot Matrix of Selected Air Quality Variables")
cor(selected_vars, use = "complete.obs")
cor(selected_vars, use = "complete.obs")["Ozone", ]
cor(selected_vars$Ozone, selected_vars$Temp, use = "complete.obs")
cor(selected_vars$Ozone, selected_vars$Temp, use = "complete.obs")
cor(selected_vars$Ozone, selected_vars$Temp, use = "complete.obs")^2
round(cor(selected_vars$Ozone, selected_vars$Temp, use = "complete.obs")^2, 2)

# display all correlation values 
cor(selected_vars, use = "complete.obs")

# Create box plots for "Wind" by "Month" to visualize the distribution of Wind across different Months. 
airquality$Month <- as.factor(airquality$Month)
boxplot(Wind ~ Month, data = airquality, main = "Boxplot of Wind by Month", xlab = "Month", ylab = "Wind (mph)", col = "lightgreen", border = "black")
boxplot_stats_wind <- boxplot.stats(airquality$Wind)
outliers_wind <- boxplot_stats_wind$out
length(outliers_wind)
outliers_wind
max(outliers_wind)

# which month has outliers for wind 
outlier_houses_wind <- airquality[airquality$Wind %in% outliers_wind, ]
outlier_houses_wind









  



