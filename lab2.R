# open file manually because location wasn't found 
train_data <- read.csv(file.choose())

head(train_data)
str(train_data)

# Create a folder for yearly files
dir.create("/Users/ayeshahabib/Desktop/yearly_files", showWarnings = FALSE)

# Get unique years
years <- unique(train_data$YEAR4)

# Split dataset by year and save each as a CSV
for (yr in years) {
  yearly_data <- subset(train_data, YEAR4 == yr)
  write.csv(yearly_data,
            file = paste0("/Users/ayeshahabib/Desktop/yearly_files/train_accidents_", yr, ".csv"),
            row.names = FALSE)
}

# Check file sizes
files <- list.files("/Users/ayeshahabib/Desktop/yearly_files", full.names = TRUE)
file_info <- file.info(files)
file_info$Year <- gsub(".*_(\\d{4})\\.csv", "\\1", rownames(file_info))

file_info_sorted <- file_info[order(file_info$size), c("Year", "size")]
print(file_info_sorted)

# Oldest and newest year
oldest_year <- min(train_data$YEAR4, na.rm = TRUE)
newest_year <- max(train_data$YEAR4, na.rm = TRUE)
cat("Oldest year:", oldest_year, "\n")
cat("Newest year:", newest_year, "\n")

# stats of HIGHSPD for all accidents (number 11 in lab) !!!!!
total_accidents <- nrow(train_data)

# More than 10,000 accidents with HIGHSPD > 100
stmt1 <- sum(train_data$HIGHSPD > 100, na.rm = TRUE) > 10000

# More than half of accidents with HIGHSPD < 10
stmt2 <- mean(train_data$HIGHSPD < 10, na.rm = TRUE) > 0.5

# More than 10,000 accidents with HIGHSPD > 30
stmt3 <- sum(train_data$HIGHSPD > 30, na.rm = TRUE) > 10000

# More than half of accidents with HIGHSPD > 50
stmt4 <- mean(train_data$HIGHSPD > 50, na.rm = TRUE) > 0.5

cat("Statement 1 (HIGHSPD > 100):", stmt1, "\n")
cat("Statement 2 (HIGHSPD < 10):", stmt2, "\n")
cat("Statement 3 (HIGHSPD > 30):", stmt3, "\n")
cat("Statement 4 (HIGHSPD > 50):", stmt4, "\n")

# checking true statements regarding temp (lab number 12)
# Summary 
summary(train_data$TEMP)

# Check if more than half of accidents occurred at TEMP > 56
stmt1 <- median(train_data$TEMP, na.rm = TRUE) > 56

# Check if more than half of accidents occurred at TEMP > 59
stmt2 <- median(train_data$TEMP, na.rm = TRUE) > 59

# How many accidents had TEMP > 100
stmt3_count <- sum(train_data$TEMP > 100, na.rm = TRUE)
stmt3 <- stmt3_count > 500

# Check for implausible outliers (e.g., TEMP < -50 or TEMP > 150)
stmt4 <- sum(train_data$TEMP < -50 | train_data$TEMP > 150, na.rm = TRUE) >= 10

cat("Statement 1 (TEMP > 56):", stmt1, "\n")
cat("Statement 2 (TEMP > 59):", stmt2, "\n")
cat("Statement 3 (TEMP > 100):", stmt3, " (Count:", stmt3_count, ")\n")
cat("Statement 4 (Implausible outliers):", stmt4, "\n")

# generating box plot (EQPDMG) (number 13 lab)
train_2001_pos$YEAR4 <- as.factor(train_2001_pos$YEAR4)

# Create box plot with rotated x-axis labels (so it's more clear)
ggplot(train_2001_pos, aes(x = YEAR4, y = EQPDMG)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_y_continuous(trans = "log10") +
  labs(
    title = "Equipment Damage by Year (2001+)",
    x = "Year",
    y = "Equipment Damage (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# box plots (TOTKLD) for accidents for all years starting in 2001 (lab number 14)
library(ggplot2)

train_2001_killed <- subset(train_data, YEAR4 >= 2001 & TOTKLD > 0)

train_2001_killed$YEAR4 <- as.factor(train_2001_killed$YEAR4)

ggplot(train_2001_killed, aes(x = YEAR4, y = TOTKLD)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_y_continuous(trans = "log10") +  # log scale for extreme values
  labs(
    title = "Total Killed by Year (2001+)",
    x = "Year",
    y = "Total Killed (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# box plots (TRKDMG) for all years starting in 2001 (lab number 15)
library(ggplot2)

train_2001_trk <- subset(train_data, YEAR4 >= 2001 & TRKDMG > 0)

train_2001_trk$YEAR4 <- as.factor(train_2001_trk$YEAR4)

ggplot(train_2001_trk, aes(x = YEAR4, y = TRKDMG)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_y_continuous(trans = "log10") +  # log scale for extreme values
  labs(
    title = "Track Damage by Year (2001+)",
    x = "Year",
    y = "Track Damage (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# box plots (TOTINJ) for accidents for all years from 2001 (lab number 16)
library(ggplot2)

train_2001_inj <- subset(train_data, YEAR4 >= 2001 & TOTINJ > 0)

train_2001_inj$YEAR4 <- as.factor(train_2001_inj$YEAR4)

ggplot(train_2001_inj, aes(x = YEAR4, y = TOTINJ)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_y_continuous(trans = "log10") +  # log scale for extreme values
  labs(
    title = "Total Injuries by Year (2001+)",
    x = "Year",
    y = "Total Injuries (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# box plots of CARSDMG for all years starting in 2001 (lab number 17)
library(ggplot2)

train_2001_cars <- subset(train_data, YEAR4 >= 2001 & CARSDMG > 0)

train_2001_cars$YEAR4 <- as.factor(train_2001_cars$YEAR4)

ggplot(train_2001_cars, aes(x = YEAR4, y = CARSDMG)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_y_continuous(trans = "log10") +  # log scale for extreme values
  labs(
    title = "Cars Damaged by Year (2001+)",
    x = "Year",
    y = "Cars Damaged (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# row(s) with the maximum TOTINJ, (lab number 18)
max_injuries <- max(train_data$TOTINJ, na.rm = TRUE)

incident_max_injuries <- train_data$INCDTNO[train_data$TOTINJ == max_injuries]

cat("Incident number(s) with the most injuries:", incident_max_injuries, "\n")
cat("Maximum number of injuries:", max_injuries, "\n")

# (lab number 19)
install.packages("tidyr")
library(ggplot2)
library(tidyr)

train_2021 <- subset(train_data, YEAR4 >= 2021)

vars <- train_2021[, c("TONS", "TOTINJ", "TEMP")]

vars_long <- pivot_longer(vars, cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(vars_long, aes(sample = Value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "QQ Plots of TONS, TOTINJ, and TEMP (2021+)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# (lab number 20)
library(ggplot2)

train_2001 <- subset(train_data, YEAR4 >= 2001)

ggplot(train_2001, aes(x = TEMP)) +
  geom_histogram(breaks = pretty(train_2001$TEMP, n = nclass.Sturges(train_2001$TEMP)),
                 fill = NA, colour = 'steelblue') +
  labs(title = "Histogram of TEMP (Accidents 2001+)",
       x = "Temperature",
       y = "Count") +
  theme_minimal()

# (lab number 21)
if(!require(psych)) install.packages("psych", dependencies = TRUE)
library(psych)

accidents_2001 <- subset(train_data, YEAR4 >= 2001)

vars_of_interest <- accidents_2001[, c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]

pairs.panels(
  vars_of_interest,
  method = "pearson",   # correlation method
  hist.col = "lightblue", 
  density = TRUE,       # show density plots
  ellipses = FALSE      # no ellipses
)

# getting the answer numerically to double check
accidents_2001 <- subset(train_data, YEAR4 >= 2001)

cor_trk_acc <- cor(accidents_2001$TRKDMG, accidents_2001$ACCDMG, use = "complete.obs")

cor_trk_acc_rounded <- signif(cor_trk_acc, 2)

cat("Correlation between TRKDMG and ACCDMG:", cor_trk_acc_rounded, "\n")

# numerical double check for lab number 22
train_2001 <- subset(train_data, YEAR4 >= 2001)

fit1 <- lm(TOTKLD ~ TOTINJ, data = train_2001)
summary(fit1)$coefficients

fit2 <- lm(ACCDMG ~ TRKDMG, data = train_2001)
summary(fit2)$coefficients

fit3 <- lm(TOTKLD ~ TRKDMG, data = train_2001)
summary(fit3)$coefficients

fit4 <- lm(ACCDMG ~ EQPDMG, data = train_2001)
summary(fit4)$coefficients

# double check numerically for lab number 23
train_2001 <- subset(train_data, YEAR4 >= 2001)

count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  outliers <- sum(x < (Q1 - 1.5*IQR_val) | x > (Q3 + 1.5*IQR_val), na.rm = TRUE)
  return(outliers)
}

count_outliers(train_2001$TOTINJ)
count_outliers(train_2001$TOTKLD)

max(train_2001$TOTINJ, na.rm = TRUE)
max(train_2001$TOTKLD, na.rm = TRUE)

# year of accident with the largest value of ACCDMG, 4 digits (lab number 24)
max_row <- train_data[which.max(train_data$ACCDMG), ]

max_year <- max_row$YEAR4
max_year

# lab number 25 
max_acc_row <- train_data[which.max(train_data$ACCDMG), ]

statements <- list(
  `Two different railroads` = length(unique(max_acc_row$RAILROAD)) > 1,
  `Type derailment` = max_acc_row$TYPE == 1,   # assuming 1 = derailment
  `Same as accident with most deaths` = max_acc_row$INCDTNO %in% train_data$INCDTNO[which.max(train_data$TOTKLD)],
  `Type Hwy-Rail` = max_acc_row$TYPE == 2     # replace 2 with actual code
)

statements

# lab number 26
max_accdmg <- max(train_data$ACCDMG, na.rm = TRUE)
max_accdmg

# lab number 27 - max number of deaths in one accident
max_deaths <- max(train_data$TOTKLD, na.rm = TRUE)
max_deaths

# lab number 28 - row of accident with maximum deaths
max_deaths_row <- train_data[which.max(train_data$TOTKLD), ]
max_deaths_year <- max_deaths_row$YEAR4
max_deaths_year

# lab number 29 - accidents with ACCDMG > 1.5 mil
num_accidents <- sum(train_data$ACCDMG > 1500000, na.rm = TRUE)
num_accidents

# lab number 30 - accidents w at least one death
num_fatal_accidents <- sum(train_data$TOTKLD >= 1, na.rm = TRUE)
num_fatal_accidents

# lab number 31 - Barplot of accident types
accident_counts <- table(train_data$TYPE)  # counts of each TYPE
barplot(accident_counts, 
        col = "steelblue", 
        main = "Frequency of Each Accident Type",
        xlab = "Accident Type",
        ylab = "Frequency")

accident_counts

# lab number 32 
cause_counts <- table(train_data$ACCAUSE)  # or train_data$CAUSE if that’s the main column

barplot(cause_counts,
        col = "steelblue",
        main = "Frequency of Major Causes of Train Accidents",
        xlab = "Cause",
        ylab = "Frequency",
        las = 2)  

cause_counts

# lab number 33
library(ggplot2)

ggplot(train_data, aes(x = "", y = ACCDMG)) +
  geom_boxplot(outlier.colour = "red", fill = "lightblue") +
  labs(title = "Boxplot of Total Accident Damage Across All Years",
       x = "",
       y = "Total Accident Damage ($)") +
  theme_minimal()

box_stats <- boxplot.stats(train_data$ACCDMG)$stats
upper_whisker <- box_stats[5]  
upper_whisker

# lab number 34 - accidents with ACCDMG greater than upper whisker (95,700)
train_2001_2023 <- subset(train_data, YEAR4 >= 2001 & YEAR4 <= 2023)

num_above_whisker <- sum(train_2001_2023$ACCDMG > 95700, na.rm = TRUE)
num_above_whisker

# lab number 35
ACCDMG <- train_data$ACCDMG
ACCDMG <- ACCDMG[!is.na(ACCDMG)]

Q1 <- quantile(ACCDMG, 0.25)
Q3 <- quantile(ACCDMG, 0.75)
IQR_val <- Q3 - Q1

extremes <- ACCDMG < (Q1 - 1.5*IQR_val) | ACCDMG > (Q3 + 1.5*IQR_val)

prop_extreme <- round(sum(extremes) / length(ACCDMG) * 100)
prop_extreme

# lab number 36 
extreme_total <- sum(train_data$ACCDMG[train_data$ACCDMG > upper_whisker], na.rm = TRUE)

all_total <- sum(train_data$ACCDMG, na.rm = TRUE)

prop_cost <- round((extreme_total / all_total) * 100)
prop_cost

# lab number 37 - accidents costing more than $15 million !!
  statements <- list(
    `WTC Sept 11 2001` = any(extreme_accidents$YEAR4 == 2001 & 
                               extreme_accidents$MONTH == 9 & 
                               extreme_accidents$DAY == 11),
    
    `Accident type Explosive` = any(extreme_accidents$TYPE == 10),  # replace 10 with the code for Explosive if different
    
    `Most expensive > $30M` = any(extreme_accidents$ACCDMG > 30000000),
    
    `More than 20 accidents` = nrow(extreme_accidents) > 20,
    
    `Only 10 accidents` = nrow(extreme_accidents) == 10
  )

statements

# lab number 38 - duplicates based on selected columns
extreme_accidents <- subset(train_data, ACCDMG > upper_whisker)

duplicate_rows <- extreme_accidents[duplicated(extreme_accidents[, c("INCDTNO", "YEAR4", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]), ]

num_duplicates <- nrow(duplicate_rows)
num_duplicates




