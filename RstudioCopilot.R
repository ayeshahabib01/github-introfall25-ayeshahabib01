#The goal of this assignment Verify you can use Microsoft Copilot / GitHub Copilot Chat alongside RStudio 
#to accelerate—but not replace—your coding.

# ------------------------------
# 1) Paths & file I/O
# ------------------------------

# 1.1 Set working directories -------------------------------------------------
# EDIT THESE for your environment
traindir <- "/Users/ayeshahabib/Desktop/SYS3501/data"
sourcedir <- "/Users/ayeshahabib/Desktop/SYS3501/source"

stopifnot(dir.exists(traindir))
setwd(traindir)
message("Working directory set to: ", getwd())

# 1.3 Safe CSV read helper ----------------------------------------------------
safe_read <- function(file) {
  # Matches read.csv defaults from class, but robust NA handling
  read.csv(file, na.strings = c("", "NA", "N/A"," NA", "NA "), stringsAsFactors = FALSE)
}

# 1.4 Read the accident CSV ------------------------------------------------
totacts <- safe_read("Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv")
# ------------------------------

#prompt 1
totacts2124 <- subset(accidents, year >= 2021 & year <= 2024)

# Did it work as intended? No
# Problem: Copilot assumed the dataframe was named 'accidents', but in my script it is 'totacts'.
# It also assumed a column named 'year', but the dataset uses 'IYR' for the year.
# Corrected version
totacts2124 <- subset(totacts, IYR >= 21 & IYR <= 24)

#prompt 2
totacts2124$CASINJ <- totacts2124$TOTINJ + totacts2124$TOTKLD
head(totacts2124)
# Did it work as intended? Yes
# The code correctly created a new CASINJ column by summing TOTINJ and TOTKLD.

#prompt 3
totacts2124$YEAR <- ifelse(totacts2124$IYR >= 0 & totacts2124$IYR <= 99, 
                           2000 + totacts2124$IYR, NA)
head(totacts2124$YEAR)

# Did it work as intended? Yes
# Copilot’s code correctly created the YEAR column by converting IYR to four-digit years.

#prompt 4
dim(totacts2124)

# Did it work as intended? Yes
# The code correctly returned the dimensions of totacts2124.



