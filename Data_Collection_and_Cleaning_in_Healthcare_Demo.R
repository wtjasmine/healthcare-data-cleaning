# ====================================
# Handling Missing Data (Basics)
# ====================================

# Install and load the mlbench and dplyr packages 
install.packages("mlbench")
install.packages("dplyr")
library(mlbench) # contains the built-in PimaIndiansDiabetes2 dataset
library(dplyr) # for data wrangling (filtering, mutating, summarizing)

# Load dataset
data("PimaIndiansDiabetes2") # Load PimaIndianDiabetes2
demo_data <- head(PimaIndiansDiabetes2, 10) # Take first 10 rows for demonstration
demo_data # print demo_data to view

# Check missing values per column
colSums(is.na(demo_data))

# Drop rows with 3 or more missing values (This threshold is just for demonstration — in practice, the cut-off depends on dataset size, missingness pattern, and study context) 
drop_demo <- demo_data %>%
  filter(rowSums(is.na(.)) < 3)
drop_demo # print drop_demo 

# Impute triceps using median
impute_demo <- drop_demo %>%  
  mutate(triceps = ifelse(is.na(triceps), 
                          median(triceps, na.rm = TRUE), 
                          triceps))
impute_demo # print impute_demo 

# Insulin column is left with NA values, this keeps the clinical meaning of "not measured"
keep_demo <- impute_demo
keep_demo # print keep_demo


# ====================================
# Handling Inconsistencies
# ====================================

# Load dataset (replace path with your file)
df <- read.csv("realworld_medical_dirty.csv")

# Show the first 6 rows only (preview the dataset)
head(df)

# Check unique values in Smoker column (before cleaning)
unique(df$Smoker)

# Replace short forms with full words
df$Smoker[df$Smoker %in% c("Y", "Yes")] <- "Yes"
df$Smoker[df$Smoker %in% c("N", "No")]  <- "No"

# Check unique values after cleaning
unique(df$Smoker)

# Quick frequency table
table(df$Smoker)


# ====================================
# Handling Duplicate Records
# ====================================

# Load from GitHub mirror (same as Kaggle dataset)
url <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv"

# Read the CSV file directly from the URL into a dataframe called df
df <- read.csv(url)

# Show the first 6 rows only (preview the dataset)
head(df)

# Count how many duplicate rows
sum(duplicated(df))

# View duplicate rows 
df[duplicated(df), ]

# Keep only unique rows
library(dplyr) # load dplyr for data manipulation
df_clean <- df %>% distinct()

# Confirm duplicates removed (should return 0 if none exist) 
sum(duplicated(df_clean))

# Compare before vs after
nrow(df)       # total rows before cleaning
nrow(df_clean) # total rows after removing duplicates

# ====================================
# Detecting Outliers
# ====================================
# Detect outliers using IQR
Q1 <- quantile(df$bmi, 0.25, na.rm = TRUE)
Q3 <- quantile(df$bmi, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outliers <- df$bmi[df$bmi < (Q1 - 1.5*IQR) | df$bmi > (Q3 + 1.5*IQR)]
outliers

# Boxplot to visualize
boxplot(df$bmi, main = "Boxplot of BMI", ylab = "BMI")

