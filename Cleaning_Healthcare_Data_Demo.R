# Load required packages
library(readxl)
library(dplyr)
library(tools)
library(lubridate)

# Load data from GitHub
url <- "https://raw.githubusercontent.com/wtjasmine/healthcare-data-cleaning/main/uncleaned_vaccine_data_demo.csv"
df <- read.csv(url)
print(df)


# 2. Standardize patient names (title case, merge duplicates)
df$Patient_Name <- tools::toTitleCase(tolower(df$Patient_Name))

# 3. Vaccine type: lower → title case
df$Vaccine_Type <- tools::toTitleCase(tolower(df$Vaccine_Type))

# 4. Dates: standardize to YYYY-MM-DD
df <- df %>%
  mutate(
    Dose_Date = case_when(
      grepl("^[0-9]+$", Dose_Date) ~ as.character(as.Date(as.numeric(Dose_Date), origin = "1899-12-30")),
      TRUE ~ as.character(Dose_Date)   # force to character
    ),
    Dose_Date = as.Date(parse_date_time(Dose_Date, 
                                        orders = c("d/m/Y","m/d/Y","d-b-Y","m-d-Y",
                                                   "d.m.Y","Y.m.d","Ymd",
                                                   "d/m/y","Y/m/d H:M:S","Y-m-d")))
  )



# 5. Temperature → convert everything to °C
df$Temperature <- gsub(" ", "", tolower(df$Temperature))
df$Temp_C <- ifelse(grepl("f", tolower(df$Temperature)),
                    (as.numeric(gsub("[^0-9\\.\\-]", "", df$Temperature)) - 32) * 5/9,
                    ifelse(grepl("c", tolower(df$Temperature)),
                           as.numeric(gsub("[^0-9\\.\\-]", "", df$Temperature)),
                           as.numeric(gsub("[^0-9\\.\\-]", "", df$Temperature))))


# 6. Age: handle outliers (>110) and missing
df$Age[df$Age > 110] <- NA
df$Age[is.na(df$Age)] <- round(mean(df$Age, na.rm=TRUE))  # impute with mean

# 7. Covid test: standardize labels
df$Covid_Test <- tolower(df$Covid_Test)
df$Covid_Test[df$Covid_Test %in% c("+","pos","positive")] <- "positive"
df$Covid_Test[df$Covid_Test %in% c("neg","-","negative")] <- "negative"


# 8. Remove duplicates (same patient, same date, same vaccine)
df_clean <- df %>%
  distinct(Patient_Name, Vaccine_Type, Dose_Date, .keep_all = TRUE)


# Final cleaned dataset
print(df_clean)


