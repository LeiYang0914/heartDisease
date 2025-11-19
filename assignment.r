# ===============================
# 1. Load Libraries
# ===============================
library(tidyverse)
library(forcats)

# ===============================
# 2. Load Dataset
# ===============================
# Change to your actual file path!
df <- read.csv("C:/Users/User/OneDrive/Documents/Master of Data Science/Sem1/Programming for data science/heart_disease_health_indicators_BRFSS2015.csv")

# ===============================
# 3. Basic Dataset Information
# ===============================

# View first few rows
head(df)

# Dimensions (rows & columns)
cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")

# Column names
colnames(df)

# Dataset structure (types of each feature)
str(df)

# Summary statistics (numeric + categorical)
summary(df)

# ===============================
# 4. Missing Value Check
# ===============================

# Count missing values per column
colSums(is.na(df))

# Count total missing values
sum(is.na(df))

# ===============================
# 5. Duplicate Row Check
# ===============================
cat("Number of duplicate rows:", sum(duplicated(df)), "\n")

# ===============================
# 6. Identify Numeric & Categorical Variables
# ===============================
numeric_cols <- df %>% select(where(is.numeric)) %>% colnames()
categorical_cols <- df %>% select(where(is.factor)) %>% colnames()

cat("Numeric Columns:\n")
print(numeric_cols)

cat("\nCategorical Columns:\n")
print(categorical_cols)

# ===============================
# 7. Distribution of Key Variables
# ===============================

# Heart Disease count
table(df$HeartDiseaseorAttack)

# Convert to proportion
prop.table(table(df$HeartDiseaseorAttack))

# BMI basic statistics
summary(df$BMI)

# Plot quick histogram (optional)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(df$BMI, main="BMI Distribution", xlab="BMI", col="skyblue", border="white")

# ===============================
# 7. Data preprocessing
# ===============================
# Keep a clean working copy
df_clean <- df

## 8.1 Check and handle missing value
total_na <- sum(is.na(df_clean))
cat("Total missing values before cleaning:", total_na, "\n")

## 8.2 Remove duplicate rows
dup_n <- sum(duplicated(df_clean))
cat("Number of duplicate rows :", dup_n, "\n")

df_clean <- df_clean %>% distinct()

cat("Rows after removing duplicates:", nrow(df_clean), "\n")

## 8.2 Handle outliers for BMI (winsorize at 1% & 99%) 
summary(df_clean$BMI)

bmi_q <- quantile(df_clean$BMI, probs = c(0.01, 0.99), na.rm = TRUE)
bmi_q

df_clean <- df_clean %>%
  mutate(
    BMI_capped = pmin(pmax(BMI, bmi_q[1]), bmi_q[2])
  )

summary(df_clean$BMI_capped)

# ------------------------------------------------------
# 8.3 Recode GenHlth (1–5 → Excellent → Poor)
# ------------------------------------------------------

df_clean <- df_clean %>%
  mutate(
    GenHlth_Factor = factor(GenHlth,
                            levels = c(1,2,3,4,5),
                            labels = c("Excellent","Very Good","Good","Fair","Poor"),
                            ordered = TRUE
    )
  )

# ------------------------------------------------------
# 8.4 Validate MentHlth & PhysHlth ranges (0–30)
# ------------------------------------------------------
# Count invalid values
invalid_ment <- sum(df_clean$MentHlth > 30, na.rm = TRUE)
invalid_phys <- sum(df_clean$PhysHlth > 30, na.rm = TRUE)

cat("Number of rows with MentHlth > 30:", invalid_ment, "\n")
cat("Number of rows with PhysHlth > 30:", invalid_phys, "\n")

# Total invalid rows (some rows may overlap)
invalid_total <- sum((df_clean$MentHlth > 30) | (df_clean$PhysHlth > 30), na.rm = TRUE)
cat("Total rows to be removed due to invalid health day values:", invalid_total, "\n")

# Remove invalid rows
df_clean <- df_clean %>%
  filter(MentHlth <= 30, PhysHlth <= 30)

# ------------------------------------------------------
## 8.5 Feature engineering
# ------------------------------------------------------
df_clean <- df_clean %>%
  mutate(
    # BMI categorical group
    BMI_Category = cut(
      BMI,
      breaks = c(-Inf, 18.5, 25, 30, Inf),
      labels = c("Underweight", "Normal", "Overweight", "Obese")
    ),
    BMI_Category = as.factor(BMI_Category)
  )

df_clean %>% 
  select(BMI, BMI_Category) %>% 
  head(10)

table(df_clean$PhysHlth)


