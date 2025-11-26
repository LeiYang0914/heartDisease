# ===============================
# 1. Load Libraries
# ===============================
library(tidyverse)
library(forcats)

# ===============================
# 2. Load Dataset
# ===============================
# Change to your actual file path!
df <- read.csv("heart_disease_missing_simulated.csv")

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
# BEFORE dropping
cat("Total rows before dropping missing values:", nrow(df_clean), "\n")

# Print missing values per column
cat("\nMissing values per column:\n")
print(colSums(is.na(df_clean)))

# Count rows that contain at least one missing value
rows_with_na <- sum(!complete.cases(df_clean))
cat("\nNumber of rows containing at least one missing value:", rows_with_na, "\n")

# Drop rows with ANY missing value
df_clean <- df_clean %>% drop_na()

# AFTER dropping
cat("\nTotal rows after dropping missing values:", nrow(df_clean), "\n")

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

# ------------------------------------------------------
# 8.6 Convert variables to appropriate types (factors)
# ------------------------------------------------------

df_clean <- df_clean %>%
  mutate(
    # Binary yes/no variables → factors
    across(
      c(HeartDiseaseorAttack, HighBP, HighChol, CholCheck,
        Smoker, Stroke, PhysActivity, Fruits, Veggies,
        HvyAlcoholConsump, AnyHealthcare, NoDocbcCost,
        DiffWalk, Sex),
      ~ factor(.)
    ),
    # Categorical codes → ordered factors
    Age       = factor(Age,       ordered = TRUE),
    Education = factor(Education, ordered = TRUE),
    Income    = factor(Income,    ordered = TRUE),
    Diabetes  = factor(Diabetes)   # 0/1/2 as categories
  )

  head(df_clean, 20)

# =========================================================
# 9. Full-feature correlation analysis (memory safe)
# =========================================================

library(corrplot)

# 1) Build a numeric-only version of df_clean
#    - Encode HeartDiseaseorAttack as numeric 0/1
#    - Convert ALL remaining factors to numeric codes
df_corr <- df_clean %>%
  mutate(
    HeartDisease_num = as.numeric(as.character(HeartDiseaseorAttack))
  ) %>%
  select(-HeartDiseaseorAttack) %>%       # avoid duplicate target
  mutate(
    across(where(is.factor), ~ as.numeric(.))   # all factors -> numeric codes
  )

# Optional: check dimensions (should be features x features later)
cat("Number of features used for correlation:", ncol(df_corr), "\n")

# 2) Compute correlation matrix across ALL features
cor_mat <- cor(df_corr, use = "pairwise.complete.obs")

# Quick sanity check
dim(cor_mat)
# Should be something like 23 x 23 (depending on how many columns you have)

# 3) Plot full correlation heatmap
corrplot(cor_mat,
         method = "color",
         type   = "upper",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.5,
         main  = "Correlation Heatmap of All Features")

# 4) Correlation list for BMI (regression task)
if ("BMI" %in% rownames(cor_mat)) {
  bmi_cor <- sort(cor_mat["BMI", ], decreasing = TRUE)
  cat("\nCorrelation with BMI:\n")
  print(round(bmi_cor, 3))
} else {
  warning("BMI not found in correlation matrix row names.")
}

# 5) Correlation list for HeartDisease (classification task)
if ("HeartDisease_num" %in% rownames(cor_mat)) {
  hd_cor <- sort(cor_mat["HeartDisease_num", ], decreasing = TRUE)
  cat("\nCorrelation with HeartDiseaseorAttack (0/1):\n")
  print(round(hd_cor, 3))
} else {
  warning("HeartDisease_num not found in correlation matrix row names.")
}


# ------------------------------------------------------
# 10. Normalization (z-score) for numeric predictors
# ------------------------------------------------------

# Create scaled versions of continuous variables
df_model <- df_clean %>%
  mutate(
    BMI_scaled          = as.numeric(scale(BMI)),
    BMI_capped_scaled   = as.numeric(scale(BMI_capped)),
    MentHlth_scaled     = as.numeric(scale(MentHlth)),
    PhysHlth_scaled     = as.numeric(scale(PhysHlth))
  )

# Quick check
summary(df_model[, c("BMI", "BMI_scaled",
                     "BMI_capped", "BMI_capped_scaled",
                     "MentHlth", "MentHlth_scaled",
                     "PhysHlth", "PhysHlth_scaled")])


# ============================================================
# 11. Train–Validation–Test Split (60% / 20% / 20%)
# ============================================================

set.seed(123)  # reproducibility

n <- nrow(df_model)
indices <- sample(seq_len(n))

# Compute split sizes
train_size <- floor(0.6 * n)
valid_size <- floor(0.2 * n)

# Generate splits
train_data <- df_model[indices[1:train_size], ]
valid_data <- df_model[indices[(train_size + 1):(train_size + valid_size)], ]
test_data  <- df_model[indices[(train_size + valid_size + 1):n], ]

# Print sizes
cat("Training rows:", nrow(train_data), "\n")
cat("Validation rows:", nrow(valid_data), "\n")
cat("Test rows:", nrow(test_data), "\n")





