# ===============================
# 1. Load Libraries
# ===============================
#install.packages("tidyverse")
#install.packages("forcats")
library(tidyverse)
library(forcats)
# ===============================
# 2. Load Dataset
# ===============================
# Change to your actual file path!
df <- read.csv("C://Users//Wei Khang//heartDisease//heart_disease_missing_simulated.csv")

 
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

# -----------------------------------------------------
# Convert Age Code to Meaningful Age Group (CDC Mapping)
# -----------------------------------------------------
# The Age variable is coded from 1–13 and represents age ranges, not actual age.
# We map each code to the correct age group and treat it as ordered categorical data.
df_clean <- df_clean %>%
  mutate(
    AgeGroup = factor(Age,
                      levels = 1:13,
                      labels = c(
                        "18-24", "25-29", "30-34", "35-39", "40-44",
                        "45-49", "50-54", "55-59", "60-64",
                        "65-69", "70-74", "75-79", "80+"
                      ),
                      ordered = TRUE
    )
  )

# Display first 10 rows to verify AgeGroup conversion
df_clean %>% 
  select(Age, AgeGroup) %>% 
  head(10)

# Frequency of each age group
table(df_clean$AgeGroup)



# -----------------------------------------------------
# ️Create Broader Age Bands
# -----------------------------------------------------
# Group AgeCategory into wider age bands to simplify modeling and interpretation.
df_clean <- df_clean %>%
  mutate(
    AgeBand = case_when(
      Age <= 3  ~ "18-34",   # Young adults
      Age <= 6  ~ "35-49",   # Middle age
      Age <= 9  ~ "50-64",   # Older adults
      Age <= 11 ~ "65-74",   # Elderly
      TRUE      ~ "75+"      # Very elderly
    ),
    AgeBand = factor(AgeBand, ordered = TRUE)
  )

# Display AgeBand mapping
df_clean %>% 
  select(Age, AgeBand) %>% 
  head(10)

# Frequency of age bands
table(df_clean$AgeBand)



# -----------------------------------------------------
# Lifestyle Risk Score (Composite Feature)
# -----------------------------------------------------
# Combine multiple unhealthy lifestyle behaviors into a single numerical risk score.
# Higher score indicates less healthy lifestyle.
df_clean <- df_clean %>%
  mutate(
    RiskScore =
      as.numeric(Smoker) +              # Smoking behavior
      as.numeric(HvyAlcoholConsump) +   # Heavy drinking behavior
      (1 - as.numeric(PhysActivity)) +  # Inactivity
      (1 - as.numeric(Fruits)) +        # Low fruit intake
      (1 - as.numeric(Veggies))         # Low vegetable intake
  )

# Inspect calculated RiskScore
df_clean %>%
  select(Smoker, HvyAlcoholConsump, PhysActivity, Fruits, Veggies, RiskScore) %>%
  head(10)

summary(df_clean$RiskScore)



# -----------------------------------------------------
# Disease Burden Index (Chronic Disease Count)
# -----------------------------------------------------
# Counts the number of existing health conditions per individual.
# Represents overall comorbidity burden.
df_clean <- df_clean %>%
  mutate(
    DiseaseCount =
      as.numeric(HighBP) +      # High blood pressure
      as.numeric(HighChol) +    # High cholesterol
      as.numeric(Diabetes) +    # Diabetes
      as.numeric(Stroke)        # History of stroke
  )

# Inspect disease burden result
df_clean %>%
  select(HighBP, HighChol, Diabetes, Stroke, DiseaseCount) %>%
  head(10)

table(df_clean$DiseaseCount)



# -----------------------------------------------------
# Health Stress Index (Mental + Physical Health)
# -----------------------------------------------------
# Combines mental and physical unhealthy days into one stress indicator.
df_clean <- df_clean %>%
  mutate(
    HealthStressIndex = MentHlth + PhysHlth
  )

# Inspect health stress indicator
df_clean %>% 
  select(MentHlth, PhysHlth, HealthStressIndex) %>% 
  head(10)

summary(df_clean$HealthStressIndex)



# -----------------------------------------------------
# Healthcare Access Score
# -----------------------------------------------------
# Captures access to healthcare services combining insurance and affordability.
df_clean <- df_clean %>%
  mutate(
    HealthcareScore =
      as.numeric(AnyHealthcare) +       # Has insurance
      (1 - as.numeric(NoDocbcCost))     # Could afford doctor visit
  )

# Inspect healthcare access score
df_clean %>% 
  select(AnyHealthcare, NoDocbcCost, HealthcareScore) %>% 
  head(10)

table(df_clean$HealthcareScore)



# -----------------------------------------------------
# Obesity Indicator
# -----------------------------------------------------
# Create obesity flag using BMI threshold (BMI >= 30 is obese).
df_clean <- df_clean %>%
  mutate(
    ObeseFlag = ifelse(BMI >= 30, 1, 0),
    ObeseFlag = factor(ObeseFlag)
  )

# Inspect obesity flag
df_clean %>% 
  select(BMI, ObeseFlag) %>% 
  head(10)

table(df_clean$ObeseFlag)



# -----------------------------------------------------
# Lifestyle Risk Category
# -----------------------------------------------------
# Convert numeric RiskScore into interpretable risk classes.
df_clean <- df_clean %>%
  mutate(
    LifestyleProfile = case_when(
      RiskScore <= 1 ~ "Healthy",
      RiskScore <= 3 ~ "ModerateRisk",
      TRUE ~ "HighRisk"
    ),
    LifestyleProfile = factor(LifestyleProfile)
  )

# Inspect lifestyle categories
df_clean %>% 
  select(RiskScore, LifestyleProfile) %>% 
  head(10)

table(df_clean$LifestyleProfile)

# =========================================================
# EDA
# =========================================================
library(ggplot2)
library(dplyr)
library(scales)

df_plot <- df_clean %>%
  mutate(
    HeartDisease = factor(HeartDiseaseorAttack,
                          levels = c(0, 1),
                          labels = c("No", "Yes")),
    SexLabel = factor(Sex,
                      levels = c(0, 1),
                      labels = c("Female", "Male"))
  )

# --------------------------------
# 9.1 Target variable distribution                                              # GOOD
# --------------------------------
df_donut <- df_plot %>%
  count(HeartDisease) %>%
  mutate(prop = n / sum(n))

ggplot(df_donut, aes(x = 2, y = prop, fill = HeartDisease)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  labs(title = "Heart Disease Prevalence (Donut Chart)") +
  geom_text(aes(label = percent(prop)), 
            position = position_stack(vjust = 0.5))

# ---------------------------------
# 9.2 Heart disease rate by AgeBand    
# ---------------------------------
# Two choose one(i think below one better)

ggplot(df_plot, aes(x = AgeBand, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Heart Disease Rate by Age Band",
       x = "Age Band",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

counts <- table(df_plot$HeartDisease)                                           # ok
barplot(counts,
        col = "#2C82C9",
        border = NA,
        main = "Heart Disease Prevalence",
        xlab = "Heart Disease",
        ylab = "Count")


# ---------------------------------
# 9.3 Heart disease rate by Sex    
# ---------------------------------                
# Two choose one(i think upper one better)
ggplot(df_plot, aes(x = SexLabel, fill = HeartDisease)) +                       # ok
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = c("#F76C6C", "#1ECBE1")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Heart Disease Rate by Sex",
       x = "Sex",
       y = "Proportion") +
  theme_minimal(base_size = 14)

par(mfrow = c(1,2))
for (sex in levels(df_plot$SexLabel)) {
  tab <- table(df_plot$HeartDisease[df_plot$SexLabel == sex])
  pie(tab,
      col = c("#F76C6C", "#1ECBE1"),
      main = paste("Heart Disease Rate:", sex))
}
par(mfrow = c(1,1))


# ---------------------------------
# 9.4 BMI category vs HeartDisease                                              # ok
# ---------------------------------   
ggplot(df_plot, aes(BMI_Category, fill = HeartDisease)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = c("#F9A825", "#1976D2")) +
  labs(title = "Heart Disease Rate by BMI Category",
       x = "BMI Category",
       y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal(base_size = 14)


# ---------------------------------
# 9.5 Obesity flag vs HeartDisease                           
# ---------------------------------
# Two choose one(i think below one better)
ggplot(df_plot, aes(x = ObeseFlag, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Heart Disease Rate by Obesity Status (BMI ≥ 30)",
       x = "ObeseFlag (0 = No, 1 = Yes)",
       y = "Proportion")

ggplot(df_plot, aes(factor(ObeseFlag), fill = HeartDisease)) +                  # ok
  geom_bar(position = "fill", width = 0.5) +
  scale_fill_manual(values = c("#81D4FA", "#EF5350")) +
  labs(title = "Heart Disease Rate by Obesity Status",
       x = "Obesity (0 = No, 1 = Yes)",
       y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal(base_size = 14)

# -------------------------------------
# 9.6 LifestyleProfile vs HeartDisease                        
# -------------------------------------
# Two choose one(i think below one better)
ggplot(df_plot, aes(LifestyleProfile, fill = HeartDisease)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = c("#43A047", "#EF5350")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Heart Disease Rate by Lifestyle Profile",
       x = "Lifestyle Category",
       y = "Proportion") +
  theme_minimal(base_size = 14)

tab <- table(df_plot$LifestyleProfile, df_plot$HeartDisease)                    # ok
barplot(prop.table(tab, 1),
        beside = FALSE,
        col = c("#43A047", "#EF5350"),
        legend = TRUE,
        main = "Heart Disease Rate by Lifestyle Profile")


# -------------------------------------
# 9.7 General health (GenHlth_Factor)                                           # ok                       
# -------------------------------------
ggplot(df_plot, aes(x = GenHlth_Factor, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Heart Disease Rate by Self-Reported General Health",
       x = "General Health",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------These 2 no idea need or not(if no need drop it)---------------------------------------------------
# -------------------------------------
# 9.8 DiseaseCount vs HeartDisease                     
# -------------------------------------
ggplot(df_plot, aes(x = factor(DiseaseCount), fill = HeartDisease)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Heart Disease Rate by Disease Burden",
       x = "Number of Chronic Conditions",
       y = "Proportion")

# --------------------------------------------------------
# 9.9 HealthStressIndex distribution (MentHlth + PhysHlth)                     
# --------------------------------------------------------
ggplot(df_plot, aes(x = HealthStressIndex)) +
  geom_histogram(binwidth = 2, fill = "darkseagreen3", color = "white") +
  labs(title = "Health Stress Index Distribution",
       x = "HealthStressIndex (MentHlth + PhysHlth)",
       y = "Count")

#----------------------------------------------------------------------------------------------------------------

# --------------------------------------
# 9.10 HealthStressIndex vs HeartDisease                         
# --------------------------------------
# Two choose one(i think below one better)
ggplot(df_plot, aes(x = HeartDisease, y = HealthStressIndex, fill = HeartDisease)) +
  geom_boxplot() +
  labs(title = "Health Stress Index by Heart Disease Status",
       x = "Heart Disease or Attack",
       y = "HealthStressIndex")

boxplot(HealthStressIndex ~ HeartDisease,                                       # ok
        data = df_plot,
        col = c("#4DB6AC", "#FF7043"),
        main = "Health Stress Index by Heart Disease",
        xlab = "Heart Disease",
        ylab = "Health Stress Index")


# -----------------------------------------
# 9.10 Healthcare Access Score Distribution                                     # ok                      
# -----------------------------------------
df_healthcare <- df_plot %>%
  group_by(HealthcareScore) %>%
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count))

ggplot(df_healthcare, aes(x = factor(HealthcareScore), y = Percent)) +
  geom_col(fill = "olivedrab3") +
  geom_text(aes(label = percent(Percent)), vjust = -0.5) +
  labs(title = "Healthcare Access Score Distribution (Waffle-style Bar)",
       x = "Healthcare Score",
       y = "Percent")


# =========================================================
# 10. Full-feature correlation analysis (memory safe)
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


#=========================================================================================
#=========================================================================================
#=========================================================================================
#=========================================================================================
### Choose One heatmap
#=========================================================================================
# 3) Plot full correlation heatmap
corrplot(cor_mat,
         method = "color",
         type   = "upper",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.5,
         main  = "Correlation Heatmap of All Features")
#=========================================================================================
library(ggplot2)
library(tidyr)
library(dplyr)

cor_long <- cor_mat %>%
  as.data.frame() %>%
  mutate(Var1 = rownames(.)) %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")

ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "#6BAED6", mid = "white", high = "#DE2D26",
    midpoint = 0, limit = c(-1, 1), name = "Correlation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Correlation Heatmap (ggplot2)",
    x = "",
    y = ""
  )

#=========================================================================================
#This one better?can hover see the value
#=========================================================================================
#install.packages("heatmaply")
library(heatmaply)
heatmaply(
  cor_mat,
  colors = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
  k_row = 3,
  k_col = 3,
  main = "Interactive Correlation Heatmap"
)

#=========================================================================================
#=========================================================================================
#=========================================================================================
#=========================================================================================

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
# 11. Normalization (z-score) for numeric predictors
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
# 12. Train–Validation–Test Split (60% / 20% / 20%)
#    -> create indices ONCE and reuse for all datasets
# ============================================================

set.seed(123)  # reproducibility

n <- nrow(df_model)
indices <- sample(seq_len(n))

# Compute split sizes
train_size <- floor(0.6 * n)
valid_size <- floor(0.2 * n)

# Row indices for each split
train_idx <- indices[1:train_size]
valid_idx <- indices[(train_size + 1):(train_size + valid_size)]
test_idx  <- indices[(train_size + valid_size + 1):n]

# (Optional) If you still want these for general use:
train_data <- df_model[train_idx, ]
valid_data <- df_model[valid_idx, ]
test_data  <- df_model[test_idx, ]

cat("Training rows:", length(train_idx), "\n")
cat("Validation rows:", length(valid_idx), "\n")
cat("Test rows:",      length(test_idx),  "\n")

# ============================================================
# 13A. Regression task dataset
# ============================================================
# 1) Drop BMI-leak features
df_reg <- df_model %>%
  select(
    -BMI_scaled,
    -BMI_capped,
    -BMI_capped_scaled,
    -BMI_Category,
    -ObeseFlag
  )

# 2) Move BMI to the first column (optional but nice)
df_reg <- df_reg %>%
  relocate(BMI)


# Use integer indices, NOT data frames
train_reg <- df_reg[train_idx, ]
valid_reg <- df_reg[valid_idx, ]
test_reg  <- df_reg[test_idx, ]

cat("\n[Regression] Rows in train/valid/test:\n")
cat("Train:", nrow(train_reg), "\n")
cat("Valid:", nrow(valid_reg), "\n")
cat("Test:",  nrow(test_reg),  "\n")

view(df_reg)
rm(df_reg)
# ============================================================
# 13B. Classification task dataset
# ============================================================

df_clf <- df_model %>%
  mutate(
    HeartDisease = factor(
      HeartDiseaseorAttack,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  ) %>%
  select(
    HeartDisease,
    everything()
  )

train_clf <- df_clf[train_idx, ]
valid_clf <- df_clf[valid_idx, ]
test_clf  <- df_clf[test_idx, ]

cat("\n[Classification] Rows in train/valid/test:\n")
cat("Train:", nrow(train_clf), "\n")
cat("Valid:", nrow(valid_clf), "\n")
cat("Test:",  nrow(test_clf),  "\n")
view(df_clf)


# ============================================================
# 14. Modelling
# ============================================================
# ============================================================
# 14A. Classification 
# ============================================================
# ============================================================
# 14B. Regression 
# ============================================================


library(randomForest)
library(xgboost)
library(caret)
library(ggplot2)

# ============================================================
# PART 1: RANDOM FOREST MODEL
# ============================================================

cat("\n========================================\n")
cat("RANDOM FOREST MODEL\n")
cat("========================================\n\n")

# Set seed for reproducibility
set.seed(123)

# ------------------------------------------------------------
# 1.1 Train Random Forest Model
# ------------------------------------------------------------

cat("Training Random Forest model...\n")

# Train model with tuned parameters
#rf_model <- randomForest(
#  BMI ~ .,                           # Predict BMI using all features
#  data = train_reg,
#  ntree = 100,                       # Number of trees
#  mtry = 8,                          # Number of variables tried at each split
#  importance = TRUE,                 # Calculate feature importance
#  nodesize = 5,                      # Minimum size of terminal nodes
#  maxnodes = NULL,                   # Maximum number of terminal nodes
#  keep.forest = TRUE
#)

install.packages("ranger")

library(ranger)

rf_model_fast <- ranger(
  BMI ~ .,
  data = train_reg,
  num.trees = 500,
  mtry = 8,
  min.node.size = 5,
  importance = "impurity",   
  num.threads = parallel::detectCores()  # use all CPU cores
)

print(rf_model_fast)

# ------------------------------------------------------------
# 1.3 Make Predictions
# ------------------------------------------------------------
# ------------------------------------------------------------
# 1.3 Make Predictions  (FIXED)
# ------------------------------------------------------------

cat("\n--- Making Predictions ---\n")

pred_rf_train <- predict(rf_model_fast, data = train_reg)$predictions
pred_rf_valid <- predict(rf_model_fast, data = valid_reg)$predictions
pred_rf_test  <- predict(rf_model_fast, data = test_reg)$predictions

# ------------------------------------------------------------
# 1.4 Evaluate Random Forest
# ------------------------------------------------------------
library(Metrics)
cat("\n--- Random Forest Performance ---\n")

rf_test_rmse    <- rmse(test_reg$BMI, pred_rf_test)
rf_test_mae     <- mae(test_reg$BMI, pred_rf_test)
rf_test_mape    <- mape(test_reg$BMI, pred_rf_test)
rf_test_r2      <- r2(test_reg$BMI, pred_rf_test)
rf_test_adj_r2  <- adjusted_r2(test_reg$BMI, pred_rf_test,
                               nrow(test_reg), ncol(test_reg) - 1)

cat(sprintf(
  "Test       -> RMSE: %.4f | MAE: %.4f | MAPE: %.2f%% | R²: %.4f | Adj R²: %.4f\n",
  rf_test_rmse, rf_test_mae, rf_test_mape,
  rf_test_r2, rf_test_adj_r2
))




cat("\n========================================\n")
cat("XGBOOST MODEL\n")
cat("========================================\n\n")

# ------------------------------------------------------------
# 2.1 Prepare Data for XGBoost
# ------------------------------------------------------------

cat("Preparing data for XGBoost...\n")

# Convert to matrix format (XGBoost requires numeric matrix)
X_train <- model.matrix(BMI ~ ., data = train_reg)[, -1]
y_train <- train_reg$BMI

X_valid <- model.matrix(BMI ~ ., data = valid_reg)[, -1]
y_valid <- valid_reg$BMI

X_test <- model.matrix(BMI ~ ., data = test_reg)[, -1]
y_test <- test_reg$BMI

# Create DMatrix objects (XGBoost's data structure)
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dvalid <- xgb.DMatrix(data = X_valid, label = y_valid)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

cat(sprintf("Training features: %d rows x %d columns\n", 
            nrow(X_train), ncol(X_train)))

# ------------------------------------------------------------
# 2.2 XGBoost Hyperparameter Grid Search
# ------------------------------------------------------------

cat("\n--- XGBoost Hyperparameter Grid Search ---\n")

# Define parameter grid
xgb_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),                    # Learning rate
  max_depth = c(3, 6, 9),                      # Tree depth
  min_child_weight = c(1, 3, 5),               # Minimum child weight
  subsample = c(0.7, 0.8, 0.9),                # Row sampling
  colsample_bytree = c(0.7, 0.8, 0.9),         # Column sampling
  gamma = c(0, 0.1, 0.2)                       # Minimum split loss
)

cat(sprintf("Total parameter combinations: %d\n", nrow(xgb_grid)))
cat("Note: Testing all combinations would take very long.\n")
cat("Using RANDOM SEARCH with 30 samples...\n\n")

# Random search: sample 30 combinations
set.seed(123)
sample_indices <- sample(1:nrow(xgb_grid), min(30, nrow(xgb_grid)))
xgb_grid_sample <- xgb_grid[sample_indices, ]

# Initialize results
xgb_tune_results <- data.frame()

# Progress bar setup
cat("Starting grid search...\n")
pb <- txtProgressBar(min = 0, max = nrow(xgb_grid_sample), style = 3)

# Grid search loop
for (i in 1:nrow(xgb_grid_sample)) {
  
  # Current parameters
  params_temp <- list(
    objective = "reg:squarederror",
    eta = xgb_grid_sample$eta[i],
    max_depth = xgb_grid_sample$max_depth[i],
    min_child_weight = xgb_grid_sample$min_child_weight[i],
    subsample = xgb_grid_sample$subsample[i],
    colsample_bytree = xgb_grid_sample$colsample_bytree[i],
    gamma = xgb_grid_sample$gamma[i],
    lambda = 1,
    alpha = 0
  )
  
  # Cross-validation with early stopping
  xgb_cv_temp <- xgb.cv(
    params = params_temp,
    data = dtrain,
    nrounds = 500,
    nfold = 3,
    early_stopping_rounds = 20,
    verbose = 0,
    showsd = FALSE
  )
  
  # Get best iteration and score
  best_iter <- xgb_cv_temp$best_iteration
  best_rmse <- min(xgb_cv_temp$evaluation_log$test_rmse_mean)
  
  # Train on full training set and evaluate on validation
  xgb_temp <- xgb.train(
    params = params_temp,
    data = dtrain,
    nrounds = best_iter,
    verbose = 0
  )
  
  pred_valid_temp <- predict(xgb_temp, dvalid)
  valid_rmse <- rmse(y_valid, pred_valid_temp)
  
  # Store results
  xgb_tune_results <- rbind(xgb_tune_results, data.frame(
    eta = xgb_grid_sample$eta[i],
    max_depth = xgb_grid_sample$max_depth[i],
    min_child_weight = xgb_grid_sample$min_child_weight[i],
    subsample = xgb_grid_sample$subsample[i],
    colsample_bytree = xgb_grid_sample$colsample_bytree[i],
    gamma = xgb_grid_sample$gamma[i],
    best_nrounds = best_iter,
    cv_rmse = best_rmse,
    valid_rmse = valid_rmse
  ))
  
  # Update progress
  setTxtProgressBar(pb, i)
}
close(pb)

# Find best parameters
best_xgb_idx <- which.min(xgb_tune_results$valid_rmse)
best_xgb_params <- xgb_tune_results[best_xgb_idx, ]

cat("\n\n--- Best XGBoost Hyperparameters ---\n")
print(best_xgb_params)

cat("\nTop 5 Parameter Configurations:\n")
print(head(xgb_tune_results[order(xgb_tune_results$valid_rmse), ], 5))

# Set best parameters
xgb_params <- list(
  objective = "reg:squarederror",
  eta = best_xgb_params$eta,
  max_depth = best_xgb_params$max_depth,
  min_child_weight = best_xgb_params$min_child_weight,
  subsample = best_xgb_params$subsample,
  colsample_bytree = best_xgb_params$colsample_bytree,
  gamma = best_xgb_params$gamma,
  lambda = 1,
  alpha = 0
)

best_nrounds <- best_xgb_params$best_nrounds

cat(sprintf("\nOptimal number of rounds: %d\n", best_nrounds))
cat(sprintf("Best Validation RMSE: %.4f\n", best_xgb_params$valid_rmse))

# ------------------------------------------------------------
# 2.3 Train Final XGBoost Model with Best Parameters
# ------------------------------------------------------------

cat("\n--- Training Final XGBoost Model with Best Parameters ---\n")

# Train with best parameters and validation monitoring
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds + 50,  # Add buffer
  watchlist = list(train = dtrain, valid = dvalid),
  early_stopping_rounds = 30,
  verbose = 1,
  print_every_n = 50
)

cat(sprintf("\nBest iteration: %d\n", xgb_model$best_iteration))


cat("\n--- Making Predictions (XGBoost) ---\n")

# Predictions
pred_xgb_train <- predict(xgb_model, dtrain)
pred_xgb_valid <- predict(xgb_model, dvalid)
pred_xgb_test <- predict(xgb_model, dtest)

# ------------------------------------------------------------
# 2.6 Evaluate XGBoost
# ------------------------------------------------------------

cat("\n--- XGBoost Performance ---\n")

# Training metrics
xgb_train_rmse <- rmse(y_train, pred_xgb_train)
xgb_train_mae <- mae(y_train, pred_xgb_train)
xgb_train_r2 <- r2(y_train, pred_xgb_train)

cat(sprintf("Training   -> RMSE: %.4f | MAE: %.4f | R²: %.4f\n", 
            xgb_train_rmse, xgb_train_mae, xgb_train_r2))

# Validation metrics
xgb_valid_rmse <- rmse(y_valid, pred_xgb_valid)
xgb_valid_mae <- mae(y_valid, pred_xgb_valid)
xgb_valid_r2 <- r2(y_valid, pred_xgb_valid)

cat(sprintf("Validation -> RMSE: %.4f | MAE: %.4f | R²: %.4f\n", 
            xgb_valid_rmse, xgb_valid_mae, xgb_valid_r2))

# Test metrics
xgb_test_rmse <- rmse(y_test, pred_xgb_test)
xgb_test_mae <- mae(y_test, pred_xgb_test)
xgb_test_mape <- mape(y_test, pred_xgb_test)
xgb_test_r2 <- r2(y_test, pred_xgb_test)
xgb_test_adj_r2 <- adjusted_r2(y_test, pred_xgb_test, 
                               length(y_test), ncol(X_test))

cat(sprintf("Test       -> RMSE: %.4f | MAE: %.4f | MAPE: %.2f%% | R²: %.4f | Adj R²: %.4f\n", 
            xgb_test_rmse, xgb_test_mae, xgb_test_mape, 
            xgb_test_r2, xgb_test_adj_r2))




















# Evaluate
# ==========================
# Custom Regression Metrics
# ==========================

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}

adjusted_r2 <- function(actual, predicted, n, p) {
  r2_val <- r2(actual, predicted)
  1 - ((1 - r2_val) * (n - 1) / (n - p - 1))
}


# Number of samples
n <- length(y_test)

# Number of predictors (exclude BMI)
p <- ncol(X_test)

rmse_xgb <- rmse(y_test, pred_rf_test)
mae_xgb  <- mae(y_test, pred_rf_test)
mape_xgb <- mape(y_test, pred_rf_test)
mse_xgb  <- mse(y_test, pred_rf_test)
r2_xgb   <- r2(y_test, pred_rf_test)
adjusted_r2_xgb <- adjusted_r2(y_test, pred_rf_test, n, p)

cat("XGBoost - Test RMSE:", rmse_xgb, "\n")
cat("XGBoost - Test MAE :", mae_xgb,  "\n")
cat("XGBoost - Test MAPE:", mape_xgb, "\n")
cat("XGBoost - Test MSE :", mse_xgb,  "\n")
cat("XGBoost - Test R2 :", r2_xgb,  "\n")
cat("XGBoost - Test Adjusted R2:", adjusted_r2_xgb, "\n")



