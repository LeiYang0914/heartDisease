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
df <- read.csv("C://Users//User//heartDisease//heart_disease_missing_simulated.csv")

 
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


# ============================================================
# 10. Select features BEFORE split 
# ============================================================

# Classification predictors (based on correlation + domain knowledge)
clf_features <- c(
  "HeartDiseaseorAttack",     # target
  "Age", "AgeGroup", "AgeBand",
  "Sex",
  "HighBP", "HighChol", "Diabetes", "Stroke",
  "Smoker", "PhysActivity",
  "PhysHlth", "MentHlth", "HealthStressIndex",
  "DiseaseCount", "ObeseFlag",
  "RiskScore", "LifestyleProfile",
  # numeric columns that later need scaling
  "BMI", "MentHlth", "PhysHlth"
)

df_clf_raw <- df_clean %>% select(all_of(clf_features))


# Regression predictors
reg_features <- c(
  "BMI",                         # target
  "Age", "AgeGroup", "AgeBand",
  "Sex",
  "HighBP", "HighChol", "Diabetes", "Stroke",
  "Smoker", "PhysActivity",
  "PhysHlth", "MentHlth", "HealthStressIndex",
  "DiseaseCount", "RiskScore", "LifestyleProfile",
  "Income", "Education"
)

df_reg_raw <- df_clean %>% select(all_of(reg_features))



#===============================================================
# 11. Train/Valid/Test Split 
#===============================================================

set.seed(123)

n <- nrow(df_clean)
indices <- sample(seq_len(n))

train_size <- floor(0.6 * n)
valid_size <- floor(0.2 * n)

train_idx <- indices[1:train_size]
valid_idx <- indices[(train_size + 1):(train_size + valid_size)]
test_idx  <- indices[(train_size + valid_size + 1):n]

cat("\nTrain:", length(train_idx),
    "\nValid:", length(valid_idx),
    "\nTest:",  length(test_idx), "\n")


#===============================================================
# 12. BUILD DATASETS FOR BOTH TASKS (still unscaled)
#===============================================================

# CLASSIFICATION
train_clf <- df_clf_raw[train_idx, ]
valid_clf <- df_clf_raw[valid_idx, ]
test_clf  <- df_clf_raw[test_idx, ]

# Convert target to factor here
train_clf$HeartDisease <- factor(train_clf$HeartDiseaseorAttack, levels=c(0,1), labels=c("No","Yes"))
valid_clf$HeartDisease <- factor(valid_clf$HeartDiseaseorAttack, levels=c(0,1), labels=c("No","Yes"))
test_clf$HeartDisease  <- factor(test_clf$HeartDiseaseorAttack, levels=c(0,1), labels=c("No","Yes"))

train_clf <- train_clf %>% select(-HeartDiseaseorAttack) %>% relocate(HeartDisease)
valid_clf <- valid_clf %>% select(-HeartDiseaseorAttack) %>% relocate(HeartDisease)
test_clf  <- test_clf  %>% select(-HeartDiseaseorAttack) %>% relocate(HeartDisease)

# REGRESSION (target BMI)
train_reg <- df_reg_raw[train_idx, ]
valid_reg <- df_reg_raw[valid_idx, ]
test_reg  <- df_reg_raw[test_idx, ]



#===============================================================
# 13. Normalization 
#===============================================================

# Columns to scale
scale_cols <- c("BMI", "MentHlth", "PhysHlth")

# Compute scaler parameters FROM TRAIN ONLY
scaler_means <- sapply(train_clf[, scale_cols], mean)
scaler_sds   <- sapply(train_clf[, scale_cols], sd)


# Scaling function
scale_apply <- function(df, cols, means, sds) {
  df[, paste0(cols, "_scaled")] <- sweep(df[, cols], 2, means, FUN = "-") / sds
  return(df)
}

# Apply scaling to ALL datasets (but fitted on TRAIN ONLY)
train_clf <- scale_apply(train_clf, scale_cols, scaler_means, scaler_sds)
valid_clf <- scale_apply(valid_clf, scale_cols, scaler_means, scaler_sds)
test_clf  <- scale_apply(test_clf,  scale_cols, scaler_means, scaler_sds)

train_reg <- scale_apply(train_reg, scale_cols, scaler_means, scaler_sds)
valid_reg <- scale_apply(valid_reg, scale_cols, scaler_means, scaler_sds)
test_reg  <- scale_apply(test_reg,  scale_cols, scaler_means, scaler_sds)



#===============================================================
# 14. Drop unscaled original variables ONLY if model does not need them
#===============================================================

# Remove unscaled if needed
train_clf <- train_clf %>% select(-BMI, -MentHlth, -PhysHlth)
valid_clf <- valid_clf %>% select(-BMI, -MentHlth, -PhysHlth)
test_clf  <- test_clf  %>% select(-BMI, -MentHlth, -PhysHlth)

# For Regression, KEEP original BMI (target), drop other unscaled ones
train_reg <- train_reg %>% select(-BMI_scaled,-MentHlth, -PhysHlth)
valid_reg <- valid_reg %>% select(-BMI_scaled,-MentHlth, -PhysHlth)
test_reg  <- test_reg  %>% select(-BMI_scaled,-MentHlth, -PhysHlth)

#===============================================================
# Data Preparation Complete
#===============================================================

cat("\n==== FINAL DATASET SHAPES ==== \n")

cat("\n[Classification]\n")
cat("Train:", nrow(train_clf), "rows,", ncol(train_clf), "columns\n")
cat("Valid:", nrow(valid_clf), "rows,", ncol(valid_clf), "columns\n")
cat("Test :", nrow(test_clf),  "rows,", ncol(test_clf),  "columns\n")

cat("\n[Regression]\n")
cat("Train:", nrow(train_reg), "rows,", ncol(train_reg), "columns\n")
cat("Valid:", nrow(valid_reg), "rows,", ncol(valid_reg), "columns\n")
cat("Test :", nrow(test_reg),  "rows,", ncol(test_reg),  "columns\n")

# ============================================================
# 15. Modelling
# ============================================================
# ============================================================
# 15A. Classification 
# ============================================================
# ============================================================
# CLASSIFICATION PIPELINE FLOW
# ============================================================
# 1. df_clf = FACTOR ("No"/"Yes") target  → used for SMOTE
# 2. SMOTE requires FACTOR target → so keep "No"/"Yes" for SMOTE
# 3. XGBoost & LightGBM require NUMERIC (0/1) → convert AFTER SMOTE
# 4. Final models:
#       - LightGBM (trained on SMOTE data)
#       - XGBoost  (trained on original data with weights)
# ============================================================

# ============================================================
#          LIGHTGBM + SMOTE + BAYESIAN OPTIMIZATION
# ============================================================

library(lightgbm)
library(ParBayesianOptimization)
library(Matrix)
library(smotefamily)
library(pROC)
library(caret)
library(xgboost)

# ============================================================
# 1.1 PREPARE DATA FOR MODELLING
# ============================================================

train_y_num <- ifelse(train_clf$HeartDisease == "Yes", 1, 0)
valid_y_num <- ifelse(valid_clf$HeartDisease == "Yes", 1, 0)
test_y_num  <- ifelse(test_clf$HeartDisease  == "Yes", 1, 0)

# Model Matrix (one-hot encoding)
X_train <- model.matrix(HeartDisease ~ ., train_clf)[, -1]
X_valid <- model.matrix(HeartDisease ~ ., valid_clf)[, -1]
X_test  <- model.matrix(HeartDisease ~ ., test_clf)[, -1]

# ============================================================
# 1.2 APPLY SMOTE TO TRAINING ONLY
# ============================================================
cat("\n--- Running SMOTE on training data ---\n")

df_smote_X <- as.data.frame(X_train)
df_smote_y <- factor(train_y_num, levels = c(0, 1))

smote_out <- SMOTE(
  df_smote_X,
  df_smote_y,
  K = 5,
  dup_size = 3
)

X_train_smote <- as.matrix(smote_out$data[, -ncol(smote_out$data)])
y_train_smote <- as.numeric(as.character(smote_out$data$class))

cat("\nOriginal distribution:\n")
print(table(train_y_num))
cat("\nAfter SMOTE:\n")
print(table(y_train_smote))

# ============================================================
# 1.3 CREATE LIGHTGBM DATASETS
# ============================================================

dtrain <- lgb.Dataset(
  data = X_train_smote,
  label = y_train_smote
)

dvalid <- lgb.Dataset.create.valid(
  dtrain,
  data = as.matrix(X_valid),
  label = valid_y_num
)

# ============================================================
# 1.4 BAYESIAN OPTIMIZATION FOR LIGHTGBM
# ============================================================
# Define the scoring function for Bayesian Optimization
# ============================================================

lgb_bayes <- function(learning_rate, num_leaves, feature_fraction,
                      bagging_fraction, min_data_in_leaf) {
  
  params <- list(
    objective = "binary",
    metric = "auc",
    learning_rate = learning_rate,
    num_leaves = as.integer(num_leaves),
    feature_fraction = feature_fraction,
    bagging_fraction = bagging_fraction,
    bagging_freq = 5,
    min_data_in_leaf = as.integer(min_data_in_leaf),
    
    # IMPORTANT FIX
    feature_pre_filter = FALSE,
    force_row_wise = TRUE,    # recommended for sparse matrix
    verbosity = -1
  )
  
  model <- lgb.train(
    params = params,
    data = dtrain,
    valids = list(valid = dvalid),
    nrounds = 500,
    early_stopping_rounds = 30,
    verbose = -1
  )
  
  best_auc <- max(unlist(model$record_evals$valid$auc$eval))
  return(list(Score = best_auc))
}

# ============================================================
# 1.5 Run Bayesian Optimization 
# ============================================================

set.seed(123)

opt_results <- bayesOpt(
  FUN = lgb_bayes,
  bounds = list(
    learning_rate = c(0.01, 0.2),
    num_leaves = c(20L, 80L),
    feature_fraction = c(0.6, 1.0),
    bagging_fraction = c(0.6, 1.0),
    min_data_in_leaf = c(10L, 80L)
  ),
  initPoints = 8,   # MUST be > 5 parameters
  iters.n = 20,
  acq = "ucb",
  kappa = 2.5,
  parallel = FALSE,
  verbose = TRUE
)

cat("\n========== BEST BAYESIAN OPT PARAMETERS ==========\n")
print(getBestPars(opt_results))
best_params <- getBestPars(opt_results)

# ============================================================
# 1.6 TRAIN FINAL MODEL USING OPTIMIZED PARAMETERS
# ============================================================

final_params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = best_params$learning_rate,
  num_leaves = as.integer(best_params$num_leaves),
  feature_fraction = best_params$feature_fraction,
  bagging_fraction = best_params$bagging_fraction,
  bagging_freq = 5,
  min_data_in_leaf = as.integer(best_params$min_data_in_leaf),
  
  # FIX
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -1
)

final_model <- lgb.train(
  params = final_params,
  data = dtrain,
  valids = list(valid = dvalid),
  nrounds = 700,
  early_stopping_rounds = 40,
  verbose = 1
)

# ============================================================
# 1.7 EVALUATE FINAL MODEL ON TEST SET
# ============================================================

pred_prob <- predict(final_model, as.matrix(X_test))
pred <- ifelse(pred_prob > 0.5, 1, 0)

pred_factor <- factor(pred, levels = c(0, 1))
test_factor <- factor(test_y_num, levels = c(0, 1))

conf <- table(Predicted = pred_factor, Actual = test_factor)

acc <- mean(pred == test_y_num)
prec <- caret::precision(pred_factor, test_factor)
rec <- caret::recall(pred_factor, test_factor)
f1 <- caret::F_meas(pred_factor, test_factor)
auc <- auc(test_y_num, pred_prob)

cat("\n================ FINAL LIGHTGBM PERFORMANCE ================\n")
print(conf)
cat(sprintf(
  "\nAccuracy: %.4f\nPrecision: %.4f\nRecall: %.4f\nF1 Score: %.4f\nAUC: %.4f\n",
  acc, prec, rec, f1, auc
))

# ============================================================
# 2.1 XGBOOST WITH BAYESIAN OPTIMIZATION (NO SMOTE)
# ============================================================

cat("\n========================================\n")
cat("   XGBOOST (Weighted + BayesOpt)\n")
cat("========================================\n")

# Prepare DMatrix
dtrain_xgb <- xgb.DMatrix(data = X_train, label = train_y_num)
dvalid_xgb <- xgb.DMatrix(data = X_valid, label = valid_y_num)
dtest_xgb  <- xgb.DMatrix(data = X_test,  label = test_y_num)

# Compute imbalance weight
scale_pos_weight_val <- sum(train_y_num == 0) / sum(train_y_num == 1)


# ============================================================
# 2.2 Bayesian Optimization objective function for XGBoost
# ============================================================

xgb_bayes <- function(eta, max_depth, subsample,
                      colsample_bytree, min_child_weight) {
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = eta,
    max_depth = as.integer(max_depth),
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    scale_pos_weight = scale_pos_weight_val,
    tree_method = "hist",
    booster = "gbtree"
  )
  
  xgb_model <- xgb.train(
    params = params,
    data = dtrain_xgb,
    nrounds = 500,
    watchlist = list(valid = dvalid_xgb),
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  best_auc <- max(xgb_model$evaluation_log$valid_auc)
  
  return(list(Score = best_auc))
}


# ============================================================
# 2.3 Run Bayesian Optimization for XGBoost
# ============================================================

set.seed(123)

opt_xgb <- bayesOpt(
  FUN = xgb_bayes,
  bounds = list(
    eta = c(0.01, 0.2),
    max_depth = c(3L, 10L),
    subsample = c(0.6, 1.0),
    colsample_bytree = c(0.6, 1.0),
    min_child_weight = c(1, 20)
  ),
  initPoints = 8,
  iters.n = 20,
  acq = "ucb",
  kappa = 2.5,
  verbose = TRUE
)

best_xgb_params <- getBestPars(opt_xgb)
cat("\nBest XGB Params:\n")
print(best_xgb_params)


# ============================================================
# 2.4 Train FINAL XGBoost model using optimized parameters
# ============================================================

final_xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = best_xgb_params$eta,
  max_depth = as.integer(best_xgb_params$max_depth),
  subsample = best_xgb_params$subsample,
  colsample_bytree = best_xgb_params$colsample_bytree,
  min_child_weight = best_xgb_params$min_child_weight,
  scale_pos_weight = scale_pos_weight_val,
  tree_method = "hist",
  booster = "gbtree"
)

xgb_clf <- xgb.train(
  params = final_xgb_params,
  data = dtrain_xgb,
  nrounds = 700,
  watchlist = list(valid = dvalid_xgb),
  early_stopping_rounds = 30,
  verbose = 1
)

# ============================================================
# 2.5 Evaluate FINAL XGBoost model
# ============================================================

pred_prob_xgb <- predict(xgb_clf, dtest_xgb)
pred_xgb <- ifelse(pred_prob_xgb > 0.5, 1, 0)

pred_xgb_factor <- factor(pred_xgb, levels = c(0,1))
test_factor_xgb <- factor(test_y_num, levels = c(0,1))

xgb_conf <- table(Predicted = pred_xgb_factor, Actual = test_factor_xgb)

xgb_acc  <- mean(pred_xgb == test_y_num)
xgb_prec <- caret::precision(pred_xgb_factor, test_factor_xgb)
xgb_rec  <- caret::recall(pred_xgb_factor, test_factor_xgb)
xgb_f1   <- caret::F_meas(pred_xgb_factor, test_factor_xgb)
xgb_auc  <- auc(test_y_num, pred_prob_xgb)

cat("\nXGBoost Confusion Matrix:\n")
print(xgb_conf)

cat(sprintf(
  "\nXGBoost Accuracy: %.4f | Precision: %.4f | Recall: %.4f | F1: %.4f | AUC: %.4f\n",
  xgb_acc, xgb_prec, xgb_rec, xgb_f1, xgb_auc
))

# ============================================================
# 15B. RANDOM FOREST REGRESSION (Optimized Ranger + BayesOpt)
# ============================================================

library(ranger)
library(Metrics)
library(ParBayesianOptimization)
library(dplyr)

cat("\n========================================\n")
cat("   RANDOM FOREST REGRESSION MODEL\n")
cat("========================================\n\n")

set.seed(123)

# ============================================================
# 1. Define Evaluation Metrics
# ============================================================

adj_r2 <- function(y_true, y_pred, p) {
  r2_val <- Metrics::r2(y_true, y_pred)
  n <- length(y_true)
  return(1 - (1 - r2_val) * ((n - 1) / (n - p - 1)))
}

p <- ncol(train_reg) - 1   # number of predictors


# ============================================================
# 2. Bayesian Optimization Function for Ranger
# ============================================================

rf_bayes <- function(mtry, min_node_size, sample_fraction) {
  
  model <- ranger(
    BMI ~ .,
    data = train_reg,
    num.trees = 500,
    mtry = as.integer(mtry),
    min.node.size = as.integer(min_node_size),
    sample.fraction = sample_fraction,
    importance = "impurity"
  )
  
  pred_valid <- predict(model, data = valid_reg)$predictions
  rmse_valid <- Metrics::rmse(valid_reg$BMI, pred_valid)
  
  return(list(Score = -rmse_valid))   # negative RMSE because BO maximizes Score
}

cat("Running Bayesian Optimization for Ranger...\n\n")

opt_results_rf <- bayesOpt(
  FUN = rf_bayes,
  bounds = list(
    mtry = c(2L, 15L),
    min_node_size = c(2L, 20L),
    sample_fraction = c(0.5, 1.0)
  ),
  initPoints = 8,
  iters.n = 20,
  acq = "ucb",
  kappa = 2.5,
  verbose = TRUE
)

best_params_rf <- getBestPars(opt_results_rf)
cat("\nBest Ranger Parameters:\n")
print(best_params_rf)


# ============================================================
# 3. Train Final Ranger Model
# ============================================================

rf_model_final <- ranger(
  BMI ~ .,
  data = train_reg,
  num.trees = 700,
  mtry = as.integer(best_params_rf$mtry),
  min.node.size = as.integer(best_params_rf$min_node_size),
  sample.fraction = best_params_rf$sample_fraction,
  importance = "impurity"
)

# ============================================================
# 4. Predictions
# ============================================================

pred_rf_train <- predict(rf_model_final, data = train_reg)$predictions
pred_rf_valid <- predict(rf_model_final, data = valid_reg)$predictions
pred_rf_test  <- predict(rf_model_final, data = test_reg)$predictions

# ============================================================
# 5. Final Evaluation (Test Set) 
# ============================================================

rf_rmse <- rmse(test_reg$BMI, pred_rf_test)
rf_mae  <- mae(test_reg$BMI, pred_rf_test)
rf_mape <- mape(test_reg$BMI, pred_rf_test)

# caret R2 (predictions first!)
rf_r2 <- caret::R2(pred_rf_test, test_reg$BMI)

# Adjusted R²
p <- ncol(test_reg) - 1
n <- nrow(test_reg)
rf_adj_r2 <- 1 - (1 - rf_r2) * ((n - 1) / (n - p - 1))

cat("\n========= RANDOM FOREST (Ranger) FINAL PERFORMANCE =========\n")
cat(sprintf(
  "Test RMSE: %.4f | MAE: %.4f | MAPE: %.2f%% | R²: %.4f | Adj R²: %.4f\n",
  rf_rmse, rf_mae, rf_mape, rf_r2, rf_adj_r2
))


# ============================================================
# XGBOOST REGRESSION (RANDOM SEARCH)
# ============================================================

library(xgboost)
library(Metrics)

cat("\n========================================\n")
cat("        XGBOOST REGRESSION MODEL\n")
cat("========================================\n\n")

# ============================================================
# 2.1 Prepare DMatrix
# ============================================================

X_train <- model.matrix(BMI ~ ., data = train_reg)[, -1]
y_train <- train_reg$BMI

X_valid <- model.matrix(BMI ~ ., data = valid_reg)[, -1]
y_valid <- valid_reg$BMI

X_test  <- model.matrix(BMI ~ ., data = test_reg)[, -1]
y_test  <- test_reg$BMI

dtrain <- xgb.DMatrix(X_train, label = y_train)
dvalid <- xgb.DMatrix(X_valid, label = y_valid)
dtest  <- xgb.DMatrix(X_test,  label = y_test)


# ============================================================
# 2.2 RANDOM SEARCH HYPERPARAMETERS
# ============================================================

cat("Running Random Search for XGBoost Hyperparameters...\n")

set.seed(123)

# Define random search space
random_search_size <- 30

param_space <- data.frame(
  eta = runif(random_search_size, 0.01, 0.2),
  max_depth = sample(3:10, random_search_size, replace = TRUE),
  min_child_weight = sample(1:15, random_search_size, replace = TRUE),
  subsample = runif(random_search_size, 0.6, 1.0),
  colsample_bytree = runif(random_search_size, 0.6, 1.0),
  gamma = runif(random_search_size, 0, 0.3)
)

results <- data.frame()

pb <- txtProgressBar(min = 0, max = random_search_size, style = 3)

for (i in 1:random_search_size) {
  
  params <- list(
    objective = "reg:squarederror",
    eta = param_space$eta[i],
    max_depth = param_space$max_depth[i],
    min_child_weight = param_space$min_child_weight[i],
    subsample = param_space$subsample[i],
    colsample_bytree = param_space$colsample_bytree[i],
    gamma = param_space$gamma[i]
  )
  
  # Run CV for current parameter set
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 400,
    nfold = 3,
    early_stopping_rounds = 20,
    verbose = FALSE
  )
  
  best_iter <- cv$best_iteration
  best_rmse <- min(cv$evaluation_log$test_rmse_mean)
  
  # Store results
  results <- rbind(results, data.frame(
    eta = params$eta,
    max_depth = params$max_depth,
    min_child_weight = params$min_child_weight,
    subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    gamma = params$gamma,
    best_iter = best_iter,
    cv_rmse = best_rmse
  ))
  
  setTxtProgressBar(pb, i)
}
close(pb)

cat("\nRandom Search Completed.\n")

# ============================================================
# 2.3 Select Best Parameters
# ============================================================

best_idx <- which.min(results$cv_rmse)
best <- results[best_idx, ]
cat("\n--- BEST XGBOOST PARAMETERS (RANDOM SEARCH) ---\n")
print(best)

best_params <- list(
  objective = "reg:squarederror",
  eta = best$eta,
  max_depth = best$max_depth,
  min_child_weight = best$min_child_weight,
  subsample = best$subsample,
  colsample_bytree = best$colsample_bytree,
  gamma = best$gamma
)

best_nrounds <- best$best_iter + 30


# ============================================================
# 2.4 Train FINAL XGBOOST MODEL
# ============================================================

cat("\nTraining Final XGBoost Model...\n")

final_xgb <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(valid = dvalid),
  early_stopping_rounds = 20,
  print_every_n = 50
)

# ============================================================
# 2.5. Evaluate FINAL Model
# ============================================================

pred_xgb <- predict(final_xgb, dtest)

xgb_rmse <- rmse(y_test, pred_xgb)
xgb_mae  <- mae(y_test, pred_xgb)
xgb_mape <- mape(y_test, pred_xgb)
xgb_r2   <- 1 - sum((y_test - pred_xgb)^2) / sum((y_test - mean(y_test))^2)
n <- length(y_test)
p <- ncol(X_test)
xgb_adj_r2 <- 1 - ((1 - xgb_r2) * (n - 1) / (n - p - 1))

cat("\n=========== XGBOOST REGRESSION PERFORMANCE (TEST SET) ===========\n")
cat(sprintf(
  "RMSE: %.4f | MAE: %.4f | MAPE: %.2f%% | R²: %.4f | Adj R²: %.4f\n",
  xgb_rmse, xgb_mae, xgb_mape, xgb_r2, xgb_adj_r2
))



