import pandas as pd
import numpy as np

# === 1. Load your dataset ===
df = pd.read_csv("heart_disease_health_indicators_BRFSS2015.csv")

# === 2. Number of rows to randomly corrupt ===
num_rows_to_modify = 1358

# === 3. Randomly select the row indices ===
np.random.seed(42)   # for reproducibility
rows_to_modify = np.random.choice(df.index, size=num_rows_to_modify, replace=False)

# === 4. For each selected row, randomly remove (set NaN) for some features ===
# Set how many columns to remove PER ROW (you can change this number)
min_features = 2     # minimum missing values per row
max_features = 5     # maximum missing values per row

for row in rows_to_modify:
    # randomly choose how many features to remove for this row
    k = np.random.randint(min_features, max_features + 1)
    
    # randomly choose which columns to remove
    cols_to_nan = np.random.choice(df.columns, size=k, replace=False)
    
    # set values to NaN
    df.loc[row, cols_to_nan] = np.nan

# === 5. Save the modified dataset ===
df.to_csv("heart_disease_missing_simulated.csv", index=False)

print("Done! Missing values have been inserted into 1358 rows.")
print("Saved as: heart_disease_missing_simulated.csv")
