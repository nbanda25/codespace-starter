#!/usr/bin/env Rscript
# ==============================================================================
# Clinical Data Engineering: Diabetes Dataset Preparation and Risk Stratification
# Author: Niket Banda
# Date: 2026-07-17
# ==============================================================================

# Load necessary library
library(tidyverse)

# Define paths
dataset_path <- "diabetes_prediction_dataset.csv"
output_path <- "cleaned_diabetes_data.csv"

cat("========================================================================\n")
cat("Starting clinical data preparation pipeline...\n")
cat("========================================================================\n\n")

# 1. Load the dataset
cat("[1/4] Loading raw dataset...\n")
raw_data <- read_csv(dataset_path, show_col_types = FALSE)
cat(paste("Loaded dataset with", nrow(raw_data), "rows and", ncol(raw_data), "columns.\n\n"))

# 2. Remove duplicated rows
cat("[2/4] Removing duplicated records...\n")
cleaned_data <- raw_data %>% distinct()
removed_count <- nrow(raw_data) - nrow(cleaned_data)
cat(paste("Removed", removed_count, "duplicate rows. Remaining rows:", nrow(cleaned_data), "\n\n"))

# 3. Convert columns to properly labeled factor columns
cat("[3/4] Formatting categorical variables as factors...\n")
prepared_data <- cleaned_data %>%
  mutate(
    # Convert gender to factor
    gender = as.factor(gender),
    
    # Convert hypertension to factor with descriptive labels
    hypertension = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")),
    
    # Convert heart_disease to factor with descriptive labels
    heart_disease = factor(heart_disease, levels = c(0, 1), labels = c("No", "Yes")),
    
    # Convert smoking_history to factor
    smoking_history = as.factor(smoking_history)
  )

# 4. Engineer the 'risk_tier' feature
cat("[4/4] Stratifying patients into risk tiers...\n")
# If a patient has diabetes (diabetes == 1), label them 'Diabetic'.
# For non-diabetic patients:
# - 'High Risk' if HbA1c_level >= 5.7 OR blood_glucose_level >= 140
# - 'Moderate Risk' if BMI >= 25
# - 'Low Risk' otherwise.
prepared_data <- prepared_data %>%
  mutate(
    risk_tier = case_when(
      diabetes == 1 ~ "Diabetic",
      HbA1c_level >= 5.7 | blood_glucose_level >= 140 ~ "High Risk",
      bmi >= 25 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    ),
    # Convert to ordered factor for logical sequencing in reports/plots
    risk_tier = factor(risk_tier, levels = c("Low Risk", "Moderate Risk", "High Risk", "Diabetic"))
  )

cat("Risk stratification complete. Saving cleaned dataset...\n\n")
write_csv(prepared_data, output_path)

# ==============================================================================
# Summary and Output
# ==============================================================================
cat("========================================================================\n")
cat("DATA SUMMARY: RISK STRATIFICATION OF PATIENT POPULATION\n")
cat("========================================================================\n")

summary_table <- prepared_data %>%
  group_by(risk_tier) %>%
  summarize(
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = (Count / sum(Count)) * 100
  )

# Print the summary table in a clean, human-readable format
print(summary_table)

cat("========================================================================\n")
cat("Data preparation pipeline executed successfully.\n")
cat("Cleaned dataset saved to:", output_path, "\n")
cat("========================================================================\n")
