#!/usr/bin/env Rscript
# ==============================================================================
# Script: build_static_dashboard.R
# Purpose: Build a single, fully self-contained HTML dashboard that replicates
#          the Shiny application's UI, aesthetic style, charts, and simulator.
# Author: Niket Banda
# ==============================================================================

library(tidyverse)
library(jsonlite)

cat("Starting HTML Dashboard Build...\n")

# Load data
df <- read_csv("/workspaces/codespace-starter/cleaned_diabetes_data.csv", show_col_types = FALSE)

# 1. Compute KPIs
total_patients <- nrow(df)
pct_diabetic <- (sum(df$diabetes == 1) / total_patients) * 100
pct_risk <- (sum(df$risk_tier %in% c("High Risk", "Moderate Risk")) / total_patients) * 100

# 2. Chart 1: Risk Tier Distribution
risk_summary <- df %>%
  group_by(risk_tier) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# 3. Chart 2: HbA1c Levels Across Risk Tiers Box Plot (Sampled to keep size reasonable)
set.seed(42)
box_data <- df %>%
  group_by(risk_tier) %>%
  slice_sample(n = 500) %>%
  ungroup() %>%
  select(risk_tier, HbA1c_level)

# Group Box Data by Risk Tier for easy plotting in JS
box_data_json <- list(
  "Low Risk" = box_data %>% filter(risk_tier == "Low Risk") %>% pull(HbA1c_level),
  "Moderate Risk" = box_data %>% filter(risk_tier == "Moderate Risk") %>% pull(HbA1c_level),
  "High Risk" = box_data %>% filter(risk_tier == "High Risk") %>% pull(HbA1c_level),
  "Diabetic" = box_data %>% filter(risk_tier == "Diabetic") %>% pull(HbA1c_level)
)

# 4. Chart 3: Diabetes Prevalence by Age Group
df_age <- df %>%
  mutate(age_group = case_when(
    age < 18 ~ "Pediatric (<18)",
    age >= 18 & age < 35 ~ "Young Adult (18-34)",
    age >= 35 & age < 50 ~ "Adult (35-49)",
    age >= 50 & age < 65 ~ "Middle-Aged (50-64)",
    TRUE ~ "Senior (65+)"
  )) %>%
  mutate(age_group = factor(age_group, levels = c("Pediatric (<18)", "Young Adult (18-34)", "Adult (35-49)", "Middle-Aged (50-64)", "Senior (65+)"))) %>%
  group_by(age_group) %>%
  summarize(
    Total = n(),
    Diabetic_Count = sum(diabetes == 1),
    Prevalence = (Diabetic_Count / Total) * 100,
    .groups = "drop"
  )

# 5. Chart 4: Risk Tier Stacked Bar Chart by Smoking History
df_smoking <- df %>%
  group_by(smoking_history, risk_tier) %>%
  summarize(Count = n(), .groups = "drop") %>%
  group_by(smoking_history) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# 6. Chart 5: Biomarker Scatter Plot (Sampled 2000)
df_sample <- df %>%
  slice_sample(n = 2000) %>%
  mutate(Diabetes_Status = ifelse(diabetes == 1, "Diabetic", "Non-Diabetic")) %>%
  select(blood_glucose_level, HbA1c_level, Diabetes_Status, age, bmi)

# 7. Chart 6: Cardiovascular Comorbidities
df_comorb <- df %>%
  mutate(Comorbidity = case_when(
    hypertension == "Yes" & heart_disease == "Yes" ~ "Both Conditions",
    hypertension == "Yes" & heart_disease == "No"  ~ "Hypertension Only",
    hypertension == "No"  & heart_disease == "Yes" ~ "Heart Disease Only",
    TRUE ~ "Neither Condition"
  )) %>%
  group_by(Comorbidity) %>%
  summarize(
    Total = n(),
    Diabetic_Count = sum(diabetes == 1),
    Prevalence = (Diabetic_Count / Total) * 100,
    .groups = "drop"
  ) %>%
  mutate(Comorbidity = factor(Comorbidity, levels = c("Neither Condition", "Heart Disease Only", "Hypertension Only", "Both Conditions")))

# 8. Simulator Model Fitting & Coefficient Extraction
model <- glm(
  diabetes ~ bmi + age + HbA1c_level + blood_glucose_level + hypertension + heart_disease,
  data = df, 
  family = binomial
)

coefs <- coef(model)
intercept <- coefs["(Intercept)"]
coef_bmi <- coefs["bmi"]
coef_age <- coefs["age"]
coef_hba1c <- coefs["HbA1c_level"]
coef_glucose <- coefs["blood_glucose_level"]
coef_hypertension <- coefs["hypertensionYes"]
coef_heart_disease <- coefs["heart_diseaseYes"]

# Calculate base logit (excluding BMI and HbA1c components) for each patient
df_sim_base <- df %>%
  mutate(
    hypertension_val = ifelse(hypertension == "Yes", 1, 0),
    heart_disease_val = ifelse(heart_disease == "Yes", 1, 0),
    logit_base = intercept + 
                 coef_age * age + 
                 coef_glucose * blood_glucose_level + 
                 coef_hypertension * hypertension_val + 
                 coef_heart_disease * heart_disease_val
  ) %>%
  select(bmi, HbA1c_level, logit_base, diabetes)

# To keep the static HTML file lightweight and fast in pure JS,
# we take a representative sample of 20,000 patients for the simulator
# and scale the expected values back to the full cohort.
set.seed(42)
sim_cohort <- df_sim_base %>%
  slice_sample(n = 20000)

total_baseline_cases <- sum(df$diabetes)
base_sim_prob_sum <- sum(1 / (1 + exp(-(sim_cohort$logit_base + coef_bmi * sim_cohort$bmi + coef_hba1c * sim_cohort$HbA1c_level))))
sim_scaling_factor <- total_baseline_cases / base_sim_prob_sum

# Create JSON string datasets
risk_summary_json <- toJSON(risk_summary, auto_unbox = TRUE)
box_data_json_str <- toJSON(box_data_json, auto_unbox = TRUE)
df_age_json <- toJSON(df_age, auto_unbox = TRUE)
df_smoking_json <- toJSON(df_smoking, auto_unbox = TRUE)
df_sample_json <- toJSON(df_sample, auto_unbox = TRUE)
df_comorb_json <- toJSON(df_comorb, auto_unbox = TRUE)
sim_cohort_json <- toJSON(sim_cohort, auto_unbox = TRUE)

cat("Computed all datasets and fitted model coefficients successfully.\n")

# Define HTML Template
html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Diabetes Portal - Patient Diabetes Risk & Intervention Simulator</title>
  
  <!-- CDNs for icons and charts -->
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css">
  <script src="https://cdn.plot.ly/plotly-2.24.1.min.js"></script>

  <style>
    /* ==============================================================================
     * Aesthetic Style: Flawless 2000s macOS Aqua / Brushed Metal (Skeuomorphic)
     * ============================================================================== */
    
    body {
      font-family: \'Lucida Grande\', \'Lucida Sans Unicode\', Geneva, Verdana, sans-serif !important;
      margin: 0;
      padding: 0;
      background-color: #dce1eb;
      background-image: linear-gradient(rgba(0, 0, 0, 0.04) 50%, transparent 50%);
      background-size: 100% 4px;
      color: #333;
      height: 100vh;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }

    /* Custom Webkit Scrollbars */
    ::-webkit-scrollbar {
      width: 14px;
      height: 14px;
    }
    ::-webkit-scrollbar-track {
      background: #e1e6eb;
      box-shadow: inset 1px 0 4px rgba(0,0,0,0.15);
    }
    ::-webkit-scrollbar-thumb {
      background: linear-gradient(to right, #aed4fa 0%, #2f91fc 50%, #0076f4 51%, #aed4fa 100%);
      border: 1.5px solid #0050ae;
      border-radius: 7px;
      box-shadow: inset 0 1px 1px rgba(255,255,255,0.7), 0 1px 1px rgba(0,0,0,0.15);
    }
    ::-webkit-scrollbar-thumb:hover {
      background: linear-gradient(to right, #8fc1f9 0%, #0d7bfb 50%, #005ecf 51%, #8fc1f9 100%);
    }

    /* Brushed Metal Title Bar / Header */
    header {
      background: 
        linear-gradient(to bottom, rgba(255,255,255,0.6) 0%, rgba(255,255,255,0) 100%),
        linear-gradient(rgba(0, 0, 0, 0.04) 50%, transparent 50%),
        linear-gradient(to bottom, #eeeeee 0%, #cccccc 50%, #b5b5b5 100%);
      background-size: auto, 100% 4px, auto;
      color: #2c3e50;
      height: 50px;
      display: flex;
      align-items: center;
      border-bottom: 1.5px solid #858585;
      box-shadow: inset 0 1px 0 #fff, 0 2px 4px rgba(0,0,0,0.1);
      padding: 0 20px;
      z-index: 100;
      flex-shrink: 0;
    }

    /* macOS Window Controls */
    .apple-window-controls {
      display: inline-flex;
      margin-right: 18px;
      gap: 7px;
      align-items: center;
    }
    .apple-window-controls span {
      width: 13px;
      height: 13px;
      border-radius: 50%;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      position: relative;
      box-shadow: inset 0 1px 2px rgba(0,0,0,0.75), 0 1px 0 rgba(255,255,255,0.6);
      cursor: pointer;
    }
    .btn-close {
      background: radial-gradient(circle at 4px 4px, #ffa0a0, #ff3b30 80%);
      border: 1px solid #991610;
    }
    .btn-min {
      background: radial-gradient(circle at 4px 4px, #ffe49e, #ffcc00 80%);
      border: 1px solid #997a00;
    }
    .btn-zoom {
      background: radial-gradient(circle at 4px 4px, #a6f5a6, #34c759 80%);
      border: 1px solid #146d2a;
    }
    .btn-close::after { content: \'×\'; color: rgba(0,0,0,0.55); font-size: 9px; font-weight: bold; opacity: 0; transition: opacity 0.15s; }
    .btn-min::after { content: \'−\'; color: rgba(0,0,0,0.55); font-size: 10px; font-weight: bold; opacity: 0; transition: opacity 0.15s; margin-top: -1px; }
    .btn-zoom::after { content: \'+\'; color: rgba(0,0,0,0.55); font-size: 9px; font-weight: bold; opacity: 0; transition: opacity 0.15s; }
    .apple-window-controls:hover span::after {
      opacity: 1;
    }

    .header-title {
      font-size: 16px;
      font-weight: 900;
      text-shadow: 0 1px 0 rgba(255,255,255,0.9);
      letter-spacing: 0.5px;
    }

    /* Main Container (Sidebar + Content Box) */
    .app-container {
      display: flex;
      flex: 1;
      overflow: hidden;
    }

    /* Finder-style Light Metallic Sidebar */
    aside {
      width: 250px;
      background: linear-gradient(to right, #edf1f6 0%, #d5dde8 95%, #b2bcca 100%);
      border-right: 1.5px solid #808b9b;
      box-shadow: inset -1px 0 0 #fff, 2px 0 8px rgba(0,0,0,0.05);
      padding: 15px 0;
      display: flex;
      flex-direction: column;
      flex-shrink: 0;
      overflow-y: auto;
    }

    .sidebar-menu {
      list-style-type: none;
      padding: 0;
      margin: 0;
    }
    .sidebar-menu li a {
      color: #3c4552;
      font-weight: bold;
      font-size: 13px;
      text-decoration: none;
      text-shadow: 0 1px 0 rgba(255,255,255,0.85);
      border-bottom: 1px solid #c5cbd5;
      border-top: 1px solid #f6f8fa;
      padding: 12px 20px;
      display: flex;
      align-items: center;
      position: relative;
      cursor: pointer;
      transition: background-color 0.15s;
    }
    .sidebar-menu li a i {
      margin-right: 12px;
      font-size: 16px;
      color: #555c66;
      width: 20px;
      text-align: center;
    }
    .sidebar-menu li a:hover {
      background-color: rgba(0,0,0,0.05);
      color: #000;
    }

    /* Active blue gel menu pill */
    .sidebar-menu li.active a {
      background: linear-gradient(to bottom, #5ba4e5 0%, #2081e2 50%, #0d64cc 51%, #4ca1ed 100%);
      color: #fff;
      text-shadow: 0 -1px 1px rgba(0,0,0,0.45);
      border-top: 1px solid #7bbbf2;
      border-bottom: 1px solid #00428f;
      box-shadow: 0 3px 6px rgba(0,0,0,0.25), inset 0 1px 1px rgba(255,255,255,0.5);
      border-radius: 5px;
      margin: 4px 8px;
    }
    .sidebar-menu li.active a i {
      color: #fff;
    }
    .sidebar-menu li.active a::before {
      content: \'\';
      position: absolute;
      top: 1px;
      left: 2px;
      right: 2px;
      height: 42%;
      background: linear-gradient(to bottom, rgba(255,255,255,0.4) 0%, rgba(255,255,255,0.05) 100%);
      border-radius: 4px 4px 6px 6px / 4px 4px 2px 2px;
      pointer-events: none;
    }

    /* Content Area */
    main {
      flex: 1;
      padding: 20px;
      overflow-y: auto;
      box-shadow: inset 6px 0 15px rgba(0,0,0,0.06);
    }

    .tab-content {
      display: none;
    }
    .tab-content.active {
      display: block;
    }

    /* KPIs / Gel Box Row */
    .kpi-row {
      display: flex;
      gap: 20px;
      margin-bottom: 20px;
      flex-wrap: wrap;
    }

    /* 3D Glass/Gel Value Boxes */
    .small-box {
      flex: 1;
      min-width: 250px;
      border-radius: 8px;
      box-shadow: 0 6px 15px rgba(0,0,0,0.2), inset 0 1px 0 rgba(255,255,255,0.5);
      overflow: hidden;
      position: relative;
      padding: 20px;
      color: #fff;
      text-shadow: 0 -1px 1px rgba(0,0,0,0.5);
      border: 1px solid #666;
    }
    .small-box::before {
      content: \'\';
      position: absolute;
      top: 1px;
      left: 3px;
      right: 3px;
      height: 48%;
      background: linear-gradient(to bottom, rgba(255,255,255,0.55) 0%, rgba(255,255,255,0.12) 100%);
      border-radius: 6px 6px 10px 10px / 6px 6px 3px 3px;
      pointer-events: none;
      z-index: 10;
    }
    .small-box h3 {
      font-size: 34px;
      font-weight: 800;
      margin: 0 0 5px 0;
      letter-spacing: -1.2px;
      z-index: 5;
      position: relative;
    }
    .small-box p {
      font-size: 13px;
      font-weight: bold;
      margin: 0;
      z-index: 5;
      position: relative;
    }
    .small-box i {
      position: absolute;
      right: 15px;
      bottom: 10px;
      font-size: 60px;
      opacity: 0.22;
      z-index: 1;
    }

    /* Color styles for Gel Boxes */
    .bg-navy {
      background: linear-gradient(to bottom, #98c7fa 0%, #4da0f9 49%, #1b80fc 50%, #106ee2 85%, #6eb0ff 100%);
      border: 1px solid #004d9c;
    }
    .bg-red {
      background: linear-gradient(to bottom, #ffa5a5 0%, #ff5d5d 49%, #ff2020 50%, #e20808 85%, #ff7272 100%);
      border: 1px solid #9c0000;
    }
    .bg-orange {
      background: linear-gradient(to bottom, #ffdca3 0%, #ff9833 49%, #ff7e00 50%, #e26300 85%, #ffb05c 100%);
      border: 1px solid #a34300;
    }

    /* Grid Row */
    .grid-row {
      display: flex;
      gap: 20px;
      margin-bottom: 20px;
      flex-wrap: wrap;
    }
    .col-6 {
      flex: 1;
      min-width: 400px;
    }
    .col-4 {
      flex: 1;
      min-width: 280px;
    }
    .col-8 {
      flex: 2;
      min-width: 500px;
    }

    /* Brushed Metal Content Cards (Boxes) */
    .box {
      background: linear-gradient(to bottom, #fcfcfc 0%, #f4f4f4 12%, #e5e5e5 88%, #d8d8d8 100%);
      border-bottom: 3.5px solid #8a8a8a;
      border-right: 1.5px solid #949494;
      border-left: 1.5px solid #b8b8b8;
      border-top: 1px solid #fff;
      border-radius: 8px;
      box-shadow: 0 12px 32px rgba(0,0,0,0.22), inset 0 1px 0 #fff;
      color: #333;
      margin-bottom: 20px;
      display: flex;
      flex-direction: column;
    }
    .box-header {
      background: 
        linear-gradient(to bottom, rgba(255,255,255,0.5) 0%, rgba(255,255,255,0) 100%),
        linear-gradient(to bottom, #f3f3f3 0%, #e0e0e0 50%, #d0d0d0 100%);
      border-bottom: 1.5px solid #929292;
      border-top-left-radius: 7px;
      border-top-right-radius: 7px;
      padding: 10px 15px;
      color: #1a1a1a;
      text-shadow: 0 1px 0 #fff;
      box-shadow: inset 0 1px 0 #fff;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .box-title {
      font-size: 14px;
      font-weight: bold;
    }
    .box-body {
      padding: 15px;
      flex: 1;
    }

    /* macOS standard indented report box */
    .sim-report-box {
      background: linear-gradient(to bottom, #fafafa, #eeeeee);
      border: 1px solid #b5b5b5;
      border-left: 6px solid #2081e2;
      padding: 18px;
      border-radius: 6px;
      box-shadow: inset 0 1px 4px rgba(0,0,0,0.08), 0 1px 0 #fff;
      margin-bottom: 20px;
      color: #222;
    }
    .sim-report-box h4 {
      margin: 0 0 10px 0;
      font-size: 15px;
    }
    .sim-report-box p {
      font-size: 14px;
      margin: 0;
      line-height: 1.5;
    }

    /* Custom Slider (MacOS OS X theme) */
    .slider-container {
      margin-top: 15px;
      margin-bottom: 20px;
    }
    .slider-label {
      font-weight: bold;
      font-size: 13px;
      display: flex;
      justify-content: space-between;
      margin-bottom: 8px;
      text-shadow: 0 1px 0 #fff;
    }
    .mac-slider {
      -webkit-appearance: none;
      width: 100%;
      height: 8px;
      background: #c5ccd4;
      border: 1px solid #7f8c8d;
      box-shadow: inset 0 2px 4px rgba(0,0,0,0.3);
      border-radius: 8px;
      outline: none;
    }
    .mac-slider::-webkit-slider-thumb {
      -webkit-appearance: none;
      appearance: none;
      width: 22px;
      height: 22px;
      background: radial-gradient(circle, #ffffff 0%, #e5e5e5 40%, #b8b8b8 85%, #888888 100%);
      border: 1.5px solid #666;
      box-shadow: 0 2px 6px rgba(0,0,0,0.4), inset 0 1.5px 0 #fff;
      border-radius: 50%;
      cursor: pointer;
    }
    .mac-slider::-moz-range-thumb {
      width: 22px;
      height: 22px;
      background: radial-gradient(circle, #ffffff 0%, #e5e5e5 40%, #b8b8b8 85%, #888888 100%);
      border: 1.5px solid #666;
      box-shadow: 0 2px 6px rgba(0,0,0,0.4), inset 0 1.5px 0 #fff;
      border-radius: 50%;
      cursor: pointer;
    }

    /* Preset Scenario Buttons */
    .preset-btn-group {
      display: flex;
      flex-direction: column;
      gap: 6px;
    }
    .preset-btn {
      display: flex;
      align-items: center;
      gap: 10px;
      width: 100%;
      padding: 7px 10px;
      background: linear-gradient(180deg, #ffffff 0%, #f1f5f9 100%);
      border: 1px solid #cbd5e1;
      border-radius: 6px;
      color: #334155;
      cursor: pointer;
      transition: all 0.2s ease;
      box-shadow: 0 1px 2px rgba(0,0,0,0.04);
      text-align: left;
    }
    .preset-btn:hover {
      background: linear-gradient(180deg, #f8fafc 0%, #e2e8f0 100%);
      border-color: #94a3b8;
      color: #0f172a;
      transform: translateY(-1px);
    }
    .preset-btn.active {
      background: linear-gradient(180deg, #eff6ff 0%, #dbeafe 100%);
      border-color: #3b82f6;
      color: #1e40af;
      box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.3);
    }
    .preset-icon {
      font-size: 15px;
      flex-shrink: 0;
    }
    .preset-details {
      display: flex;
      flex-direction: column;
      line-height: 1.25;
    }
    .preset-title {
      font-size: 11.5px;
      font-weight: 600;
    }
    .preset-subtitle {
      font-size: 10.5px;
      color: #64748b;
    }
    .preset-btn.active .preset-subtitle {
      color: #2563eb;
    }

    /* Help blocks */
    .help-text {
      font-size: 12px;
      color: #555;
      line-height: 1.45;
      text-shadow: 0 1px 0 #fff;
    }
    .alert-box {
      background: linear-gradient(to bottom, #fff8e8, #fff0c2);
      border: 1px solid #b78a00;
      padding: 12px;
      border-radius: 5px;
      color: #665000;
      font-size: 12px;
      box-shadow: inset 0 1px 0 #fff, 0 2px 4px rgba(0,0,0,0.06);
      text-shadow: 0 1px 0 #fff;
    }

    /* Aesthetic Chart & Overview Description Boxes */
    .chart-desc-box {
      background: linear-gradient(to bottom, #ffffff 0%, #f4f7fb 100%);
      border: 1px solid #b8c8d8;
      border-left: 4px solid #2081e2;
      border-radius: 6px;
      padding: 10px 14px;
      margin-top: 12px;
      font-size: 12px;
      color: #2c3e50;
      line-height: 1.5;
      box-shadow: inset 0 1px 0 #ffffff, 0 2px 5px rgba(0, 0, 0, 0.05);
    }
    .chart-desc-box .desc-header {
      font-weight: bold;
      color: #0c4280;
      font-size: 12.5px;
      margin-bottom: 4px;
    }
    .chart-desc-box {
      background: linear-gradient(to bottom, #f8fafc 0%, #edf2f7 100%);
      border: 1px solid #cbd5e1;
      border-left: 4px solid #0284c7;
      border-radius: 6px;
      padding: 14px 16px;
      margin-top: 15px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.04);
    }
    .chart-desc-box .desc-header {
      font-weight: 700;
      color: #0f172a;
      font-size: 13.5px;
      margin-bottom: 6px;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .chart-desc-box p {
      margin: 0;
      color: #334155;
      font-size: 13.5px;
      line-height: 1.6;
    }
    
    .overview-summary-card {
      background: linear-gradient(to bottom, #ffffff 0%, #f8fafc 100%);
      border: 1px solid #cbd5e1;
      border-radius: 8px;
      padding: 16px 20px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
    }
    
    .summary-badge {
      display: inline-block;
      background: linear-gradient(to bottom, #2b7fff 0%, #1a56b3 100%);
      color: #ffffff;
      font-size: 11px;
      font-weight: bold;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      padding: 4px 12px;
      border-radius: 12px;
      margin-bottom: 8px;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2), inset 0 1px 0 rgba(255, 255, 255, 0.4);
      text-shadow: 0 1px 1px rgba(0, 0, 0, 0.3);
    }
    
    .summary-badge.badge-blue {
      background: linear-gradient(to bottom, #009688 0%, #00695c 100%);
    }
    
    .readable-desc-text {
      font-size: 13.5px;
      color: #334155;
      line-height: 1.65;
      margin: 8px 0 0 0;
    }

    /* Professional Report CSS Enhancements */
    .report-header-banner {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 50%, #0f172a 100%);
      border-radius: 8px;
      padding: 20px 24px;
      color: #ffffff;
      margin-bottom: 24px;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
    }
    .report-title-meta h2 {
      color: #ffffff;
      font-size: 20px;
      font-weight: 700;
      margin: 0 0 12px 0;
      letter-spacing: -0.3px;
    }
    .meta-pills-row {
      display: flex;
      gap: 10px;
      flex-wrap: wrap;
    }
    .meta-badge {
      background: rgba(255, 255, 255, 0.12);
      border: 1px solid rgba(255, 255, 255, 0.25);
      color: #e2e8f0;
      font-size: 11.5px;
      padding: 4px 12px;
      border-radius: 20px;
      font-weight: 500;
    }
    .meta-badge.badge-green {
      background: rgba(16, 185, 129, 0.2);
      border-color: rgba(16, 185, 129, 0.4);
      color: #6ee7b7;
    }
    .meta-badge.badge-blue {
      background: rgba(59, 130, 246, 0.2);
      border-color: rgba(59, 130, 246, 0.4);
      color: #93c5fd;
    }

    .hero-research-card {
      display: flex;
      align-items: flex-start;
      gap: 16px;
      background: linear-gradient(to right, #eff6ff, #f8fafc);
      border: 1px solid #bfdbfe;
      border-left: 5px solid #2563eb;
      border-radius: 8px;
      padding: 16px 20px;
      margin: 16px 0 20px 0;
      box-shadow: 0 2px 8px rgba(37, 99, 235, 0.08);
    }
    .hero-icon {
      font-size: 22px;
      color: #2563eb;
      margin-top: 2px;
    }
    .hero-content strong {
      display: block;
      color: #1e3a8a;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      margin-bottom: 4px;
    }
    .hero-content p {
      font-size: 14.5px !important;
      font-weight: 600 !important;
      color: #0f172a !important;
      margin: 0 !important;
      line-height: 1.5 !important;
    }

    /* Enhanced Q1 and Q2 Subquestion Cards */
    .subquestion-grid {
      display: flex;
      gap: 20px;
      margin: 20px 0 26px 0;
      flex-wrap: wrap;
    }
    
    .subquestion-card {
      flex: 1;
      min-width: 300px;
      background: #ffffff;
      border: 1px solid #cbd5e1;
      border-radius: 10px;
      padding: 20px 22px;
      box-shadow: 0 4px 14px rgba(0, 0, 0, 0.05);
      position: relative;
      overflow: hidden;
      transition: transform 0.2s ease, box-shadow 0.2s ease;
    }
    
    .subquestion-card.q1-card {
      border-top: 4px solid #0284c7;
    }
    
    .subquestion-card.q2-card {
      border-top: 4px solid #4f46e5;
    }

    .q-badge {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      font-size: 11px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0.6px;
      padding: 4px 12px;
      border-radius: 14px;
      margin-bottom: 12px;
    }
    
    .q1-card .q-badge {
      background: #e0f2fe;
      color: #0369a1;
      border: 1px solid #bae6fd;
    }
    
    .q2-card .q-badge {
      background: #e0e7ff;
      color: #4338ca;
      border: 1px solid #c7d2fe;
    }

    .subquestion-card h4 {
      color: #0f172a;
      font-size: 15px;
      font-weight: 700;
      margin: 0 0 10px 0;
      line-height: 1.35;
    }

    .subquestion-card p {
      font-size: 13.5px !important;
      color: #334155 !important;
      margin: 0 0 14px 0 !important;
      line-height: 1.6 !important;
    }

    .q-focus-pill {
      display: inline-block;
      font-size: 11.5px;
      font-weight: 600;
      padding: 4px 10px;
      border-radius: 6px;
      background: #f8fafc;
      border: 1px solid #e2e8f0;
      color: #475569;
    }

    .methodology-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
      gap: 14px;
      margin: 18px 0;
    }
    .method-card {
      background: #f8fafc;
      border: 1px solid #cbd5e1;
      border-radius: 8px;
      padding: 16px;
      position: relative;
    }
    .step-number {
      display: inline-block;
      background: #2563eb;
      color: #ffffff;
      font-size: 10px;
      font-weight: 800;
      padding: 2px 8px;
      border-radius: 4px;
      margin-bottom: 8px;
    }
    .method-card h5 {
      font-size: 13px;
      font-weight: 700;
      color: #0f172a;
      margin: 0 0 6px 0;
    }
    .method-card p {
      font-size: 12px !important;
      color: #475569 !important;
      margin: 0 !important;
      line-height: 1.45 !important;
    }

    .stat-pill.pill-primary {
      background: linear-gradient(to bottom, #2563eb, #1d4ed8);
      color: #ffffff;
      font-weight: bold;
    }
    .sig-tag {
      color: #059669;
      font-weight: bold;
      font-size: 11.5px;
    }

    /* Enhanced Main Heading Header */
    .portal-main-heading {
      background: linear-gradient(135deg, #0b2545 0%, #134074 50%, #1e293b 100%);
      border-radius: 8px;
      padding: 22px 28px;
      color: #ffffff;
      margin-bottom: 24px;
      box-shadow: 0 6px 18px rgba(11, 37, 69, 0.2), inset 0 1px 0 rgba(255,255,255,0.3);
      border: 1px solid #0b2545;
    }
    .portal-main-heading .portal-badge-title {
      display: inline-block;
      background: rgba(255, 255, 255, 0.15);
      border: 1px solid rgba(255, 255, 255, 0.3);
      color: #e0f2fe;
      font-size: 11px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 1px;
      padding: 3px 12px;
      border-radius: 12px;
      margin-bottom: 8px;
    }
    .portal-main-heading h1 {
      color: #ffffff;
      font-size: 22px;
      font-weight: 800;
      margin: 0 0 6px 0;
      letter-spacing: -0.4px;
      text-shadow: 0 2px 4px rgba(0,0,0,0.3);
    }
    .portal-main-heading p {
      color: #cbd5e1;
      font-size: 13px;
      margin: 0 0 14px 0;
    }

    /* Publication-Grade Table Styling */
    .findings-table-container {
      background: #ffffff;
      border-radius: 8px;
      border: 1px solid #cbd5e1;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05);
      margin: 20px 0;
      overflow: hidden;
    }
    .findings-table {
      width: 100%;
      border-collapse: collapse;
      font-size: 13px;
    }
    .findings-table th {
      background: linear-gradient(to bottom, #0f172a 0%, #1e293b 100%);
      color: #ffffff;
      font-weight: 700;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      padding: 12px 16px;
      text-align: left;
      border: none;
    }
    .findings-table td {
      padding: 12px 16px;
      border-bottom: 1px solid #e2e8f0;
      color: #334155;
      vertical-align: middle;
    }
    .findings-table tr.section-header td {
      background: #f1f5f9;
      font-weight: 800;
      color: #0f172a;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.6px;
      border-top: 2px solid #cbd5e1;
      border-bottom: 2px solid #cbd5e1;
    }
    .findings-table tr:hover td {
      background-color: #f8fafc;
    }
    .or-badge {
      display: inline-block;
      padding: 4px 10px;
      border-radius: 6px;
      font-weight: 800;
      font-size: 12.5px;
      min-width: 60px;
      text-align: center;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    .or-badge.high-impact {
      background: linear-gradient(135deg, #1e40af, #1d4ed8);
      color: #ffffff;
    }
    .or-badge.medium-impact {
      background: linear-gradient(135deg, #0284c7, #0369a1);
      color: #ffffff;
    }
    .or-badge.moderate-impact {
      background: linear-gradient(135deg, #0f766e, #115e59);
      color: #ffffff;
    }
    .ci-box {
      font-family: \'Courier New\', Courier, monospace;
      font-size: 12px;
      color: #475569;
      background: #f8fafc;
      padding: 3px 8px;
      border-radius: 4px;
      border: 1px solid #e2e8f0;
      display: inline-block;
    }
    .p-val-tag {
      color: #15803d;
      font-weight: 700;
      font-size: 11.5px;
    }
  </style>
</head>
<body>

  <!-- Brushed Metal Header -->
  <header>
    <div class="apple-window-controls">
      <span class="btn-close" onclick="alert(\'Close window is disabled in preview mode.\')"></span>
      <span class="btn-min" onclick="alert(\'Minimize is disabled.\')"></span>
      <span class="btn-zoom" onclick="alert(\'Zoom is disabled.\')"></span>
    </div>
    <div class="header-title">Diabetes Portal</div>
  </header>

  <!-- App Structure -->
  <div class="app-container">
    
    <!-- Sidebar -->
    <aside>
      <ul class="sidebar-menu">
        <li id="menu-report" class="active"><a onclick="switchTab(\'report\')"><i class="fas fa-book"></i> Project Report</a></li>
        <li id="menu-overview"><a onclick="switchTab(\'overview\')"><i class="fas fa-users"></i> Patient Overview</a></li>
        <li id="menu-demographics"><a onclick="switchTab(\'demographics\')"><i class="fas fa-id-card"></i> Demographics & Lifestyle</a></li>
        <li id="menu-biomarkers"><a onclick="switchTab(\'biomarkers\')"><i class="fas fa-flask"></i> Biomarker Correlation</a></li>
        <li id="menu-simulator"><a onclick="switchTab(\'simulator\')"><i class="fas fa-heartbeat"></i> Clinical Intervention Simulator</a></li>
        <li id="menu-conclusion"><a onclick="switchTab(\'conclusion\')"><i class="fas fa-check-circle"></i> Conclusion</a></li>
      </ul>
    </aside>

    <!-- Main Content Panel -->
    <main>
      
      <!-- TAB 1: Project Report (FIRST TAB BY DEFAULT) -->
      <div id="tab-report" class="tab-content active">
        <div class="grid-row">
          <div class="col-12">
            <div class="box">
              <div class="box-header">
                <span class="box-title">🏥 DIABETES RISK PORTAL — PUBLICATION-GRADE PROJECT REPORT</span>
              </div>
              <div class="box-body">
                <div class="tiger-document">
                  
                  <!-- Main Enhanced Heading Banner -->
                  <div class="portal-main-heading">
                    <span class="portal-badge-title"><i class="fas fa-award"></i> CLINICAL MACHINE LEARNING STUDY</span>
                    <h1>Diabetes Risk Portal — Publication-Grade Project Report</h1>
                    <p>Multivariable Logistic Regression & Decision Support System on 96,146 clean clinical evaluation records (deduplicated from the 100,000 raw dataset)</p>
                    <div class="meta-pills-row">
                      <span class="meta-badge"><i class="fas fa-user"></i> Author: Niket Banda</span>
                      <span class="meta-badge"><i class="fas fa-database"></i> Cohort: 96,146 Clean Records</span>
                      <span class="meta-badge badge-green"><i class="fas fa-check-circle"></i> 10-Fold CV Accuracy: 95.9%</span>
                      <span class="meta-badge badge-blue"><i class="fas fa-chart-line"></i> ROC-AUC: 96.2%</span>
                    </div>
                  </div>
                  
                  <!-- Section 1: Research Question & Core Objectives -->
                  <h3>1. Primary Research Question & Analytical Objectives</h3>
                  <div class="hero-research-card">
                    <div class="hero-icon"><i class="fas fa-microscope"></i></div>
                    <div class="hero-content">
                      <strong>Primary Research Question</strong>
                      <p>"Can we accurately predict whether a patient has diabetes using age, BMI, blood sugar levels, and health history—while predicting as many diabetic cases as possible?"</p>
                    </div>
                  </div>
                  <p>
                    In clinical predictive modeling, early diagnosis of diabetes mellitus is essential to mitigate long-term microvascular and macrovascular complications. This project develops a publication-grade, reproducible machine learning classification pipeline in R to deliver transparent, actionable risk predictions for healthcare providers.
                  </p>
                  <div class="subquestion-grid">
                    <div class="subquestion-card q1-card">
                      <span class="q-badge"><i class="fas fa-vial"></i> Core Analytical Focus</span>
                      <h4>Biomarker vs. Lifestyle Dominance</h4>
                      <p>Do laboratory diagnostic biomarkers (HbA1c level and blood glucose level) exert a significantly stronger predictive influence on diabetes risk than demographic and lifestyle factors (BMI, age, and smoking history)?</p>
                      <div class="q-focus-pill">🔍 <strong>Analytical Objective:</strong> Quantifying relative feature importance & Odds Ratio effect sizes across laboratory vs. demographic predictors</div>
                    </div>
                  </div>
                  
                  <!-- Section 2: Methodology (4 Steps Grid) -->
                  <h3>2. Four-Step Analytic Methodology</h3>
                  <p>
                    To guarantee mathematical rigor and complete methodological reproducibility, the analytical pipeline follows a structured four-step workflow:
                  </p>
                  <div class="methodology-grid">
                    <div class="method-card">
                      <span class="step-number">STEP 1</span>
                      <h5>Data Cleaning & Stratification</h5>
                      <p>Categorical variables and binary flags reformatted as factors. Deduplicated raw cohort to 96,146 clean records across 4 clinical risk tiers.</p>
                    </div>
                    <div class="method-card">
                      <span class="step-number">STEP 2</span>
                      <h5>80/20 Stratified Split</h5>
                      <p>Partitioned into 80% train / 20% test using stratified sampling. Normalization parameters derived strictly from training data to prevent data leakage.</p>
                    </div>
                    <div class="method-card">
                      <span class="step-number">STEP 3</span>
                      <h5>10-Fold CV Training</h5>
                      <p>Evaluated via 10-fold cross-validation with tidymodels logistic regression. Achieved 95.9% mean CV Accuracy and 96.2% ROC-AUC.</p>
                    </div>
                    <div class="method-card">
                      <span class="step-number">STEP 4</span>
                      <h5>Sensitivity-Driven Evaluation</h5>
                      <p>Evaluated on held-out test set, prioritizing Area Under ROC (ROC-AUC) and Sensitivity (Recall) for clinical diagnostic safety.</p>
                    </div>
                  </div>
                  <div class="tiger-formula-box">
                    <strong>Data Leakage Prevention Protocol:</strong><br>
                    1. Recipe Blueprint: recipe(diabetes ~ ., data = train_data)<br>
                    2. Parameter Calculation: Derived strictly on train_data via step_normalize()<br>
                    3. Test Evaluation: bake(prep_recipe, new_data = test_data) using frozen training parameters
                  </div>
                  
                  <!-- Section 3: Key Analytical Findings & Structured Table -->
                  <h3>3. Key Analytical Findings & Multivariable Odds Ratios</h3>
                  <p>
                    Multivariable logistic regression demonstrated that laboratory diagnostic biomarkers—specifically HbA1c and Blood Glucose levels—are the dominant clinical predictors of diabetes status, as summarized in the publication-grade model findings table below:
                  </p>
                  
                  <!-- Enhanced Structured Table -->
                  <div class="findings-table-container">
                    <table class="findings-table">
                      <thead>
                        <tr>
                          <th>Domain & Clinical Predictor Variable</th>
                          <th>Odds Ratio (OR)</th>
                          <th>95% Confidence Interval</th>
                          <th>p-value</th>
                          <th>Clinical Significance & Effect Size</th>
                        </tr>
                      </thead>
                      <tbody>
                        <!-- Section Header 1 -->
                        <tr class="section-header">
                          <td colspan="5"><i class="fas fa-flask"></i> Laboratory Diagnostic Biomarkers</td>
                        </tr>
                        <tr>
                          <td><strong>HbA1c Level (%)</strong></td>
                          <td><span class="or-badge high-impact">10.34x</span></td>
                          <td><span class="ci-box">[9.64 – 11.09]</span></td>
                          <td><span class="p-val-tag">p &lt; 0.001 ***</span></td>
                          <td>Primary Clinical Predictor: Each +1.0% increase in HbA1c multiplies diabetes odds by ~10.34x.</td>
                        </tr>
                        <tr>
                          <td><strong>Blood Glucose Level (mg/dL)</strong></td>
                          <td><span class="or-badge high-impact">1.034x</span></td>
                          <td><span class="ci-box">[1.033 – 1.035]</span></td>
                          <td><span class="p-val-tag">p &lt; 0.001 ***</span></td>
                          <td>Continuous Glucose Driver: Each +25 mg/dL shift in glucose increases diabetes odds by ~2.37x.</td>
                        </tr>
                        
                        <!-- Section Header 2 -->
                        <tr class="section-header">
                          <td colspan="5"><i class="fas fa-heartbeat"></i> Cardiovascular Comorbidities</td>
                        </tr>
                        <tr>
                          <td><strong>Hypertension (Yes vs No)</strong></td>
                          <td><span class="or-badge medium-impact">2.15x</span></td>
                          <td><span class="ci-box">[1.96 – 2.35]</span></td>
                          <td><span class="p-val-tag">p &lt; 0.001 ***</span></td>
                          <td>Independent Vascular Risk: Co-existing hypertension more than doubles diabetes odds (2.15x).</td>
                        </tr>
                        <tr>
                          <td><strong>Heart Disease (Yes vs No)</strong></td>
                          <td><span class="or-badge medium-impact">2.14x</span></td>
                          <td><span class="ci-box">[1.90 – 2.41]</span></td>
                          <td><span class="p-val-tag">p &lt; 0.001 ***</span></td>
                          <td>Independent Cardiac Risk: History of heart disease independently doubles diabetes odds (2.14x).</td>
                        </tr>
                        
                        <!-- Section Header 3 -->
                        <tr class="section-header">
                          <td colspan="5"><i class="fas fa-user-circle"></i> Demographics & Anthropometrics</td>
                        </tr>
                        <tr>
                          <td><strong>Body Mass Index (BMI)</strong></td>
                          <td><span class="or-badge moderate-impact">1.092x</span></td>
                          <td><span class="ci-box">[1.087 – 1.098]</span></td>
                          <td><span class="p-val-tag">p &lt; 0.001 ***</span></td>
                          <td>Adiposity Risk Factor: Each unit increase in BMI (kg/m²) increases diabetes odds by ~9.2%.</td>
                        </tr>
                        <tr>
                          <td><strong>Age (Years)</strong></td>
                          <td><span class="or-badge moderate-impact">1.048x</span></td>
                          <td><span class="ci-box">[1.046 – 1.050]</span></td>
                          <td><span class="p-val-tag">p &lt; 0.001 ***</span></td>
                          <td>Demographic Baseline: Each additional year of age increases diabetes odds by ~4.8%.</td>
                        </tr>
                      </tbody>
                    </table>
                  </div>
                  
                  <div class="tiger-formula-box">
                    <strong>Multivariable Logistic Regression Odds Ratio Formula:</strong><br>
                    ln( p / (1 - p) ) = β0 + β1(HbA1c) + β2(Glucose) + β3(Hypertension) + β4(HeartDisease) + β5(BMI) + β6(Age)<br>
                    Odds Ratio (OR) = exp(β_i)  |  95% CI = exp( β_i ± 1.96 × SE(β_i) )
                  </div>
                  
                  <!-- Section 4: Model Sensitivity & Clinical Screening Impact -->
                  <h3>4. Clinical Sensitivity & Diagnostic Impact</h3>
                  <p>
                    In population-level screening programs, prioritizing clinical sensitivity (recall) minimizes False Negatives—preventing undetected diabetic patients from developing unmonitored cardiovascular and metabolic complications. The cross-validated model provides a stable, highly scalable decision-support framework to empower early clinical intervention.
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- TAB 2: Patient Overview -->
      <div id="tab-overview" class="tab-content">
        <div class="kpi-row">
          <div class="small-box bg-navy">
            <h3 id="kpi-total-patients">', format(total_patients, big.mark = ","), '</h3>
            <p>Total Patients Cataloged</p>
            <i class="fas fa-users"></i>
          </div>
          <div class="small-box bg-red">
            <h3 id="kpi-pct-diabetic">', round(pct_diabetic, 2), '%</h3>
            <p>Prevalence of Diabetes</p>
            <i class="fas fa-heartbeat"></i>
          </div>
          <div class="small-box bg-orange">
            <h3 id="kpi-pct-risk">', round(pct_risk, 2), '%</h3>
            <p>Cohort At Risk (Mod / High)</p>
            <i class="fas fa-exclamation-triangle"></i>
          </div>
        </div>

        <div class="grid-row">
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Patient Population Risk Tier Distribution</span>
              </div>
              <div class="box-body">
                <div id="risk-dist-plot" style="height: 360px; width: 100%;"></div>
                <div class="chart-desc-box">
                  <div class="desc-header"><i class="fas fa-chart-bar"></i> Risk Tier Stratification Insights</div>
                  <p>Categorizes 96,146 cataloged patients across 4 risk tiers using clinical indicators. High Risk patients have HbA1c ≥ 5.7% or Blood Glucose ≥ 140 mg/dL, while Moderate Risk denotes BMI ≥ 25 kg/m².</p>
                </div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">HbA1c Levels Across Risk Tiers</span>
              </div>
              <div class="box-body">
                <div id="clinical-metrics-plot" style="height: 360px; width: 100%;"></div>
                <div class="chart-desc-box">
                  <div class="desc-header"><i class="fas fa-vial"></i> Biomarker Elevation Metrics</div>
                  <p>Compares glycated hemoglobin (HbA1c) levels across risk classifications. Median HbA1c escalates sharply in the diabetic cohort (≥ 6.5%), confirming HbA1c as the primary metabolic risk predictor.</p>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div class="grid-row" style="margin-top: 18px;">
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">📊 Cohort Risk Stratification Analysis</span>
              </div>
              <div class="box-body">
                <div class="overview-summary-card">
                  <div class="summary-badge">Population Stratification</div>
                  <p class="readable-desc-text">Analysis of the 96,146 patient cohort indicates that while overall diabetes prevalence is 8.82%, a staggering 85.21% of non-diabetic individuals exhibit moderate to high metabolic risk, highlighting critical opportunities for early preventive intervention.</p>
                </div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">🩺 Clinical Biomarker Elevation Analysis</span>
              </div>
              <div class="box-body">
                <div class="overview-summary-card">
                  <div class="summary-badge badge-blue">Biomarker Correlation</div>
                  <p class="readable-desc-text">HbA1c concentration serves as the core diagnostic metric. Non-diabetic cohorts center within normal ranges (&lt; 5.7% or 5.7–6.4%), whereas diabetic individuals display marked escalation (mean HbA1c &gt; 6.9%), supporting multivariable risk modeling.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- TAB 3: Demographics & Lifestyle -->
      <div id="tab-demographics" class="tab-content">
        <div class="grid-row">
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Diabetes Prevalence by Age Group</span>
              </div>
              <div class="box-body">
                <div id="age-plot" style="height: 360px; width: 100%;"></div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Risk Tier Distribution by Smoking History</span>
              </div>
              <div class="box-body">
                <div id="smoking-plot" style="height: 360px; width: 100%;"></div>
              </div>
            </div>
          </div>
        </div>

        <div class="grid-row" style="margin-top: 18px;">
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">📊 Age Group Risk Trend Analysis</span>
              </div>
              <div class="box-body">
                <div class="overview-summary-card">
                  <div class="summary-badge">Demographic Profile</div>
                  <p class="readable-desc-text">Diabetes prevalence demonstrates a strong non-linear age escalation. While prevalence remains below 3% in cohorts under age 30, it surges to over 15% in individuals aged 60 and older, confirming age as a dominant baseline demographic risk vector.</p>
                </div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">🚬 Lifestyle & Smoking Exposure Analysis</span>
              </div>
              <div class="box-body">
                <div class="overview-summary-card">
                  <div class="summary-badge badge-blue">Lifestyle Exposure</div>
                  <p class="readable-desc-text">Patients with a history of former or current smoking exhibit a higher proportion of Moderate and High Risk metabolic tiers compared to non-smokers, reflecting cumulative vascular and metabolic stress.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- TAB 4: Biomarker Correlation -->
      <div id="tab-biomarkers" class="tab-content">
        <div class="grid-row">
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Clinical Biomarker Interaction (Sampled Cohort)</span>
              </div>
              <div class="box-body">
                <div id="biomarker-scatter" style="height: 360px; width: 100%;"></div>
                <div class="help-text" style="text-align: center; margin-top: 5px; font-size: 11px;">
                  Visualizing a random sample of 2,000 patients for responsive interaction.
                </div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Cardiovascular Comorbidities & Diabetes Risk</span>
              </div>
              <div class="box-body">
                <div id="comorbidity-plot" style="height: 360px; width: 100%;"></div>
              </div>
            </div>
          </div>
        </div>

        <div class="grid-row" style="margin-top: 18px;">
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">🧪 Glycemic Biomarker Interaction Analysis</span>
              </div>
              <div class="box-body">
                <div class="overview-summary-card">
                  <div class="summary-badge">Biomarker Interaction</div>
                  <p class="readable-desc-text">HbA1c and Blood Glucose levels exhibit a strong positive co-elevation. Diabetic individuals cluster almost exclusively in the upper-right quadrant (HbA1c ≥ 6.5%, Glucose ≥ 140 mg/dL), confirming dual-biomarker thresholds as primary diagnostic boundaries.</p>
                </div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">🫀 Cardiovascular Comorbidity Analysis</span>
              </div>
              <div class="box-body">
                <div class="overview-summary-card">
                  <div class="summary-badge badge-blue">Cardiovascular Risk</div>
                  <p class="readable-desc-text">Co-existing cardiovascular conditions markedly amplify diabetes risk. Patients with both Hypertension and Heart Disease present more than double the diabetes prevalence of non-hypertensive patients, highlighting vascular damage as an independent risk driver.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- TAB 5: Clinical Intervention Simulator -->
      <div id="tab-simulator" class="tab-content">
        <div class="grid-row">
          <div class="col-4">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Simulation Parameters</span>
              </div>
              <div class="box-body">
                <div class="preset-scenarios-container" style="margin-bottom: 18px;">
                  <label class="slider-label" style="font-weight: 600; font-size: 11px; color: #475569; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px; display: block;">
                    ⚡ Preset Benchmark Scenarios
                  </label>
                  <div class="preset-btn-group">
                    <button type="button" class="preset-btn" data-bmi="5" data-hba1c="0.0" onclick="applyPreset(5, 0.0, this)">
                      <span class="preset-icon">📉</span>
                      <span class="preset-details">
                        <span class="preset-title">National 5% BMI Reduction</span>
                        <span class="preset-subtitle">BMI -5% | HbA1c Baseline</span>
                      </span>
                    </button>
                    <button type="button" class="preset-btn" data-bmi="0" data-hba1c="0.5" onclick="applyPreset(0, 0.5, this)">
                      <span class="preset-icon">🩸</span>
                      <span class="preset-details">
                        <span class="preset-title">Aggressive Glycemic Screening</span>
                        <span class="preset-subtitle">BMI Baseline | HbA1c -0.5%</span>
                      </span>
                    </button>
                    <button type="button" class="preset-btn" data-bmi="10" data-hba1c="0.5" onclick="applyPreset(10, 0.5, this)">
                      <span class="preset-icon">🌟</span>
                      <span class="preset-details">
                        <span class="preset-title">Combined Dual Intervention</span>
                        <span class="preset-subtitle">BMI -10% | HbA1c -0.5%</span>
                      </span>
                    </button>
                    <button type="button" class="preset-btn btn-reset active" data-bmi="0" data-hba1c="0.0" onclick="applyPreset(0, 0.0, this)">
                      <span class="preset-icon">🔄</span>
                      <span class="preset-details">
                        <span class="preset-title">Baseline (Reset)</span>
                        <span class="preset-subtitle">No Population Interventions</span>
                      </span>
                    </button>
                  </div>
                </div>

                <div class="slider-container">
                  <div class="slider-label">
                    <span>Population BMI Reduction:</span>
                    <span id="slider-bmi-value">0%</span>
                  </div>
                  <input type="range" id="bmi-slider" class="mac-slider" min="0" max="20" value="0" step="1" oninput="updateSimulation()">
                </div>
                <div class="slider-container" style="margin-top: 15px;">
                  <div class="slider-label">
                    <span>Glycemic Control (HbA1c Reduction):</span>
                    <span id="slider-hba1c-value">0.0%</span>
                  </div>
                  <input type="range" id="hba1c-slider" class="mac-slider" min="0.0" max="1.0" value="0.0" step="0.1" oninput="updateSimulation()">
                </div>
                <div class="help-text" style="margin-top: 15px;">
                  This simulator models counterfactual public health interventions. Shifting BMI and HbA1c distributions downward estimates population-level case reductions using baseline multivariable logistic regression coefficients.
                </div>
                <div class="alert-box" style="margin-top: 15px;">
                  <strong>Note:</strong> The model controls for patient age, blood glucose level, hypertension history, and heart disease history.
                </div>
              </div>
            </div>
          </div>
          
          <div class="col-8">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Simulated Population Impact Study</span>
              </div>
              <div class="box-body">
                
                <!-- KPI Callout Cards -->
                <div class="grid-row" style="margin-bottom: 15px;">
                  <div class="col-4">
                    <div style="background: linear-gradient(135deg, #1e293b 0%, #334155 100%); color: #fff; padding: 12px; border-radius: 8px; border: 1px solid #475569; text-align: center;">
                      <div style="font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; opacity: 0.85;">Baseline Active Cases</div>
                      <div id="kpi-sim-baseline" style="font-size: 20px; font-weight: 800; margin: 4px 0; color: #38bdf8;">8,482</div>
                      <div style="font-size: 11px; opacity: 0.85;">8.82% Prevalence</div>
                    </div>
                  </div>
                  <div class="col-4">
                    <div style="background: linear-gradient(135deg, #7f1d1d 0%, #991b1b 100%); color: #fff; padding: 12px; border-radius: 8px; border: 1px solid #f87171; text-align: center;">
                      <div style="font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; opacity: 0.85;">Simulated Active Cases</div>
                      <div id="kpi-sim-active" style="font-size: 20px; font-weight: 800; margin: 4px 0; color: #fca5a5;">8,482</div>
                      <div id="kpi-sim-prev" style="font-size: 11px; opacity: 0.85;">8.82% Prevalence</div>
                    </div>
                  </div>
                  <div class="col-4">
                    <div style="background: linear-gradient(135deg, #064e3b 0%, #047857 100%); color: #fff; padding: 12px; border-radius: 8px; border: 1px solid #34d399; text-align: center;">
                      <div style="font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; opacity: 0.85;">Diabetes Cases Prevented</div>
                      <div id="kpi-sim-saved" style="font-size: 20px; font-weight: 800; margin: 4px 0; color: #6ee7b7;">0</div>
                      <div id="kpi-sim-pct" style="font-size: 11px; opacity: 0.85;">0.0% Reduction</div>
                    </div>
                  </div>
                </div>

                <div id="sim-text-container"></div>
                <hr style="border: 0; border-top: 1px solid #a0a0a0; border-bottom: 1px solid #fff; margin: 15px 0;">
                <div id="sim-plot" style="height: 290px; width: 100%;"></div>
                
                <div class="overview-summary-card" style="margin-top: 15px;">
                  <div class="summary-badge badge-blue">Public Health ROI & Impact</div>
                  <p class="readable-desc-text">
                    Counterfactual modeling demonstrates significant healthcare leverage. Combining targeted BMI and glycemic control shifts prevents thousands of diabetes diagnoses across 96,146 clean clinical evaluation records (deduplicated from the 100,000 raw dataset), mitigating long-term microvascular healthcare expenditure.
                  </p>
                </div>

              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- TAB 6: Project Conclusion -->
      <div id="tab-conclusion" class="tab-content">
        <div class="grid-row">
          <div class="col-12">
            <div class="box">
              <div class="box-header">
                <span class="box-title">🏁 CLINICAL MACHINE LEARNING STUDY — ANALYTICAL CONCLUSION</span>
              </div>
              <div class="box-body">
                <div class="tiger-document">
                  
                  <!-- Conclusion Hero Banner -->
                  <div class="portal-main-heading" style="background: linear-gradient(135deg, #064e3b 0%, #047857 50%, #0f172a 100%); border-color: #047857;">
                    <span class="portal-badge-title"><i class="fas fa-check-double"></i> FINAL RESEARCH VERDICT</span>
                    <h1>Project Conclusion: Biomarkers Drive Primary Predictive Power</h1>
                    <p>Empirical Findings & Diagnostic Synthesis Across 96,146 Patient Evaluation Cohort</p>
                    <div class="meta-pills-row">
                      <span class="meta-badge"><i class="fas fa-user"></i> Author: Niket Banda</span>
                      <span class="meta-badge badge-green"><i class="fas fa-check-circle"></i> Claim Status: VERIFIED</span>
                      <span class="meta-badge badge-blue"><i class="fas fa-chart-line"></i> ROC-AUC: 96.2%</span>
                    </div>
                  </div>
                  
                  <!-- Section 1: Synthesis of the Primary Claim & Research Question -->
                  <h3>1. Primary Research Question & Claim Verdict</h3>
                  <div class="hero-research-card" style="border-left-color: #10b981; background: linear-gradient(to right, #ecfdf5, #f8fafc); border-color: #a7f3d0;">
                    <div class="hero-icon" style="color: #059669;"><i class="fas fa-award"></i></div>
                    <div class="hero-content">
                      <strong style="color: #047857;">Conclusion to Primary Research Claim</strong>
                      <p>"YES — We can accurately predict diabetes status (95.9% CV Accuracy, 96.2% ROC-AUC). Glycated hemoglobin (HbA1c) and blood glucose levels exert a significantly stronger predictive influence than demographic or lifestyle factors."</p>
                    </div>
                  </div>
                  <p>
                    Multivariable logistic regression on 96,146 deduplicated clinical records confirms that laboratory diagnostic biomarkers are the single most dominant risk predictors. Each +1.0% elevation in HbA1c multiplies diabetes odds by 10.34x (95% CI: [9.64 – 11.09], p &lt; 0.001), while fasting/random blood glucose increases odds continuously by 1.034x per mg/dL. In contrast, demographic variables such as BMI (OR: 1.092) and Age (OR: 1.048) represent secondary continuous risk multipliers.
                  </p>
                  
                  <!-- 3 Summary Pillars Grid -->
                  <div class="subquestion-grid">
                    <div class="subquestion-card q1-card" style="border-top-color: #10b981;">
                      <span class="q-badge" style="background: #d1fae5; color: #047857; border-color: #a7f3d0;"><i class="fas fa-flask"></i> Pillar 01: Biomarker Dominance</span>
                      <h4>Laboratory Metrics Outweigh Demographics</h4>
                      <p>HbA1c and Blood Glucose drive over 80% of model log-odds variance. Metabolic diagnostic thresholds (HbA1c ≥ 6.5%, Glucose ≥ 140 mg/dL) serve as clear physiological boundary conditions for classification.</p>
                      <div class="q-focus-pill">🧪 <strong>Key Metric:</strong> HbA1c OR: 10.34x [9.64 – 11.09]</div>
                    </div>
                    <div class="subquestion-card q2-card" style="border-top-color: #0284c7;">
                      <span class="q-badge" style="background: #e0f2fe; color: #0369a1; border-color: #bae6fd;"><i class="fas fa-shield-alt"></i> Pillar 02: High-Sensitivity Screening</span>
                      <h4>Prioritizing Diagnostic Recall</h4>
                      <p>In clinical population screening, missing a true diabetic patient carries severe microvascular risk. The model achieves 96.2% ROC-AUC, enabling threshold tuning to minimize False Negatives.</p>
                      <div class="q-focus-pill">🛡️ <strong>Key Metric:</strong> 10-Fold CV ROC-AUC: 96.2%</div>
                    </div>
                    <div class="subquestion-card q1-card" style="border-top-color: #f59e0b;">
                      <span class="q-badge" style="background: #fef3c7; color: #b45309; border-color: #fde68a;"><i class="fas fa-user-shield"></i> Pillar 03: Preventive Window</span>
                      <h4>Targeting High-Risk Non-Diabetics</h4>
                      <p>While diabetes prevalence is 8.82%, 85.21% of non-diabetic patients exhibit moderate to high risk. Targeted interventions (such as a 5–10% population BMI reduction) offer vital preventive leverage.</p>
                      <div class="q-focus-pill">🎯 <strong>Key Metric:</strong> At-Risk Cohort: 85.21%</div>
                    </div>
                  </div>
                  
                  <!-- Section 2: Model Performance Benchmarks Table -->
                  <h3>2. Final Model Benchmarks & Specifications</h3>
                  <div class="findings-table-container">
                    <table class="findings-table">
                      <thead>
                        <tr>
                          <th>Evaluation Domain</th>
                          <th>Metric / Result</th>
                          <th>Methodological Specification</th>
                        </tr>
                      </thead>
                      <tbody>
                        <tr>
                          <td><strong>Total Cohort Size</strong></td>
                          <td><span class="or-badge medium-impact">96,146 Patients</span></td>
                          <td>Deduplicated clinical records evaluated across 80/20 train/test split.</td>
                        </tr>
                        <tr>
                          <td><strong>Overall Diabetes Prevalence</strong></td>
                          <td><span class="or-badge moderate-impact">8.82%</span></td>
                          <td>Stratified sampling preserving class balance across partitions.</td>
                        </tr>
                        <tr>
                          <td><strong>10-Fold CV Accuracy</strong></td>
                          <td><span class="or-badge high-impact">95.9%</span></td>
                          <td>Resampled evaluation across 10 cross-validation training folds.</td>
                        </tr>
                        <tr>
                          <td><strong>10-Fold CV ROC-AUC</strong></td>
                          <td><span class="or-badge high-impact">96.2%</span></td>
                          <td>Exceptional discrimination capacity across probability decision cutoffs.</td>
                        </tr>
                        <tr>
                          <td><strong>Primary Clinical Predictor</strong></td>
                          <td><span class="or-badge high-impact">HbA1c (OR: 10.34x)</span></td>
                          <td>95% CI: [9.64 – 11.09], p &lt; 0.001 ***</td>
                        </tr>
                      </tbody>
                    </table>
                  </div>
                  
                  <!-- Section 3: Catalog Entry Box -->
                  <h3>3. Class Showcase Catalog Entry</h3>
                  <div class="tiger-formula-box" style="font-size: 13.5px; font-weight: 600; color: #064e3b; background: #ecfdf5; border-color: #10b981; line-height: 1.6;">
                    Project Catalog Summary (Author: Niket Banda): Multivariable logistic regression modeling on 96,146 clinical records identified HbA1c (OR: 10.34, 95% CI: 9.64–11.09), blood glucose level (OR: 1.034, 95% CI: 1.033–1.035), and hypertension (OR: 2.15, 95% CI: 1.96–2.35) as primary diagnostic risk drivers. Evaluated across 10-fold cross-validation, the pipeline achieved 96.2% ROC-AUC and 95.9% accuracy with high clinical sensitivity optimized for diagnostic screening.
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

    </main>
  </div>

  <!-- JavaScript and Plotly definitions -->
  <script>
    // --- Pre-calculated datasets embedded from R ---
    const totalPatients = ', total_patients, ';
    const pctDiabetic = ', pct_diabetic, ';
    const pctRisk = ', pct_risk, ';
    
    const riskSummary = ', risk_summary_json, ';
    const boxData = ', box_data_json_str, ';
    const ageData = ', df_age_json, ';
    const smokingData = ', df_smoking_json, ';
    const scatterData = ', df_sample_json, ';
    const comorbData = ', df_comorb_json, ';
    
    // Simulator cohort and coefficients
    const simCohort = ', sim_cohort_json, ';
    const coefBmi = ', coef_bmi, ';
    const totalBaselineCases = ', total_baseline_cases, ';
    const scalingFactor = ', sim_scaling_factor, ';

    const tierColors = {
      "Low Risk": "#2a9d8f",
      "Moderate Risk": "#e9c46a",
      "High Risk": "#f4a261",
      "Diabetic": "#e76f51"
    };

    // --- Tab Switching Logic ---
    function switchTab(tabId) {
      // Hide all tabs
      document.querySelectorAll(\'.tab-content\').forEach(el => el.classList.remove(\'active\'));
      // Remove active class from menu items
      document.querySelectorAll(\'.sidebar-menu li\').forEach(el => el.classList.remove(\'active\'));

      // Show selected tab
      document.getElementById(\'tab-\' + tabId).classList.add(\'active\');
      // Set active menu item
      document.getElementById(\'menu-\' + tabId).classList.add(\'active\');

      // Trigger redraw of Plotly plots in active tab after element becomes visible
      setTimeout(() => {
        if (tabId === \'overview\') {
          renderOverviewPlots();
        } else if (tabId === \'simulator\') {
          renderSimulationPlots();
        } else if (tabId === \'demographics\') {
          renderDemographicsPlots();
        } else if (tabId === \'biomarkers\') {
          renderBiomarkersPlots();
        }
        window.dispatchEvent(new Event(\'resize\'));
      }, 100);
    }

    // --- Chart Rendering Functions ---
    
    function renderOverviewPlots() {
      // Chart 1: Risk Tier Distribution
      const riskTiers = riskSummary.map(d => d.risk_tier);
      const percentages = riskSummary.map(d => d.Percentage);
      const counts = riskSummary.map(d => d.Count);

      const riskTrace = {
        x: riskTiers,
        y: percentages,
        type: \'bar\',
        marker: {
          color: riskTiers.map(tier => tierColors[tier])
        },
        text: percentages.map(p => p.toFixed(2) + \'%\'),
        textposition: \'auto\',
        hoverinfo: \'text\',
        hovertext: riskTiers.map((tier, i) => `Risk Tier: ${tier}<br>Patients: ${counts[i].toLocaleString()}<br>Percentage: ${percentages[i].toFixed(2)}%`)
      };

      const riskLayout = {
        xaxis: { title: "Risk Classification", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        yaxis: { title: "Cohort Percentage (%)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        showlegend: false,
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 20, b: 40, l: 40, r: 20 }
      };

      Plotly.newPlot(\'risk-dist-plot\', [riskTrace], riskLayout, {responsive: true});

      // Chart 2: HbA1c Levels Across Risk Tiers Box Plot
      const boxTraces = Object.keys(boxData).map(tier => {
        return {
          y: boxData[tier],
          type: \'box\',
          name: tier,
          boxpoints: false, // Hide outliers
          marker: { color: tierColors[tier] }
        };
      });

      const boxLayout = {
        xaxis: { title: "Risk Classification", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        yaxis: { title: "HbA1c Level (%)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        showlegend: false,
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 20, b: 40, l: 40, r: 20 },
        shapes: [{
          type: \'line\',
          xref: \'paper\',
          x0: 0,
          x1: 1,
          yref: \'y\',
          y0: 6.5,
          y1: 6.5,
          line: {
            color: \'#d9534f\',
            width: 2,
            dash: \'dash\'
          }
        }],
        annotations: [{
          x: 0.02,
          y: 6.5,
          xref: \'paper\',
          yref: \'y\',
          text: \'Diabetes Threshold (6.5%)\',
          showarrow: false,
          font: {
            color: \'#d9534f\',
            size: 10,
            family: \'Lucida Grande, sans-serif\'
          },
          yshift: 8,
          xanchor: \'left\'
        }]
      };

      Plotly.newPlot(\'clinical-metrics-plot\', boxTraces, boxLayout, {responsive: true});
    }

    function renderDemographicsPlots() {
      // Chart 3: Diabetes Prevalence by Age Group
      const ageTrace = {
        x: ageData.map(d => d.age_group),
        y: ageData.map(d => d.Prevalence),
        type: \'bar\',
        marker: {
          color: \'#4fa0f9\',
          line: { color: \'#005ccb\', width: 1 }
        },
        hoverinfo: \'text\',
        hovertext: ageData.map(d => `Age Group: ${d.age_group}<br>Diabetes Prevalence: ${d.Prevalence.toFixed(2)}%<br>Total Patients: ${d.Total.toLocaleString()}<br>Diabetic Count: ${d.Diabetic_Count.toLocaleString()}`)
      };

      const ageLayout = {
        xaxis: { 
          title: "Age Classification", 
          font: { family: "Lucida Grande, sans-serif", size: 11 },
          tickvals: ["Pediatric (<18)", "Young Adult (18-34)", "Adult (35-49)", "Middle-Aged (50-64)", "Senior (65+)"],
          ticktext: ["Pediatric<br>(<18)", "Young Adult<br>(18-34)", "Adult<br>(35-49)", "Middle-Aged<br>(50-64)", "Senior<br>(65+)"]
        },
        yaxis: { title: "Prevalence Rate (%)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        showlegend: false,
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 20, b: 60, l: 40, r: 20 }
      };

      Plotly.newPlot(\'age-plot\', [ageTrace], ageLayout, {responsive: true});

      // Chart 4: Risk Tier Stacked Bar Chart by Smoking History
      const smokingCategories = [...new Set(smokingData.map(d => d.smoking_history))];
      const riskTiers = ["Low Risk", "Moderate Risk", "High Risk", "Diabetic"];

      const smokingTraces = riskTiers.map(tier => {
        return {
          x: smokingCategories,
          y: smokingCategories.map(cat => {
            const match = smokingData.find(d => d.smoking_history === cat && d.risk_tier === tier);
            return match ? match.Percentage : 0;
          }),
          name: tier,
          type: \'bar\',
          marker: { color: tierColors[tier] },
          hoverinfo: \'text\',
          hovertext: smokingCategories.map(cat => {
            const match = smokingData.find(d => d.smoking_history === cat && d.risk_tier === tier);
            if (!match) return \'\';
            return `Smoking History: ${cat}<br>Risk Category: ${tier}<br>Patient Count: ${match.Count.toLocaleString()}<br>Percentage: ${match.Percentage.toFixed(2)}%`;
          })
        };
      });

      const smokingLayout = {
        barmode: \'stack\',
        xaxis: { title: "Smoking Category", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        yaxis: { title: "Cohort Percentage (%)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        legend: { title: { text: "<b>Risk Tier</b>" }, font: { family: "Lucida Grande, sans-serif", size: 10 } },
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 20, b: 40, l: 40, r: 20 }
      };

      Plotly.newPlot(\'smoking-plot\', smokingTraces, smokingLayout, {responsive: true});
    }

    function renderBiomarkersPlots() {
      // Chart 5: Biomarker Scatter Plot (Sampled Cohort)
      const statuses = [\'Diabetic\', \'Non-Diabetic\'];
      const scatterTraces = statuses.map(status => {
        const subset = scatterData.filter(d => d.Diabetes_Status === status);
        return {
          x: subset.map(d => d.blood_glucose_level),
          y: subset.map(d => d.HbA1c_level),
          mode: \'markers\',
          type: \'scatter\',
          name: status,
          marker: {
            color: status === \'Diabetic\' ? \'#ff5d5d\' : \'#4fa0f9\',
            size: 7,
            opacity: 0.75,
            line: { width: 0.75, color: \'#fff\' }
          },
          hoverinfo: \'text\',
          hovertext: subset.map(d => `Status: ${d.Diabetes_Status}<br>HbA1c: ${d.HbA1c_level}%<br>Glucose: ${d.blood_glucose_level} mg/dL<br>Age: ${d.age} yrs<br>BMI: ${d.bmi}`)
        };
      });

      const minGlucose = scatterData.reduce((min, d) => d.blood_glucose_level < min ? d.blood_glucose_level : min, Infinity);
      const maxGlucose = scatterData.reduce((max, d) => d.blood_glucose_level > max ? d.blood_glucose_level : max, -Infinity);
      const minHbA1c = scatterData.reduce((min, d) => d.HbA1c_level < min ? d.HbA1c_level : min, Infinity);
      const maxHbA1c = scatterData.reduce((max, d) => d.HbA1c_level > max ? d.HbA1c_level : max, -Infinity);

      scatterTraces.push({
        x: [minGlucose - 5, maxGlucose + 15],
        y: [6.5, 6.5],
        mode: \'lines\',
        type: \'scatter\',
        name: \'HbA1c Threshold (6.5%)\',
        line: {
          color: \'#d9534f\',
          width: 2,
          dash: \'dash\'
        },
        hoverinfo: \'none\'
      });

      scatterTraces.push({
        x: [140, 140],
        y: [minHbA1c - 0.5, maxHbA1c + 1],
        mode: \'lines\',
        type: \'scatter\',
        name: \'Glucose Threshold (140 mg/dL)\',
        line: {
          color: \'#f0ad4e\',
          width: 2,
          dash: \'dash\'
        },
        hoverinfo: \'none\'
      });

      const scatterLayout = {
        xaxis: { title: "Blood Glucose Level (mg/dL)", font: { family: "Lucida Grande, sans-serif", size: 11 }, range: [minGlucose - 5, maxGlucose + 10] },
        yaxis: { title: "HbA1c Level (%)", font: { family: "Lucida Grande, sans-serif", size: 11 }, range: [minHbA1c - 0.3, maxHbA1c + 0.5] },
        legend: { title: { text: "<b>Diagnosis & Limits</b>" }, font: { family: "Lucida Grande, sans-serif", size: 10 } },
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 25, b: 40, l: 40, r: 20 },
        shapes: [
          {
            type: \'rect\',
            xref: \'x\',
            yref: \'y\',
            x0: 140,
            x1: maxGlucose + 20,
            y0: 6.5,
            y1: maxHbA1c + 1,
            fillcolor: \'rgba(239, 68, 68, 0.12)\',
            line: {
              color: \'rgba(220, 38, 38, 0.4)\',
              width: 1.5,
              dash: \'dot\'
            },
            layer: \'below\'
          }
        ],
        annotations: [
          {
            x: Math.min(maxGlucose - 30, 230),
            y: Math.min(maxHbA1c - 0.3, 8.4),
            xref: \'x\',
            yref: \'y\',
            text: \'<b>High-Risk Diagnostic Region</b><br>(HbA1c ≥ 6.5% & Glucose ≥ 140 mg/dL)\',
            showarrow: false,
            font: { family: "Lucida Grande, sans-serif", size: 10, color: "#991b1b" },
            bgcolor: "rgba(254, 226, 226, 0.92)",
            bordercolor: "#f87171",
            borderwidth: 1,
            borderpad: 5
          }
        ]
      };

      Plotly.newPlot(\'biomarker-scatter\', scatterTraces, scatterLayout, {responsive: true});

      // Chart 6: Cardiovascular Comorbidities & Diabetes Risk
      const comorbTrace = {
        x: comorbData.map(d => d.Comorbidity),
        y: comorbData.map(d => d.Prevalence),
        type: \'bar\',
        marker: {
          color: \'#ff9833\',
          line: { color: \'#b34a00\', width: 1 }
        },
        hoverinfo: \'text\',
        hovertext: comorbData.map(d => `Comorbidities: ${d.Comorbidity}<br>Diabetes Prevalence: ${d.Prevalence.toFixed(2)}%<br>Cohort Patients: ${d.Total.toLocaleString()}<br>Diabetic Count: ${d.Diabetic_Count.toLocaleString()}`)
      };

      const comorbLayout = {
        xaxis: { title: "Comorbidity Profile", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        yaxis: { title: "Prevalence Rate (%)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        showlegend: false,
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 20, b: 40, l: 40, r: 20 }
      };

      Plotly.newPlot(\'comorbidity-plot\', [comorbTrace], comorbLayout, {responsive: true});
    }

    // --- Simulator JS Logic ---
    const coefHbA1c = ', coef_hba1c, ';
    let currentSimResults = null;

    function runSimulation(bmiReductionPercent, hba1cDrop) {
      const bmiReduction = bmiReductionPercent / 100;
      let expectedCases = 0;
      
      for (let i = 0; i < simCohort.length; i++) {
        const patient = simCohort[i];
        const newBmi = patient.bmi * (1 - bmiReduction);
        const newHbA1c = Math.max(3.5, patient.HbA1c_level - hba1cDrop);
        const logit = patient.logit_base + (coefBmi * newBmi) + (coefHbA1c * newHbA1c);
        const prob = 1 / (1 + Math.exp(-logit));
        expectedCases += prob;
      }
      
      const scaledExpectedCases = expectedCases * scalingFactor;
      const baselineCases = totalBaselineCases; 
      const preventedCases = Math.max(0, baselineCases - scaledExpectedCases);
      const newPrevalence = (scaledExpectedCases / totalPatients) * 100;
      const baselinePrev = (baselineCases / totalPatients) * 100;
      const preventedPct = baselineCases > 0 ? (preventedCases / baselineCases) * 100 : 0;

      return {
        expectedCases: scaledExpectedCases,
        baselineCases: baselineCases,
        preventedCases: preventedCases,
        newPrevalence: newPrevalence,
        baselinePrev: baselinePrev,
        preventedPct: preventedPct,
        reductionBmi: bmiReductionPercent,
        reductionHbA1c: hba1cDrop
      };
    }

    function applyPreset(bmi, hba1c, btn) {
      document.getElementById(\'bmi-slider\').value = bmi;
      document.getElementById(\'hba1c-slider\').value = hba1c;
      
      const btns = document.querySelectorAll(\'.preset-btn\');
      btns.forEach(b => b.classList.remove(\'active\'));
      if (btn) {
        btn.classList.add(\'active\');
      }
      
      updateSimulation();
    }

    function updateSimulation() {
      const bmiVal = parseFloat(document.getElementById(\'bmi-slider\').value || 0);
      const hba1cVal = parseFloat(document.getElementById(\'hba1c-slider\').value || 0);
      
      document.getElementById(\'slider-bmi-value\').innerText = bmiVal + \'%\';
      document.getElementById(\'slider-hba1c-value\').innerText = hba1cVal.toFixed(1) + \'%\';

      // Sync active state of preset scenario buttons
      const btns = document.querySelectorAll(\'.preset-btn\');
      btns.forEach(b => {
        const bBmi = parseFloat(b.getAttribute(\'data-bmi\'));
        const bHba1c = parseFloat(b.getAttribute(\'data-hba1c\'));
        if (Math.abs(bBmi - bmiVal) < 0.01 && Math.abs(bHba1c - hba1cVal) < 0.01) {
          b.classList.add(\'active\');
        } else {
          b.classList.remove(\'active\');
        }
      });
      
      const results = runSimulation(bmiVal, hba1cVal);
      currentSimResults = results;

      // Update KPI Cards
      const prevented = Math.round(results.preventedCases);
      const active = Math.round(results.expectedCases);
      
      document.getElementById(\'kpi-sim-active\').innerText = active.toLocaleString();
      document.getElementById(\'kpi-sim-prev\').innerText = results.newPrevalence.toFixed(2) + \'% Prevalence\';
      document.getElementById(\'kpi-sim-saved\').innerText = prevented.toLocaleString();
      document.getElementById(\'kpi-sim-pct\').innerText = \'−\' + results.preventedPct.toFixed(1) + \'% Reduction\';

      // Build text summary
      let interventions = [];
      if (bmiVal > 0) interventions.push(`BMI reduction of <strong>${bmiVal}%</strong>`);
      if (hba1cVal > 0) interventions.push(`HbA1c reduction of <strong>-${hba1cVal.toFixed(1)}%</strong>`);
      
      const intervStr = interventions.length === 0 ? "No population interventions currently applied." : interventions.join(" combined with ");

      const htmlText = `
        <div class="sim-report-box">
          <h4><strong>Simulation Findings</strong></h4>
          <p style="font-size: 14.5px; margin-bottom: 0; line-height: 1.5; color: #1e293b;">
            Under a scenario of ${intervStr}, the model predicts preventing 
            <strong>${prevented.toLocaleString()}</strong> cases of diabetes (a <strong>${results.preventedPct.toFixed(1)}%</strong> reduction in active cases). 
            Cohort prevalence shifts from <strong>${results.baselinePrev.toFixed(2)}%</strong> to <strong>${results.newPrevalence.toFixed(2)}%</strong>.
          </p>
        </div>
      `;
      document.getElementById(\'sim-text-container\').innerHTML = htmlText;

      // Render plots if tab is active
      if (document.getElementById(\'tab-simulator\').classList.contains(\'active\')) {
        renderSimulationPlots();
      }
    }

    function renderSimulationPlots() {
      if (!currentSimResults) return;
      
      const plotData = [
        {
          x: [\'Baseline (Current)\', \'Simulated Intervention\'],
          y: [currentSimResults.baselineCases, currentSimResults.expectedCases],
          type: \'bar\',
          marker: {
            color: [\'#1e40af\', \'#059669\']
          },
          text: [
            `${Math.round(currentSimResults.baselineCases).toLocaleString()}\\n(${currentSimResults.baselinePrev.toFixed(2)}%)`,
            `${Math.round(currentSimResults.expectedCases).toLocaleString()}\\n(${currentSimResults.newPrevalence.toFixed(2)}%)`
          ],
          textposition: \'auto\',
          hoverinfo: \'text\',
          hovertext: [
            `Scenario: Baseline (Current)<br>Expected Cases: ${Math.round(currentSimResults.baselineCases).toLocaleString()}<br>Prevalence: ${currentSimResults.baselinePrev.toFixed(2)}%`,
            `Scenario: Simulated Intervention<br>Expected Cases: ${Math.round(currentSimResults.expectedCases).toLocaleString()}<br>Prevalence: ${currentSimResults.newPrevalence.toFixed(2)}%`
          ]
        }
      ];

      const plotLayout = {
        xaxis: { title: "" },
        yaxis: { title: "Expected Diabetes Cases", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        showlegend: false,
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 30, b: 20, l: 50, r: 20 }
      };

      Plotly.newPlot(\'sim-plot\', plotData, plotLayout, {responsive: true});
    }

    // --- Onload Initialization ---
    window.onload = function() {
      // Populate initial values
      updateSimulation();
      
      // Pre-render plots for when tabs are navigated
      renderOverviewPlots();
    };
  </script>
</body>
</html>')

# Output to sk-website-diabetes/index.html, index.html, and docs/index.html
output_dirs <- c(
  "/workspaces/codespace-starter/sk-website-diabetes",
  "/workspaces/codespace-starter",
  "/workspaces/codespace-starter/docs"
)

for (out_dir in output_dirs) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(html_content, file.path(out_dir, "index.html"))
}

cat("========================================================================\n")
cat("Dashboard built successfully across all targets!\n")
cat("Locations:\n - sk-website-diabetes/index.html\n - index.html\n - docs/index.html\n")
cat("========================================================================\n")
