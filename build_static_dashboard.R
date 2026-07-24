#!/usr/bin/env Rscript
# ==============================================================================
# Script: build_static_dashboard.R
# Purpose: Build a single, fully self-contained HTML dashboard that replicates
#          the Shiny application's UI, aesthetic style, charts, and simulator.
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

# Calculate base logit (no BMI component) for each patient
df_sim_base <- df %>%
  mutate(
    hypertension_val = ifelse(hypertension == "Yes", 1, 0),
    heart_disease_val = ifelse(heart_disease == "Yes", 1, 0),
    logit_no_bmi = intercept + 
                   coef_age * age + 
                   coef_hba1c * HbA1c_level + 
                   coef_glucose * blood_glucose_level + 
                   coef_hypertension * hypertension_val + 
                   coef_heart_disease * heart_disease_val
  ) %>%
  select(bmi, logit_no_bmi, diabetes)

# To keep the static HTML file lightweight and fast in pure JS,
# we take a representative sample of 20,000 patients for the simulator
# and scale the expected values back to the full cohort.
set.seed(42)
sim_cohort <- df_sim_base %>%
  slice_sample(n = 20000)

total_baseline_cases <- sum(df$diabetes)
base_sim_prob_sum <- sum(1 / (1 + exp(-(sim_cohort$logit_no_bmi + coef_bmi * sim_cohort$bmi))))
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
      margin-top: 15px;
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
        <li id="menu-overview" class="active"><a onclick="switchTab(\'overview\')"><i class="fas fa-users"></i> Patient Overview</a></li>
        <li id="menu-simulator"><a onclick="switchTab(\'simulator\')"><i class="fas fa-heartbeat"></i> Clinical Intervention Simulator</a></li>
        <li id="menu-demographics"><a onclick="switchTab(\'demographics\')"><i class="fas fa-id-card"></i> Demographics & Lifestyle</a></li>
        <li id="menu-biomarkers"><a onclick="switchTab(\'biomarkers\')"><i class="fas fa-flask"></i> Biomarker Correlation</a></li>
      </ul>
    </aside>

    <!-- Main Content Panel -->
    <main>
      
      <!-- TAB 1: Patient Overview -->
      <div id="tab-overview" class="tab-content active">
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
                <div id="risk-dist-plot" style="height: 400px; width: 100%;"></div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">HbA1c Levels Across Risk Tiers</span>
              </div>
              <div class="box-body">
                <div id="clinical-metrics-plot" style="height: 400px; width: 100%;"></div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- TAB 2: Clinical Intervention Simulator -->
      <div id="tab-simulator" class="tab-content">
        <div class="grid-row">
          <div class="col-4">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Simulation Parameters</span>
              </div>
              <div class="box-body">
                <div class="slider-container">
                  <div class="slider-label">
                    <span>Population BMI Reduction:</span>
                    <span id="slider-value">0%</span>
                  </div>
                  <input type="range" id="bmi-slider" class="mac-slider" min="0" max="20" value="0" step="1" oninput="updateSimulation(this.value)">
                </div>
                <div class="help-text">
                  This simulator models a counterfactual clinical scenario. By shifting the entire cohort\'s BMI distribution downward by the selected percentage, it estimates the potential reduction in diabetes prevalence using a baseline logistic regression model.
                </div>
                <div class="alert-box">
                  <strong>Note:</strong> The model controls for patient age, HbA1c level, blood glucose level, hypertension history, and heart disease history.
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
                <div id="sim-text-container"></div>
                <hr style="border: 0; border-top: 1px solid #a0a0a0; border-bottom: 1px solid #fff; margin: 20px 0;">
                <div id="sim-plot" style="height: 350px; width: 100%;"></div>
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
                <div id="age-plot" style="height: 400px; width: 100%;"></div>
              </div>
            </div>
          </div>
          <div class="col-6">
            <div class="box">
              <div class="box-header">
                <span class="box-title">Risk Tier Distribution by Smoking History</span>
              </div>
              <div class="box-body">
                <div id="smoking-plot" style="height: 400px; width: 100%;"></div>
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
                <div id="biomarker-scatter" style="height: 400px; width: 100%;"></div>
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
                <div id="comorbidity-plot" style="height: 400px; width: 100%;"></div>
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

      // Trigger redraw of Plotly plots in active tab because they might render poorly if drawn while hidden
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
      }, 50);
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

      scatterTraces.push({
        x: [minGlucose, maxGlucose],
        y: [6.5, 6.5],
        mode: \'lines\',
        type: \'scatter\',
        name: \'Diabetes Threshold (6.5%)\',
        line: {
          color: \'#d9534f\',
          width: 2,
          dash: \'dash\'
        },
        hoverinfo: \'none\'
      });

      const scatterLayout = {
        xaxis: { title: "Blood Glucose Level (mg/dL)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        yaxis: { title: "HbA1c Level (%)", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        legend: { title: { text: "<b>Diagnosis</b>" }, font: { family: "Lucida Grande, sans-serif", size: 10 } },
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 20, b: 40, l: 40, r: 20 }
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

    // --- Simulation System Logic ---
    let currentSimResults = null;

    function runSimulation(bmiReductionPercent) {
      const reduction = bmiReductionPercent / 100;
      let expectedCases = 0;
      
      for (let i = 0; i < simCohort.length; i++) {
        const patient = simCohort[i];
        const newBmi = patient.bmi * (1 - reduction);
        const logit = patient.logit_no_bmi + (coefBmi * newBmi);
        const prob = 1 / (1 + Math.exp(-logit));
        expectedCases += prob;
      }
      
      // Scale cases using standard scaling factor (actual cohort size vs sample size)
      const scaledExpectedCases = expectedCases * scalingFactor;
      const baselineCases = totalBaselineCases; 
      const preventedCases = Math.max(0, baselineCases - scaledExpectedCases);
      const newPrevalence = (scaledExpectedCases / totalPatients) * 100;
      const baselinePrev = (baselineCases / totalPatients) * 100;

      return {
        expectedCases: scaledExpectedCases,
        baselineCases: baselineCases,
        preventedCases: preventedCases,
        newPrevalence: newPrevalence,
        baselinePrev: baselinePrev,
        reductionPercent: bmiReductionPercent
      };
    }

    function updateSimulation(val) {
      document.getElementById(\'slider-value\').innerText = val + \'%\\n\';
      
      // Run calculations
      const results = runSimulation(parseFloat(val));
      currentSimResults = results;

      // Update Text Box
      const prevented = Math.round(results.preventedCases);
      const baselinePrev = results.baselinePrev.toFixed(2);
      const newPrev = results.newPrevalence.toFixed(2);
      
      const htmlText = `
        <div class="sim-report-box">
          <h4><strong>Simulation Findings</strong></h4>
          <p>
            A population-wide BMI reduction of <strong>${val}%</strong> is predicted to prevent 
            <strong>${prevented.toLocaleString()}</strong> cases of diabetes. 
            This shift would decrease the expected cohort prevalence from <strong>${baselinePrev}%</strong> 
            to <strong>${newPrev}%</strong> (reducing the expected number of active cases from 
            ${Math.round(results.baselineCases).toLocaleString()} down to 
            ${Math.round(results.expectedCases).toLocaleString()}).
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
          x: [\'Baseline (Current)\', `Intervention (-${currentSimResults.reductionPercent}% BMI)`],
          y: [currentSimResults.baselineCases, currentSimResults.expectedCases],
          type: \'bar\',
          marker: {
            color: [\'#4fa0f9\', \'#ff5d5d\']
          },
          text: [
            `${Math.round(currentSimResults.baselineCases).toLocaleString()}\\n(${currentSimResults.baselinePrev.toFixed(2)}%)`,
            `${Math.round(currentSimResults.expectedCases).toLocaleString()}\\n(${currentSimResults.newPrevalence.toFixed(2)}%)`
          ],
          textposition: \'auto\',
          hoverinfo: \'text\',
          hovertext: [
            `Scenario: Baseline (Current)<br>Expected Cases: ${Math.round(currentSimResults.baselineCases).toLocaleString()}<br>Prevalence: ${currentSimResults.baselinePrev.toFixed(2)}%`,
            `Scenario: Intervention (-${currentSimResults.reductionPercent}% BMI)<br>Expected Cases: ${Math.round(currentSimResults.expectedCases).toLocaleString()}<br>Prevalence: ${currentSimResults.newPrevalence.toFixed(2)}%`
          ]
        }
      ];

      const plotLayout = {
        xaxis: { title: "" },
        yaxis: { title: "Expected Diabetes Cases", font: { family: "Lucida Grande, sans-serif", size: 11 } },
        showlegend: false,
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        margin: { t: 40, b: 20, l: 50, r: 20 }
      };

      Plotly.newPlot(\'sim-plot\', plotData, plotLayout, {responsive: true});
    }

    // --- Onload Initialization ---
    window.onload = function() {
      // Populate initial values
      updateSimulation(0);
      
      // Render first tab\'s plots
      renderOverviewPlots();
    };
  </script>
</body>
</html>')

# Output to sk-website-diabetes/index.html
output_dir <- "/workspaces/codespace-starter/sk-website-diabetes"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
writeLines(html_content, file.path(output_dir, "index.html"))

cat("========================================================================\n")
cat("Dashboard built successfully!\n")
cat("Location: ", file.path(output_dir, "index.html"), "\n")
cat("========================================================================\n")
