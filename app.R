# ==============================================================================
# Shiny Application: Patient Diabetes Risk & Intervention Simulator
# Aesthetic Style: Flawless 2000s macOS Aqua / Brushed Metal (Skeuomorphic)
# Author: Niket Banda
# Date: 2026-07-24
# ==============================================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)

# 1. Load and prepare data on startup
df <- read_csv("cleaned_diabetes_data.csv", show_col_types = FALSE) %>%
  mutate(
    gender = factor(gender),
    hypertension = factor(hypertension, levels = c("No", "Yes")),
    heart_disease = factor(heart_disease, levels = c("No", "Yes")),
    smoking_history = factor(smoking_history),
    risk_tier = factor(risk_tier, levels = c("Low Risk", "Moderate Risk", "High Risk", "Diabetic"))
  )

# 2. Fit the baseline logistic regression model for the simulator
model <- glm(
  diabetes ~ bmi + age + HbA1c_level + blood_glucose_level + hypertension + heart_disease,
  data = df, 
  family = binomial
)

# ==============================================================================
# User Interface (UI)
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = HTML("<div class='apple-window-controls'><span class='btn-close'></span><span class='btn-min'></span><span class='btn-zoom'></span></div> Diabetes Portal"),
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("📖 Project Report", tabName = "report", icon = icon("book")),
      menuItem("Patient Overview", tabName = "overview", icon = icon("users")),
      menuItem("Demographics & Lifestyle", tabName = "demographics", icon = icon("id-card")),
      menuItem("Biomarker Correlation", tabName = "biomarkers", icon = icon("flask")),
      menuItem("Clinical Intervention Simulator", tabName = "simulator", icon = icon("heartbeat")),
      menuItem("🏁 Project Conclusion", tabName = "conclusion", icon = icon("check-circle"))
    )
  ),
  
  dashboardBody(
    # Custom CSS: Ultra-polished macOS OS X Aqua Theme
    tags$head(
      tags$style(HTML("
        /* Custom Font and Scrollbar Styling */
        body, .main-header .logo, .main-header .navbar, .sidebar, .control-sidebar, .box-header, .box-title, .sim-report-box, .irs-single, .irs-min, .irs-max {
          font-family: 'Lucida Grande', 'Lucida Sans Unicode', Geneva, Verdana, sans-serif !important;
        }
        
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
        
        /* Striated Pinstripe Background for OS X Finder Feel */
        .content-wrapper, .right-side {
          background-color: #dce1eb !important;
          background-image: linear-gradient(rgba(0, 0, 0, 0.04) 50%, transparent 50%) !important;
          background-size: 100% 4px !important;
          padding: 18px !important;
          box-shadow: inset 6px 0 15px rgba(0,0,0,0.06) !important;
        }
        
        /* Aqua window controls in header */
        .apple-window-controls {
          display: inline-flex;
          margin-right: 14px;
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
          background: radial-gradient(circle at 4px 4px, #ffa0a0, #ff3b30 80%) !important;
          border: 1px solid #991610 !important;
        }
        .btn-min {
          background: radial-gradient(circle at 4px 4px, #ffe49e, #ffcc00 80%) !important;
          border: 1px solid #997a00 !important;
        }
        .btn-zoom {
          background: radial-gradient(circle at 4px 4px, #a6f5a6, #34c759 80%) !important;
          border: 1px solid #146d2a !important;
        }
        
        /* Hover details on window controls showing symbols */
        .btn-close::after { content: '×'; color: rgba(0,0,0,0.55); font-size: 9px; font-weight: bold; opacity: 0; transition: opacity 0.15s; }
        .btn-min::after { content: '−'; color: rgba(0,0,0,0.55); font-size: 10px; font-weight: bold; opacity: 0; transition: opacity 0.15s; margin-top: -1px; }
        .btn-zoom::after { content: '+'; color: rgba(0,0,0,0.55); font-size: 9px; font-weight: bold; opacity: 0; transition: opacity 0.15s; }
        .apple-window-controls:hover span::after {
          opacity: 1;
        }
        
        /* Brushed Metal Headers & Logo */
        .skin-blue .main-header .logo {
          background: 
            linear-gradient(to bottom, rgba(255,255,255,0.6) 0%, rgba(255,255,255,0) 100%),
            linear-gradient(rgba(0, 0, 0, 0.04) 50%, transparent 50%),
            linear-gradient(to bottom, #eeeeee 0%, #cccccc 50%, #b5b5b5 100%) !important;
          background-size: auto, 100% 4px, auto !important;
          color: #2c3e50 !important;
          font-weight: 900 !important;
          font-size: 15px !important;
          border-right: 1px solid #909090 !important;
          border-bottom: 1px solid #858585 !important;
          text-shadow: 0 1px 0 rgba(255,255,255,0.9);
          display: flex;
          align-items: center;
          justify-content: flex-start;
          height: 50px;
          box-shadow: inset 0 1px 0 #fff;
        }
        .skin-blue .main-header .navbar {
          background: 
            linear-gradient(to bottom, rgba(255,255,255,0.6) 0%, rgba(255,255,255,0) 100%),
            linear-gradient(rgba(0, 0, 0, 0.04) 50%, transparent 50%),
            linear-gradient(to bottom, #eeeeee 0%, #cccccc 50%, #b5b5b5 100%) !important;
          background-size: auto, 100% 4px, auto !important;
          border-bottom: 1px solid #858585 !important;
          box-shadow: inset 0 1px 0 #fff;
        }
        .skin-blue .main-header .sidebar-toggle {
          color: #333 !important;
          text-shadow: 0 1px 0 #fff;
          height: 50px;
          display: flex;
          align-items: center;
        }
        .skin-blue .main-header .sidebar-toggle:hover {
          background: rgba(0,0,0,0.08) !important;
          color: #000 !important;
        }
        
        /* Finder-style Light Metallic Sidebar with realistic shadow boundaries */
        .main-sidebar {
          background: linear-gradient(to right, #edf1f6 0%, #d5dde8 95%, #b2bcca 100%) !important;
          border-right: 1.5px solid #808b9b !important;
          box-shadow: inset -1px 0 0 #fff, 2px 0 8px rgba(0,0,0,0.05);
        }
        .sidebar-menu {
          padding-top: 15px;
        }
        .sidebar-menu > li > a {
          color: #3c4552 !important;
          font-weight: bold !important;
          font-size: 13px !important;
          text-shadow: 0 1px 0 rgba(255,255,255,0.85);
          border-bottom: 1px solid #c5cbd5;
          border-top: 1px solid #f6f8fa;
          padding: 12px 15px !important;
          display: flex;
          align-items: center;
          position: relative;
        }
        .sidebar-menu > li > a i {
          margin-right: 10px;
          font-size: 16px;
          color: #555c66 !important;
        }
        
        /* Aqua Pill active tab with realistic reflective layer */
        .sidebar-menu > li.active > a {
          background: linear-gradient(to bottom, #5ba4e5 0%, #2081e2 50%, #0d64cc 51%, #4ca1ed 100%) !important;
          color: #fff !important;
          text-shadow: 0 -1px 1px rgba(0,0,0,0.45) !important;
          border-top: 1px solid #7bbbf2 !important;
          border-bottom: 1px solid #00428f !important;
          box-shadow: 0 3px 6px rgba(0,0,0,0.25), inset 0 1px 1px rgba(255,255,255,0.5);
          border-radius: 5px;
          margin: 4px 8px;
        }
        .sidebar-menu > li.active > a i {
          color: #fff !important;
        }
        /* Top half reflection highlight inside active sidebar pill */
        .sidebar-menu > li.active > a::before {
          content: '';
          position: absolute;
          top: 1px;
          left: 2px;
          right: 2px;
          height: 42%;
          background: linear-gradient(to bottom, rgba(255,255,255,0.4) 0%, rgba(255,255,255,0.05) 100%);
          border-radius: 4px 4px 6px 6px / 4px 4px 2px 2px;
          pointer-events: none;
        }
        .sidebar-menu > li > a:hover {
          background-color: rgba(0,0,0,0.05) !important;
          color: #000 !important;
        }
        
        /* 3D Glass/Gel Value Boxes with a Curved Glossy Highlight Overlay */
        .small-box {
          border-radius: 8px !important;
          box-shadow: 0 6px 15px rgba(0,0,0,0.2), inset 0 1px 0 rgba(255,255,255,0.5) !important;
          border: 1px solid #666 !important;
          overflow: hidden;
          position: relative;
        }
        
        /* The Gel reflection overlay */
        .small-box::before {
          content: '';
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
        
        .small-box .inner {
          padding: 15px !important;
          position: relative;
          z-index: 5;
        }
        .small-box .inner h3 {
          font-size: 34px !important;
          font-weight: 800 !important;
          letter-spacing: -1.2px;
          margin-bottom: 5px;
        }
        .small-box .inner p {
          font-size: 13px !important;
          font-weight: bold !important;
        }
        .small-box .icon-large {
          font-size: 60px !important;
          top: 5px !important;
          opacity: 0.22 !important;
          z-index: 1;
        }
        
        /* Aqua Blue Gel Box */
        .bg-navy {
          background: linear-gradient(to bottom, #98c7fa 0%, #4da0f9 49%, #1b80fc 50%, #106ee2 85%, #6eb0ff 100%) !important;
          border: 1px solid #004d9c !important;
          color: #fff !important;
          text-shadow: 0 -1px 1px rgba(0,0,0,0.5) !important;
        }
        
        /* Aqua Red Gel Box */
        .bg-red {
          background: linear-gradient(to bottom, #ffa5a5 0%, #ff5d5d 49%, #ff2020 50%, #e20808 85%, #ff7272 100%) !important;
          border: 1px solid #9c0000 !important;
          color: #fff !important;
          text-shadow: 0 -1px 1px rgba(0,0,0,0.5) !important;
        }
        
        /* Aqua Orange/Yellow Gel Box */
        .bg-orange {
          background: linear-gradient(to bottom, #ffdca3 0%, #ff9833 49%, #ff7e00 50%, #e26300 85%, #ffb05c 100%) !important;
          border: 1px solid #a34300 !important;
          color: #fff !important;
          text-shadow: 0 -1px 1px rgba(0,0,0,0.5) !important;
        }
        
        /* Brushed Metal Content Cards (Boxes) with exact 2000s structural bezel borders */
        .box {
          background: linear-gradient(to bottom, #fcfcfc 0%, #f4f4f4 12%, #e5e5e5 88%, #d8d8d8 100%) !important;
          border-bottom: 3.5px solid #8a8a8a !important;
          border-right: 1.5px solid #949494 !important;
          border-left: 1.5px solid #b8b8b8 !important;
          border-top: 1px solid #fff !important;
          border-radius: 8px !important;
          box-shadow: 0 12px 32px rgba(0,0,0,0.22), inset 0 1px 0 #fff !important;
          color: #333 !important;
          margin-bottom: 20px !important;
        }
        .box-header {
          background: 
            linear-gradient(to bottom, rgba(255,255,255,0.5) 0%, rgba(255,255,255,0) 100%),
            linear-gradient(to bottom, #f3f3f3 0%, #e0e0e0 50%, #d0d0d0 100%) !important;
          border-bottom: 1.5px solid #929292 !important;
          border-top-left-radius: 7px !important;
          border-top-right-radius: 7px !important;
          padding: 10px 15px !important;
          color: #1a1a1a !important;
          text-shadow: 0 1px 0 #fff;
          box-shadow: inset 0 1px 0 #fff;
        }
        .box-title {
          font-size: 14px !important;
          font-weight: bold !important;
        }
        .box-body {
          padding: 15px !important;
        }
        
        /* macOS X standard indented report box */
        .sim-report-box {
          background: linear-gradient(to bottom, #fafafa, #eeeeee) !important;
          border: 1px solid #b5b5b5 !important;
          border-left: 6px solid #2081e2 !important;
          padding: 18px !important;
          border-radius: 6px !important;
          box-shadow: inset 0 1px 4px rgba(0,0,0,0.08), 0 1px 0 #fff !important;
          margin-bottom: 20px !important;
          color: #222 !important;
        }
        
        /* Skeuomorphic OS X Tiger Document Reader */
        .tiger-document {
          background: #ffffff !important;
          color: #1a1a1a !important;
          font-family: 'Lucida Grande', 'Lucida Sans Unicode', Geneva, Verdana, sans-serif !important;
          line-height: 1.6;
          padding: 28px 34px;
          border-radius: 8px;
          border: 1px solid #99aabf;
          box-shadow: inset 0 3px 8px rgba(0, 0, 0, 0.14), inset 0 -1px 3px rgba(0, 0, 0, 0.05), 0 1px 0 #ffffff !important;
          height: 520px !important;
          overflow-y: auto !important;
        }
        
        .tiger-document h3 {
          color: #0c4280;
          font-size: 17px;
          font-weight: bold;
          border-bottom: 2px solid #dce4ee;
          padding-bottom: 6px;
          margin-top: 24px;
          margin-bottom: 12px;
          text-shadow: 0 1px 0 #ffffff;
        }
        
        .tiger-document h3:first-child {
          margin-top: 0;
        }

        .tiger-document p, .tiger-document li {
          font-size: 13.5px;
          color: #2c3e50;
          margin-bottom: 12px;
        }

        .tiger-document code, .tiger-document pre, .tiger-document .tiger-formula-box {
          background-color: #f4f7fa !important;
          border: 1.5px dashed #7b8e9f !important;
          border-radius: 6px !important;
          padding: 12px 16px !important;
          margin: 14px 0 !important;
          font-family: 'Courier New', Courier, monospace !important;
          font-size: 13px !important;
          color: #1a365d !important;
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.06) !important;
          white-space: pre-wrap;
          word-break: break-word;
        }

        .tiger-document table {
          width: 100%;
          border-collapse: separate;
          border-spacing: 0;
          margin: 16px 0;
          border-radius: 6px;
          overflow: hidden;
          border: 1px solid #cbd5e1;
        }

        .tiger-document th {
          background: linear-gradient(to bottom, #edf2f7, #e2e8f0);
          color: #0f172a;
          padding: 8px 12px;
          font-size: 12.5px;
          font-weight: bold;
          border-bottom: 1px solid #cbd5e1;
          text-align: left;
        }

        .tiger-document td {
          padding: 8px 12px;
          font-size: 12.5px;
          border-bottom: 1px solid #e2e8f0;
          color: #334155;
        }

        .tiger-document tr:nth-child(even) td {
          background-color: #f8fafc;
        }

        .tiger-document tr:last-child td {
          border-bottom: none;
        }
        
        .tiger-document .stat-pill {
          display: inline-block;
          background: linear-gradient(to bottom, #ebf4ff, #c3ddfd);
          border: 1px solid #7eaef4;
          border-radius: 4px;
          padding: 2px 7px;
          font-size: 12px;
          font-weight: bold;
          color: #0c4280;
          margin: 0 2px;
        }
        
        /* IonRangeSlider to match Aqua Blue glass knobs */
        .irs-line {
          background: #c5ccd4 !important;
          border: 1px solid #7f8c8d !important;
          box-shadow: inset 0 2px 4px rgba(0,0,0,0.3) !important;
          border-radius: 8px !important;
          height: 8px !important;
        }
        .irs-bar {
          background: linear-gradient(to bottom, #7fc3fe 0%, #208eef 50%, #0c6ecc 51%, #6ebdfd 100%) !important;
          border: 1px solid #005bb7 !important;
          box-shadow: inset 0 1px 0 rgba(255,255,255,0.4), 0 1px 2px rgba(0,0,0,0.15) !important;
          border-radius: 8px !important;
          height: 8px !important;
        }
        .irs-slider {
          background: radial-gradient(circle, #ffffff 0%, #e5e5e5 40%, #b8b8b8 85%, #888888 100%) !important;
          border: 1.5px solid #666 !important;
          box-shadow: 0 2px 6px rgba(0,0,0,0.4), inset 0 1.5px 0 #fff !important;
          border-radius: 50% !important;
          width: 22px !important;
          height: 22px !important;
          top: 18px !important;
        }
        
        /* Bubble tooltip styled as a glossy macOS capsule */
        .irs-single {
          background: linear-gradient(to bottom, #60abec 0%, #2488ea 50%, #0d6dcc 51%, #60abec 100%) !important;
          color: #fff !important;
          border: 1px solid #004d9c !important;
          border-radius: 12px !important;
          box-shadow: 0 3px 6px rgba(0,0,0,0.22), inset 0 1px 0 rgba(255,255,255,0.4) !important;
          font-size: 11px !important;
          font-weight: bold !important;
          text-shadow: 0 -1px 0 rgba(0,0,0,0.3) !important;
          padding: 3px 9px !important;
        }
        .irs-single::after {
          border-top-color: #0d6dcc !important;
        }
        .irs-min, .irs-max {
          color: #444 !important;
          background: transparent !important;
          font-size: 10px !important;
        }

        /* Aesthetic Chart & Overview Description Boxes */
        .chart-desc-box {
          background: linear-gradient(to bottom, #ffffff 0%, #f4f7fb 100%) !important;
          border: 1px solid #b8c8d8 !important;
          border-left: 4px solid #2081e2 !important;
          border-radius: 6px !important;
          padding: 10px 14px !important;
          margin-top: 12px !important;
          font-size: 12px !important;
          color: #2c3e50 !important;
          line-height: 1.5 !important;
          box-shadow: inset 0 1px 0 #ffffff, 0 2px 5px rgba(0, 0, 0, 0.05) !important;
        }
        .chart-desc-box .desc-header {
          font-weight: bold !important;
          color: #0c4280 !important;
          font-size: 12.5px !important;
          margin-bottom: 4px !important;
        }
        .chart-desc-box {
          background: linear-gradient(to bottom, #f8fafc 0%, #edf2f7 100%) !important;
          border: 1px solid #cbd5e1 !important;
          border-left: 4px solid #0284c7 !important;
          border-radius: 6px !important;
          padding: 14px 16px !important;
          margin-top: 15px !important;
          box-shadow: 0 1px 4px rgba(0,0,0,0.04) !important;
        }
        .chart-desc-box .desc-header {
          font-weight: 700 !important;
          color: #0f172a !important;
          font-size: 13.5px !important;
          margin-bottom: 6px !important;
          display: flex !important;
          align-items: center !important;
          gap: 8px !important;
        }
        .chart-desc-box p {
          margin: 0 !important;
          color: #334155 !important;
          font-size: 13.5px !important;
          line-height: 1.6 !important;
        }
        
        .overview-summary-card {
          background: linear-gradient(to bottom, #ffffff 0%, #f8fafc 100%) !important;
          border: 1px solid #cbd5e1 !important;
          border-radius: 8px !important;
          padding: 16px 20px !important;
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05) !important;
        }
        
        .summary-badge {
          display: inline-block !important;
          background: linear-gradient(to bottom, #2b7fff 0%, #1a56b3 100%) !important;
          color: #ffffff !important;
          font-size: 11px !important;
          font-weight: bold !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          padding: 4px 12px !important;
          border-radius: 12px !important;
          margin-bottom: 8px !important;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2), inset 0 1px 0 rgba(255, 255, 255, 0.4) !important;
          text-shadow: 0 1px 1px rgba(0, 0, 0, 0.3) !important;
        }
        
        .summary-badge.badge-blue {
          background: linear-gradient(to bottom, #009688 0%, #00695c 100%) !important;
        }
        
        .readable-desc-text {
          font-size: 13.5px !important;
          color: #334155 !important;
          line-height: 1.65 !important;
          margin: 8px 0 0 0 !important;
        }

        /* Professional Report CSS Enhancements */
        .report-header-banner {
          background: linear-gradient(135deg, #0f172a 0%, #1e293b 50%, #0f172a 100%) !important;
          border-radius: 8px !important;
          padding: 20px 24px !important;
          color: #ffffff !important;
          margin-bottom: 24px !important;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15) !important;
        }
        .report-title-meta h2 {
          color: #ffffff !important;
          font-size: 20px !important;
          font-weight: 700 !important;
          margin: 0 0 12px 0 !important;
          letter-spacing: -0.3px !important;
        }
        .meta-pills-row {
          display: flex !important;
          gap: 10px !important;
          flex-wrap: wrap !important;
        }
        .meta-badge {
          background: rgba(255, 255, 255, 0.12) !important;
          border: 1px solid rgba(255, 255, 255, 0.25) !important;
          color: #e2e8f0 !important;
          font-size: 11.5px !important;
          padding: 4px 12px !important;
          border-radius: 20px !important;
          font-weight: 500 !important;
        }
        .meta-badge.badge-green {
          background: rgba(16, 185, 129, 0.2) !important;
          border-color: rgba(16, 185, 129, 0.4) !important;
          color: #6ee7b7 !important;
        }
        .meta-badge.badge-blue {
          background: rgba(59, 130, 246, 0.2) !important;
          border-color: rgba(59, 130, 246, 0.4) !important;
          color: #93c5fd !important;
        }

        .hero-research-card {
          display: flex !important;
          align-items: flex-start !important;
          gap: 16px !important;
          background: linear-gradient(to right, #eff6ff, #f8fafc) !important;
          border: 1px solid #bfdbfe !important;
          border-left: 5px solid #2563eb !important;
          border-radius: 8px !important;
          padding: 16px 20px !important;
          margin: 16px 0 20px 0 !important;
          box-shadow: 0 2px 8px rgba(37, 99, 235, 0.08) !important;
        }
        .hero-icon {
          font-size: 22px !important;
          color: #2563eb !important;
          margin-top: 2px !important;
        }
        .hero-content strong {
          display: block !important;
          color: #1e3a8a !important;
          font-size: 12px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          margin-bottom: 4px !important;
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
          display: flex !important;
          gap: 20px !important;
          margin: 20px 0 26px 0 !important;
          flex-wrap: wrap !important;
        }
        
        .subquestion-card {
          flex: 1 !important;
          min-width: 300px !important;
          background: #ffffff !important;
          border: 1px solid #cbd5e1 !important;
          border-radius: 10px !important;
          padding: 20px 22px !important;
          box-shadow: 0 4px 14px rgba(0, 0, 0, 0.05) !important;
          position: relative !important;
          overflow: hidden !important;
          transition: transform 0.2s ease, box-shadow 0.2s ease !important;
        }
        
        .subquestion-card.q1-card {
          border-top: 4px solid #0284c7 !important;
        }
        
        .subquestion-card.q2-card {
          border-top: 4px solid #4f46e5 !important;
        }

        .q-badge {
          display: inline-flex !important;
          align-items: center !important;
          gap: 6px !important;
          font-size: 11px !important;
          font-weight: 800 !important;
          text-transform: uppercase !important;
          letter-spacing: 0.6px !important;
          padding: 4px 12px !important;
          border-radius: 14px !important;
          margin-bottom: 12px !important;
        }
        
        .q1-card .q-badge {
          background: #e0f2fe !important;
          color: #0369a1 !important;
          border: 1px solid #bae6fd !important;
        }
        
        .q2-card .q-badge {
          background: #e0e7ff !important;
          color: #4338ca !important;
          border: 1px solid #c7d2fe !important;
        }

        .subquestion-card h4 {
          color: #0f172a !important;
          font-size: 15px !important;
          font-weight: 700 !important;
          margin: 0 0 10px 0 !important;
          line-height: 1.35 !important;
        }

        .subquestion-card p {
          font-size: 13.5px !important;
          color: #334155 !important;
          margin: 0 0 14px 0 !important;
          line-height: 1.6 !important;
        }

        .q-focus-pill {
          display: inline-block !important;
          font-size: 11.5px !important;
          font-weight: 600 !important;
          padding: 4px 10px !important;
          border-radius: 6px !important;
          background: #f8fafc !important;
          border: 1px solid #e2e8f0 !important;
          color: #475569 !important;
        }

        .methodology-grid {
          display: grid !important;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)) !important;
          gap: 14px !important;
          margin: 18px 0 !important;
        }
        .method-card {
          background: #f8fafc !important;
          border: 1px solid #cbd5e1 !important;
          border-radius: 8px !important;
          padding: 16px !important;
          position: relative !important;
        }
        .step-number {
          display: inline-block !important;
          background: #2563eb !important;
          color: #ffffff !important;
          font-size: 10px !important;
          font-weight: 800 !important;
          padding: 2px 8px !important;
          border-radius: 4px !important;
          margin-bottom: 8px !important;
        }
        .method-card h5 {
          font-size: 13px !important;
          font-weight: 700 !important;
          color: #0f172a !important;
          margin: 0 0 6px 0 !important;
        }
        .method-card p {
          font-size: 12px !important;
          color: #475569 !important;
          margin: 0 !important;
          line-height: 1.45 !important;
        }

        .stat-pill.pill-primary {
          background: linear-gradient(to bottom, #2563eb, #1d4ed8) !important;
          color: #ffffff !important;
          font-weight: bold !important;
        }
        .sig-tag {
          color: #059669 !important;
          font-weight: bold !important;
          font-size: 11.5px !important;
        }

        /* Enhanced Main Heading Header */
        .portal-main-heading {
          background: linear-gradient(135deg, #0b2545 0%, #134074 50%, #1e293b 100%) !important;
          border-radius: 8px !important;
          padding: 22px 28px !important;
          color: #ffffff !important;
          margin-bottom: 24px !important;
          box-shadow: 0 6px 18px rgba(11, 37, 69, 0.2), inset 0 1px 0 rgba(255,255,255,0.3) !important;
          border: 1px solid #0b2545 !important;
        }
        .portal-main-heading .portal-badge-title {
          display: inline-block !important;
          background: rgba(255, 255, 255, 0.15) !important;
          border: 1px solid rgba(255, 255, 255, 0.3) !important;
          color: #e0f2fe !important;
          font-size: 11px !important;
          font-weight: 800 !important;
          text-transform: uppercase !important;
          letter-spacing: 1px !important;
          padding: 3px 12px !important;
          border-radius: 12px !important;
          margin-bottom: 8px !important;
        }
        .portal-main-heading h1 {
          color: #ffffff !important;
          font-size: 22px !important;
          font-weight: 800 !important;
          margin: 0 0 6px 0 !important;
          letter-spacing: -0.4px !important;
          text-shadow: 0 2px 4px rgba(0,0,0,0.3) !important;
        }
        .portal-main-heading p {
          color: #cbd5e1 !important;
          font-size: 13px !important;
          margin: 0 0 14px 0 !important;
        }

        /* Publication-Grade Table Styling */
        .findings-table-container {
          background: #ffffff !important;
          border-radius: 8px !important;
          border: 1px solid #cbd5e1 !important;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05) !important;
          margin: 20px 0 !important;
          overflow: hidden !important;
        }
        .findings-table {
          width: 100% !important;
          border-collapse: collapse !important;
          font-size: 13px !important;
        }
        .findings-table th {
          background: linear-gradient(to bottom, #0f172a 0%, #1e293b 100%) !important;
          color: #ffffff !important;
          font-weight: 700 !important;
          font-size: 12px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          padding: 12px 16px !important;
          text-align: left !important;
          border: none !important;
        }
        .findings-table td {
          padding: 12px 16px !important;
          border-bottom: 1px solid #e2e8f0 !important;
          color: #334155 !important;
          vertical-align: middle !important;
        }
        .findings-table tr.section-header td {
          background: #f1f5f9 !important;
          font-weight: 800 !important;
          color: #0f172a !important;
          font-size: 12px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.6px !important;
          border-top: 2px solid #cbd5e1 !important;
          border-bottom: 2px solid #cbd5e1 !important;
        }
        .findings-table tr:hover td {
          background-color: #f8fafc !important;
        }
        .or-badge {
          display: inline-block !important;
          padding: 4px 10px !important;
          border-radius: 6px !important;
          font-weight: 800 !important;
          font-size: 12.5px !important;
          min-width: 60px !important;
          text-align: center !important;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1) !important;
        }
        .or-badge.high-impact {
          background: linear-gradient(135deg, #1e40af, #1d4ed8) !important;
          color: #ffffff !important;
        }
        .or-badge.medium-impact {
          background: linear-gradient(135deg, #0284c7, #0369a1) !important;
          color: #ffffff !important;
        }
        .or-badge.moderate-impact {
          background: linear-gradient(135deg, #0f766e, #115e59) !important;
          color: #ffffff !important;
        }
        .ci-box {
          font-family: 'Courier New', Courier, monospace !important;
          font-size: 12px !important;
          color: #475569 !important;
          background: #f8fafc !important;
          padding: 3px 8px !important;
          border-radius: 4px !important;
          border: 1px solid #e2e8f0 !important;
          display: inline-block !important;
        }
        .p-val-tag {
          color: #15803d !important;
          font-weight: 700 !important;
          font-size: 11.5px !important;
        }
      "))
    ),
    
    tabItems(
      # --- TAB 1: Project Report (FIRST TAB BY DEFAULT) ---
      tabItem(
        tabName = "report",
        fluidRow(
          box(
            title = "📖 Diabetes Risk Portal — Publication-Grade Project Report",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              class = "tiger-document",
              
      # --- TAB 1: Project Report (FIRST TAB BY DEFAULT) ---
      tabItem(
        tabName = "report",
        fluidRow(
          box(
            title = "🏥 DIABETES RISK PORTAL — PUBLICATION-GRADE PROJECT REPORT",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              class = "tiger-document",
              
              # Main Enhanced Heading Banner
              tags$div(
                class = "portal-main-heading",
                tags$span(class = "portal-badge-title", tags$i(class = "fas fa-award"), " CLINICAL MACHINE LEARNING STUDY"),
                tags$h1("Diabetes Risk Portal — Publication-Grade Project Report"),
                tags$p("Multivariable Logistic Regression & Decision Support System on 96,146 clean clinical evaluation records (deduplicated from the 100,000 raw dataset)"),
                tags$div(
                  class = "meta-pills-row",
                  tags$span(class = "meta-badge", tags$i(class = "fas fa-user"), " Author: Niket Banda"),
                  tags$span(class = "meta-badge", tags$i(class = "fas fa-database"), " Cohort: 96,146 Clean Records"),
                  tags$span(class = "meta-badge badge-green", tags$i(class = "fas fa-check-circle"), " 10-Fold CV Accuracy: 95.9%"),
                  tags$span(class = "meta-badge badge-blue", tags$i(class = "fas fa-chart-line"), " ROC-AUC: 96.2%")
                )
              ),
              
              # Section 1: Research Question & Core Objectives
              tags$h3("1. Primary Research Question & Analytical Objectives"),
              tags$div(
                class = "hero-research-card",
                tags$div(class = "hero-icon", tags$i(class = "fas fa-microscope")),
                tags$div(
                  class = "hero-content",
                  tags$strong("Primary Research Question"),
                  tags$p("\"Can we accurately predict whether a patient has diabetes using age, BMI, blood sugar levels, and health history—while predicting as many diabetic cases as possible?\"")
                )
              ),
              tags$p(
                "In clinical predictive modeling, early diagnosis of diabetes mellitus is essential to mitigate long-term microvascular and macrovascular complications. This project develops a publication-grade, reproducible machine learning classification pipeline in R to deliver transparent, actionable risk predictions for healthcare providers."
              ),
              tags$div(
                class = "subquestion-grid",
                tags$div(
                  class = "subquestion-card q1-card",
                  tags$span(class = "q-badge", tags$i(class = "fas fa-vial"), " Core Analytical Focus"),
                  tags$h4("Biomarker vs. Lifestyle Dominance"),
                  tags$p("Do laboratory diagnostic biomarkers (HbA1c level and blood glucose level) exert a significantly stronger predictive influence on diabetes risk than demographic and lifestyle factors (BMI, age, and smoking history)?"),
                  tags$div(class = "q-focus-pill", "🔍 ", tags$strong("Analytical Objective:"), " Quantifying relative feature importance & Odds Ratio effect sizes across laboratory vs. demographic predictors")
                )
              ),
              
              # Section 2: Methodology (4 Steps Grid)
              tags$h3("2. Four-Step Analytic Methodology"),
              tags$p(
                "To guarantee mathematical rigor and complete methodological reproducibility, the analytical pipeline follows a structured four-step workflow:"
              ),
              tags$div(
                class = "methodology-grid",
                tags$div(
                  class = "method-card",
                  tags$span(class = "step-number", "STEP 1"),
                  tags$h5("Data Cleaning & Stratification"),
                  tags$p("Categorical variables and binary flags reformatted as factors. Deduplicated raw cohort to 96,146 clean records across 4 clinical risk tiers.")
                ),
                tags$div(
                  class = "method-card",
                  tags$span(class = "step-number", "STEP 2"),
                  tags$h5("80/20 Stratified Split"),
                  tags$p("Partitioned into 80% train / 20% test using stratified sampling. Normalization parameters derived strictly from training data to prevent data leakage.")
                ),
                tags$div(
                  class = "method-card",
                  tags$span(class = "step-number", "STEP 3"),
                  tags$h5("10-Fold CV Training"),
                  tags$p("Evaluated via 10-fold cross-validation with tidymodels logistic regression. Achieved 95.9% mean CV Accuracy and 96.2% ROC-AUC.")
                ),
                tags$div(
                  class = "method-card",
                  tags$span(class = "step-number", "STEP 4"),
                  tags$h5("Sensitivity-Driven Evaluation"),
                  tags$p("Evaluated on held-out test set, prioritizing Area Under ROC (ROC-AUC) and Sensitivity (Recall) for clinical diagnostic safety.")
                )
              ),
              tags$div(
                class = "tiger-formula-box",
                tags$strong("Data Leakage Prevention Protocol:"), "\n",
                " 1. Recipe Blueprint: recipe(diabetes ~ ., data = train_data)\n",
                " 2. Parameter Calculation: Derived strictly on train_data via step_normalize()\n",
                " 3. Test Evaluation: bake(prep_recipe, new_data = test_data) using frozen training parameters"
              ),
              
              # Section 3: Key Analytical Findings & Structured Table
              tags$h3("3. Key Analytical Findings & Multivariable Odds Ratios"),
              tags$p(
                "Multivariable logistic regression demonstrated that laboratory diagnostic biomarkers—specifically HbA1c and Blood Glucose levels—are the dominant clinical predictors of diabetes status, as summarized in the publication-grade model findings table below:"
              ),
              
              # Enhanced Structured Table
              tags$div(
                class = "findings-table-container",
                tags$table(
                  class = "findings-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Domain & Clinical Predictor Variable"),
                      tags$th("Odds Ratio (OR)"),
                      tags$th("95% Confidence Interval"),
                      tags$th("p-value"),
                      tags$th("Clinical Significance & Effect Size")
                    )
                  ),
                  tags$tbody(
                    # Section Header 1
                    tags$tr(
                      class = "section-header",
                      tags$td(colspan = 5, tags$i(class = "fas fa-flask"), " Laboratory Diagnostic Biomarkers")
                    ),
                    tags$tr(
                      tags$td(tags$strong("HbA1c Level (%)")),
                      tags$td(tags$span(class = "or-badge high-impact", "10.34x")),
                      tags$td(tags$span(class = "ci-box", "[9.64 – 11.09]")),
                      tags$td(tags$span(class = "p-val-tag", "p < 0.001 ***")),
                      tags$td("Primary Clinical Predictor: Each +1.0% increase in HbA1c multiplies diabetes odds by ~10.34x.")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Blood Glucose Level (mg/dL)")),
                      tags$td(tags$span(class = "or-badge high-impact", "1.034x")),
                      tags$td(tags$span(class = "ci-box", "[1.033 – 1.035]")),
                      tags$td(tags$span(class = "p-val-tag", "p < 0.001 ***")),
                      tags$td("Continuous Glucose Driver: Each +25 mg/dL shift in glucose increases diabetes odds by ~2.37x.")
                    ),
                    
                    # Section Header 2
                    tags$tr(
                      class = "section-header",
                      tags$td(colspan = 5, tags$i(class = "fas fa-heartbeat"), " Cardiovascular Comorbidities")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Hypertension (Yes vs No)")),
                      tags$td(tags$span(class = "or-badge medium-impact", "2.15x")),
                      tags$td(tags$span(class = "ci-box", "[1.96 – 2.35]")),
                      tags$td(tags$span(class = "p-val-tag", "p < 0.001 ***")),
                      tags$td("Independent Vascular Risk: Co-existing hypertension more than doubles diabetes odds (2.15x).")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Heart Disease (Yes vs No)")),
                      tags$td(tags$span(class = "or-badge medium-impact", "2.14x")),
                      tags$td(tags$span(class = "ci-box", "[1.90 – 2.41]")),
                      tags$td(tags$span(class = "p-val-tag", "p < 0.001 ***")),
                      tags$td("Independent Cardiac Risk: History of heart disease independently doubles diabetes odds (2.14x).")
                    ),
                    
                    # Section Header 3
                    tags$tr(
                      class = "section-header",
                      tags$td(colspan = 5, tags$i(class = "fas fa-user-circle"), " Demographics & Anthropometrics")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Body Mass Index (BMI)")),
                      tags$td(tags$span(class = "or-badge moderate-impact", "1.092x")),
                      tags$td(tags$span(class = "ci-box", "[1.087 – 1.098]")),
                      tags$td(tags$span(class = "p-val-tag", "p < 0.001 ***")),
                      tags$td("Adiposity Risk Factor: Each unit increase in BMI (kg/m²) increases diabetes odds by ~9.2%.")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Age (Years)")),
                      tags$td(tags$span(class = "or-badge moderate-impact", "1.048x")),
                      tags$td(tags$span(class = "ci-box", "[1.046 – 1.050]")),
                      tags$td(tags$span(class = "p-val-tag", "p < 0.001 ***")),
                      tags$td("Demographic Baseline: Each additional year of age increases diabetes odds by ~4.8%.")
                    )
                  )
                )
              ),
              
              tags$div(
                class = "tiger-formula-box",
                tags$strong("Multivariable Logistic Regression Odds Ratio Formula:"), "\n",
                " ln( p / (1 - p) ) = β0 + β1(HbA1c) + β2(Glucose) + β3(Hypertension) + β4(HeartDisease) + β5(BMI) + β6(Age)\n",
                " Odds Ratio (OR) = exp(β_i)  |  95% CI = exp( β_i ± 1.96 × SE(β_i) )"
              ),
              
              # Section 4: Model Sensitivity & Clinical Screening Impact
              tags$h3("4. Clinical Sensitivity & Diagnostic Impact"),
              tags$p(
                "In population-level screening programs, prioritizing clinical sensitivity (recall) minimizes False Negatives—preventing undetected diabetic patients from developing unmonitored cardiovascular and metabolic complications. The cross-validated model provides a stable, highly scalable decision-support framework to empower early clinical intervention."
              )
            )
          )
        )
      ),

      # --- TAB 2: Patient Overview ---
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_patients", width = 4),
          valueBoxOutput("pct_diabetic", width = 4),
          valueBoxOutput("pct_risk", width = 4)
        ),
        fluidRow(
          box(
            title = "Patient Population Risk Tier Distribution", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("risk_dist_plot", height = "360px"),
            tags$div(
              class = "chart-desc-box",
              tags$div(class = "desc-header", tags$i(class = "fas fa-chart-bar"), " Risk Tier Stratification Insights"),
              tags$p("Categorizes 96,146 cataloged patients across 4 risk tiers using clinical indicators. High Risk patients have HbA1c ≥ 5.7% or Blood Glucose ≥ 140 mg/dL, while Moderate Risk denotes BMI ≥ 25 kg/m².")
            )
          ),
          box(
            title = "HbA1c Levels Across Risk Tiers", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("clinical_metrics_plot", height = "360px"),
            tags$div(
              class = "chart-desc-box",
              tags$div(class = "desc-header", tags$i(class = "fas fa-vial"), " Biomarker Elevation Metrics"),
              tags$p("Compares glycated hemoglobin (HbA1c) levels across risk classifications. Median HbA1c escalates sharply in the diabetic cohort (≥ 6.5%), confirming HbA1c as the primary metabolic risk predictor.")
            )
          )
        ),
        fluidRow(
          box(
            title = "📊 Cohort Risk Stratification Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              class = "overview-summary-card",
              tags$div(class = "summary-badge", "Population Stratification"),
              tags$p(
                class = "readable-desc-text",
                "Analysis of the 96,146 patient cohort indicates that while overall diabetes prevalence is 8.82%, a staggering 85.21% of non-diabetic individuals exhibit moderate to high metabolic risk, highlighting critical opportunities for early preventive intervention."
              )
            )
          ),
          box(
            title = "🩺 Clinical Biomarker Elevation Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              class = "overview-summary-card",
              tags$div(class = "summary-badge badge-blue", "Biomarker Correlation"),
              tags$p(
                class = "readable-desc-text",
                "HbA1c concentration serves as the core diagnostic metric. Non-diabetic cohorts center within normal ranges (< 5.7% or 5.7–6.4%), whereas diabetic individuals display marked escalation (mean HbA1c > 6.9%), supporting multivariable risk modeling."
              )
            )
          )
        )
      ),
      
      # --- TAB 3: Demographics & Lifestyle ---
      tabItem(
        tabName = "demographics",
        fluidRow(
          box(
            title = "Diabetes Prevalence by Age Group", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("age_plot", height = "360px")
          ),
          box(
            title = "Risk Tier Distribution by Smoking History", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("smoking_plot", height = "360px")
          )
        ),
        fluidRow(
          box(
            title = "📊 Age Group Risk Trend Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              class = "overview-summary-card",
              tags$div(class = "summary-badge", "Demographic Profile"),
              tags$p(
                class = "readable-desc-text",
                "Diabetes prevalence demonstrates a strong non-linear age escalation. While prevalence remains below 3% in cohorts under age 30, it surges to over 15% in individuals aged 60 and older, confirming age as a dominant baseline demographic risk vector."
              )
            )
          ),
          box(
            title = "🚬 Lifestyle & Smoking Exposure Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              class = "overview-summary-card",
              tags$div(class = "summary-badge badge-blue", "Lifestyle Exposure"),
              tags$p(
                class = "readable-desc-text",
                "Patients with a history of former or current smoking exhibit a higher proportion of Moderate and High Risk metabolic tiers compared to non-smokers, reflecting cumulative vascular and metabolic stress."
              )
            )
          )
        )
      ),
      
      # --- TAB 4: Biomarker Correlation ---
      tabItem(
        tabName = "biomarkers",
        fluidRow(
          box(
            title = "Clinical Biomarker Interaction (Sampled Cohort)", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("biomarker_scatter", height = "360px"),
            helpText(style = "text-align: center; font-size: 11px; color: #555; margin-top: 5px;", 
                     "Visualizing a random sample of 2,000 patients for responsive interaction.")
          ),
          box(
            title = "Cardiovascular Comorbidities & Diabetes Risk", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("comorbidity_plot", height = "360px")
          )
        ),
        fluidRow(
          box(
            title = "🧪 Glycemic Biomarker Interaction Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              class = "overview-summary-card",
              tags$div(class = "summary-badge", "Biomarker Interaction"),
              tags$p(
                class = "readable-desc-text",
                "HbA1c and Blood Glucose levels exhibit a strong positive co-elevation. Diabetic individuals cluster almost exclusively in the upper-right quadrant (HbA1c ≥ 6.5%, Glucose ≥ 140 mg/dL), confirming dual-biomarker thresholds as primary diagnostic boundaries."
              )
            )
          ),
          box(
            title = "🫀 Cardiovascular Comorbidity Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              class = "overview-summary-card",
              tags$div(class = "summary-badge badge-blue", "Cardiovascular Risk"),
              tags$p(
                class = "readable-desc-text",
                "Co-existing cardiovascular conditions markedly amplify diabetes risk. Patients with both Hypertension and Heart Disease present more than double the diabetes prevalence of non-hypertensive patients, highlighting vascular damage as an independent risk driver."
              )
            )
          )
        )
      ),
      
      # --- TAB 5: Clinical Intervention Simulator ---
      tabItem(
        tabName = "simulator",
        fluidRow(
          box(
            title = "Simulation Parameters", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 4,
            sliderInput(
              "sim_bmi_drop", 
              "Population BMI Reduction:",
              min = 0, 
              max = 20, 
              value = 0, 
              step = 1, 
              post = "%"
            ),
            sliderInput(
              "sim_hba1c_drop", 
              "Glycemic Control (HbA1c Reduction):",
              min = 0.0, 
              max = 1.0, 
              value = 0.0, 
              step = 0.1, 
              post = "%"
            ),
            tags$p(
              style = "margin-top: 15px; color: #333; font-size: 13.5px; line-height: 1.5;",
              "This simulator models counterfactual public health interventions. Shifting BMI and HbA1c distributions downward estimates population-level case reductions using baseline multivariable logistic regression coefficients."
            ),
            hr(style = "border-top: 1px solid #a0a0a0; border-bottom: 1px solid #fff;"),
            tags$div(
              style = "background: linear-gradient(to bottom, #fff8e8, #fff0c2); border: 1px solid #b78a00; padding: 12px; border-radius: 5px; color: #665000; font-size: 12px; box-shadow: inset 0 1px 0 #fff, 0 2px 4px rgba(0,0,0,0.06); text-shadow: 0 1px 0 #fff;",
              tags$strong("Note:"), " The model controls for patient age, blood glucose level, hypertension history, and heart disease history."
            )
          ),
          box(
            title = "Simulated Population Impact Study", 
            status = "success", 
            solidHeader = TRUE, 
            width = 8,
            uiOutput("sim_kpi_cards"),
            uiOutput("sim_text"),
            hr(style = "border-top: 1px solid #a0a0a0; border-bottom: 1px solid #fff;"),
            plotlyOutput("sim_plot", height = "320px"),
            tags$div(
              class = "overview-summary-card",
              style = "margin-top: 15px;",
              tags$div(class = "summary-badge badge-blue", "Public Health ROI & Impact"),
              tags$p(
                class = "readable-desc-text",
                "Counterfactual modeling demonstrates significant healthcare leverage. Combining targeted BMI and glycemic control shifts prevents thousands of diabetes diagnoses across 96,146 clean clinical evaluation records (deduplicated from the 100,000 raw dataset), mitigating long-term microvascular healthcare expenditure."
              )
            )
          )
        )
      ),

      # --- TAB 6: Project Conclusion (LAST TAB) ---
      tabItem(
        tabName = "conclusion",
        fluidRow(
          box(
            title = "🏁 CLINICAL MACHINE LEARNING STUDY — ANALYTICAL CONCLUSION",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              class = "tiger-document",
              
              # Conclusion Hero Banner
              tags$div(
                class = "portal-main-heading",
                style = "background: linear-gradient(135deg, #064e3b 0%, #047857 50%, #0f172a 100%); border-color: #047857;",
                tags$span(class = "portal-badge-title", tags$i(class = "fas fa-check-double"), " FINAL RESEARCH VERDICT"),
                tags$h1("Project Conclusion: Biomarkers Drive Primary Predictive Power"),
                tags$p("Empirical Findings & Diagnostic Synthesis Across 96,146 Patient Evaluation Cohort"),
                tags$div(
                  class = "meta-pills-row",
                  tags$span(class = "meta-badge", tags$i(class = "fas fa-user"), " Author: Niket Banda"),
                  tags$span(class = "meta-badge badge-green", tags$i(class = "fas fa-check-circle"), " Claim Status: VERIFIED"),
                  tags$span(class = "meta-badge badge-blue", tags$i(class = "fas fa-chart-line"), " ROC-AUC: 96.2%")
                )
              ),
              
              # Section 1: Synthesis of the Primary Claim & Research Question
              tags$h3("1. Primary Research Question & Claim Verdict"),
              tags$div(
                class = "hero-research-card",
                style = "border-left-color: #10b981; background: linear-gradient(to right, #ecfdf5, #f8fafc); border-color: #a7f3d0;",
                tags$div(class = "hero-icon", style = "color: #059669;", tags$i(class = "fas fa-award")),
                tags$div(
                  class = "hero-content",
                  tags$strong(style = "color: #047857;", "Conclusion to Primary Research Claim"),
                  tags$p("\"YES — We can accurately predict diabetes status (95.9% CV Accuracy, 96.2% ROC-AUC). Glycated hemoglobin (HbA1c) and blood glucose levels exert a significantly stronger predictive influence than demographic or lifestyle factors.\"")
                )
              ),
              tags$p(
                "Multivariable logistic regression on 96,146 deduplicated clinical records confirms that laboratory diagnostic biomarkers are the single most dominant risk predictors. Each +1.0% elevation in HbA1c multiplies diabetes odds by 10.34x (95% CI: [9.64 – 11.09], p < 0.001), while fasting/random blood glucose increases odds continuously by 1.034x per mg/dL. In contrast, demographic variables such as BMI (OR: 1.092) and Age (OR: 1.048) represent secondary continuous risk multipliers."
              ),
              
              # 3 Summary Pillars Grid
              tags$div(
                class = "subquestion-grid",
                tags$div(
                  class = "subquestion-card q1-card",
                  style = "border-top-color: #10b981;",
                  tags$span(class = "q-badge", style = "background: #d1fae5; color: #047857; border-color: #a7f3d0;", tags$i(class = "fas fa-flask"), " Pillar 01: Biomarker Dominance"),
                  tags$h4("Laboratory Metrics Outweigh Demographics"),
                  tags$p("HbA1c and Blood Glucose drive over 80% of model log-odds variance. Metabolic diagnostic thresholds (HbA1c ≥ 6.5%, Glucose ≥ 140 mg/dL) serve as clear physiological boundary conditions for classification."),
                  tags$div(class = "q-focus-pill", "🧪 ", tags$strong("Key Metric:"), " HbA1c OR: 10.34x [9.64 – 11.09]")
                ),
                tags$div(
                  class = "subquestion-card q2-card",
                  style = "border-top-color: #0284c7;",
                  tags$span(class = "q-badge", style = "background: #e0f2fe; color: #0369a1; border-color: #bae6fd;", tags$i(class = "fas fa-shield-alt"), " Pillar 02: High-Sensitivity Screening"),
                  tags$h4("Prioritizing Diagnostic Recall"),
                  tags$p("In clinical population screening, missing a true diabetic patient carries severe microvascular risk. The model achieves 96.2% ROC-AUC, enabling threshold tuning to minimize False Negatives."),
                  tags$div(class = "q-focus-pill", "🛡️ ", tags$strong("Key Metric:"), " 10-Fold CV ROC-AUC: 96.2%")
                ),
                tags$div(
                  class = "subquestion-card q1-card",
                  style = "border-top-color: #f59e0b;",
                  tags$span(class = "q-badge", style = "background: #fef3c7; color: #b45309; border-color: #fde68a;", tags$i(class = "fas fa-user-shield"), " Pillar 03: Preventive Window"),
                  tags$h4("Targeting High-Risk Non-Diabetics"),
                  tags$p("While diabetes prevalence is 8.82%, 85.21% of non-diabetic patients exhibit moderate to high risk. Targeted interventions (such as a 5–10% population BMI reduction) offer vital preventive leverage."),
                  tags$div(class = "q-focus-pill", "🎯 ", tags$strong("Key Metric:"), " At-Risk Cohort: 85.21%")
                )
              ),
              
              # Section 2: Model Performance Benchmarks Table
              tags$h3("2. Final Model Benchmarks & Specifications"),
              tags$div(
                class = "findings-table-container",
                tags$table(
                  class = "findings-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Evaluation Domain"),
                      tags$th("Metric / Result"),
                      tags$th("Methodological Specification")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$strong("Total Cohort Size")),
                      tags$td(tags$span(class = "or-badge medium-impact", "96,146 Patients")),
                      tags$td("Deduplicated clinical records evaluated across 80/20 train/test split.")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Overall Diabetes Prevalence")),
                      tags$td(tags$span(class = "or-badge moderate-impact", "8.82%")),
                      tags$td("Stratified sampling preserving class balance across partitions.")
                    ),
                    tags$tr(
                      tags$td(tags$strong("10-Fold CV Accuracy")),
                      tags$td(tags$span(class = "or-badge high-impact", "95.9%")),
                      tags$td("Resampled evaluation across 10 cross-validation training folds.")
                    ),
                    tags$tr(
                      tags$td(tags$strong("10-Fold CV ROC-AUC")),
                      tags$td(tags$span(class = "or-badge high-impact", "96.2%")),
                      tags$td("Exceptional discrimination capacity across probability decision cutoffs.")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Primary Clinical Predictor")),
                      tags$td(tags$span(class = "or-badge high-impact", "HbA1c (OR: 10.34x)")),
                      tags$td("95% CI: [9.64 – 11.09], p < 0.001 ***")
                    )
                  )
                )
              ),
              
              # Section 3: Catalog Entry Box
              tags$h3("3. Class Showcase Catalog Entry"),
              tags$div(
                class = "tiger-formula-box",
                style = "font-size: 13.5px; font-weight: 600; color: #064e3b; background: #ecfdf5; border-color: #10b981; line-height: 1.6;",
                "Project Catalog Summary (Author: Niket Banda): Multivariable logistic regression modeling on 96,146 clinical records identified HbA1c (OR: 10.34, 95% CI: 9.64–11.09), blood glucose level (OR: 1.034, 95% CI: 1.033–1.035), and hypertension (OR: 2.15, 95% CI: 1.96–2.35) as primary diagnostic risk drivers. Evaluated across 10-fold cross-validation, the pipeline achieved 96.2% ROC-AUC and 95.9% accuracy with high clinical sensitivity optimized for diagnostic screening."
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# Server Logic
# ==============================================================================
server <- function(input, output, session) {
  
  # --- TAB: Patient Overview Server Output ---
  
  # KPI 1: Total Patient Count (Classic Blue Gel)
  output$total_patients <- renderValueBox({
    valueBox(
      value = format(nrow(df), big.mark = ","),
      subtitle = "Total Patients Cataloged",
      icon = icon("users"),
      color = "navy"
    )
  })
  
  # KPI 2: Percent Diabetic (Classic Red Gel)
  output$pct_diabetic <- renderValueBox({
    diabetic_pct <- (sum(df$diabetes == 1) / nrow(df)) * 100
    valueBox(
      value = paste0(round(diabetic_pct, 2), "%"),
      subtitle = "Prevalence of Diabetes",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  # KPI 3: Percent High or Moderate Risk (Classic Yellow/Orange Gel)
  output$pct_risk <- renderValueBox({
    risk_pct <- (sum(df$risk_tier %in% c("High Risk", "Moderate Risk")) / nrow(df)) * 100
    valueBox(
      value = paste0(round(risk_pct, 2), "%"),
      subtitle = "Cohort At Risk (Mod / High)",
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  
  # Chart 1: Risk Tier Distribution Bar Chart
  output$risk_dist_plot <- renderPlotly({
    risk_summary <- df %>%
      group_by(risk_tier) %>%
      summarize(Count = n(), .groups = "drop") %>%
      mutate(Percentage = (Count / sum(Count)) * 100)
    
    plot_ly(
      risk_summary, 
      x = ~risk_tier, 
      y = ~Percentage, 
      type = "bar",
      color = ~risk_tier, 
      colors = c("#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
      text = ~paste0(round(Percentage, 2), "%"),
      textposition = 'auto',
      hoverinfo = "text",
      hovertext = ~paste(
        "Risk Tier:", risk_tier,
        "<br>Patients:", format(Count, big.mark = ","),
        "<br>Percentage:", round(Percentage, 2), "%"
      )
    ) %>%
      layout(
        xaxis = list(title = "Risk Classification", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        yaxis = list(title = "Cohort Percentage (%)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 20, b = 20, l = 40, r = 20)
      )
  })
  
  # Chart 2: HbA1c Level by Risk Tier Box Plot
  output$clinical_metrics_plot <- renderPlotly({
    plot_ly(
      df, 
      x = ~risk_tier, 
      y = ~HbA1c_level, 
      color = ~risk_tier, 
      type = "box",
      colors = c("#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
      boxpoints = FALSE # Hide outliers to optimize rendering speed and visual cleanliness
    ) %>%
      layout(
        xaxis = list(title = "Risk Classification", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        yaxis = list(title = "HbA1c Level (%)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 20, b = 20, l = 40, r = 20),
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 6.5,
            y1 = 6.5,
            line = list(color = "#d9534f", width = 2, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = 0.02,
            y = 6.5,
            xref = "paper",
            yref = "y",
            text = "Diabetes Threshold (6.5%)",
            showarrow = FALSE,
            font = list(color = "#d9534f", size = 10, family = "Lucida Grande, sans-serif"),
            yshift = 8,
            xanchor = "left"
          )
        )
      )
  })
  
  # --- TAB: Simulator Server Output ---
  
  # Reactive calculations for BMI and HbA1c shift simulation
  sim_data <- reactive({
    bmi_drop_pct <- if (!is.null(input$sim_bmi_drop)) input$sim_bmi_drop else 0
    hba1c_drop <- if (!is.null(input$sim_hba1c_drop)) input$sim_hba1c_drop else 0
    
    # Counterfactual dataset: lower population BMI and HbA1c
    df_sim <- df %>%
      mutate(
        bmi = bmi * (1 - (bmi_drop_pct / 100)),
        HbA1c_level = pmax(3.5, HbA1c_level - hba1c_drop)
      )
    
    # Predict diabetes risk under counterfactual scenario
    probs <- predict(model, newdata = df_sim, type = "response")
    
    # Compute expected number of cases
    expected_cases <- sum(probs)
    baseline_cases <- sum(df$diabetes)
    prevented_cases <- max(0, baseline_cases - expected_cases)
    new_prevalence <- (expected_cases / nrow(df)) * 100
    baseline_prev <- (baseline_cases / nrow(df)) * 100
    prevented_pct <- ifelse(baseline_cases > 0, (prevented_cases / baseline_cases) * 100, 0)
    
    list(
      expected_cases = expected_cases,
      baseline_cases = baseline_cases,
      prevented_cases = prevented_cases,
      new_prevalence = new_prevalence,
      baseline_prev = baseline_prev,
      prevented_pct = prevented_pct,
      reduction_bmi = bmi_drop_pct,
      reduction_hba1c = hba1c_drop
    )
  })

  # Render KPI Callout Cards
  output$sim_kpi_cards <- renderUI({
    sim <- sim_data()
    
    fluidRow(
      column(4, valueBox(
        value = format(round(sim$baseline_cases), big.mark = ","),
        subtitle = paste0("Baseline Cases (", round(sim$baseline_prev, 2), "%)"),
        icon = icon("users"),
        color = "navy",
        width = 12
      )),
      column(4, valueBox(
        value = format(round(sim$expected_cases), big.mark = ","),
        subtitle = paste0("Simulated Cases (", round(sim$new_prevalence, 2), "%)"),
        icon = icon("heartbeat"),
        color = "red",
        width = 12
      )),
      column(4, valueBox(
        value = format(round(sim$prevented_cases), big.mark = ","),
        subtitle = paste0("Cases Saved (−", round(sim$prevented_pct, 1), "%)"),
        icon = icon("shield-alt"),
        color = "green",
        width = 12
      ))
    )
  })
  
  # Render the simulation dynamic text report
  output$sim_text <- renderUI({
    sim <- sim_data()
    
    prevented <- round(sim$prevented_cases)
    baseline_prev <- round(sim$baseline_prev, 2)
    new_prev <- round(sim$new_prevalence, 2)
    
    interventions <- c()
    if (sim$reduction_bmi > 0) interventions <- c(interventions, paste0("BMI reduction of <strong>", sim$reduction_bmi, "%</strong>"))
    if (sim$reduction_hba1c > 0) interventions <- c(interventions, paste0("HbA1c reduction of <strong>-", sim$reduction_hba1c, "%</strong>"))
    
    interv_str <- if (length(interventions) == 0) "No population interventions currently applied." else paste(interventions, collapse = " combined with ")
    
    tags$div(
      class = "sim-report-box",
      tags$h4(tags$strong("Simulation Findings")),
      tags$p(
        style = "font-size: 14.5px; margin-bottom: 0; line-height: 1.5; color: #1e293b;",
        HTML(paste0(
          "Under a scenario of ", interv_str, ", the model predicts preventing ",
          "<strong>", format(prevented, big.mark = ","), "</strong> cases of diabetes (a <strong>",
          round(sim$prevented_pct, 1), "%</strong> reduction in active cases). ",
          "Cohort prevalence shifts from <strong>", baseline_prev, "%</strong> to <strong>", new_prev, "%</strong>."
        ))
      )
    )
  })
  
  # Render the simulation comparative bar plot
  output$sim_plot <- renderPlotly({
    sim <- sim_data()
    
    plot_df <- tibble(
      Scenario = c("Baseline (Current)", "Simulated Intervention"),
      Cases = c(sim$baseline_cases, sim$expected_cases),
      Prevalence = c(sim$baseline_prev, sim$new_prevalence)
    )
    
    plot_ly(
      plot_df, 
      x = ~Scenario, 
      y = ~Cases, 
      type = "bar",
      color = ~Scenario, 
      colors = c("#1e40af", "#059669"),
      text = ~paste0(format(round(Cases), big.mark = ","), "\n(", round(Prevalence, 2), "%)"),
      textposition = 'auto',
      hoverinfo = "text",
      hovertext = ~paste(
        "Scenario:", Scenario,
        "<br>Expected Cases:", format(round(Cases), big.mark = ","),
        "<br>Prevalence:", round(Prevalence, 2), "%"
      )
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Expected Diabetes Cases", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 30, b = 20, l = 50, r = 20)
      )
  })
  
  # --- TAB: Demographics & Lifestyle Server Output ---
  
  # Chart 3: Diabetes Prevalence by Age Group
  output$age_plot <- renderPlotly({
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
      
    plot_ly(
      df_age, 
      x = ~age_group, 
      y = ~Prevalence, 
      type = "bar",
      marker = list(
        color = "#4fa0f9",
        line = list(color = "#005ccb", width = 1)
      ),
      hoverinfo = "text",
      hovertext = ~paste(
        "Age Group:", age_group,
        "<br>Diabetes Prevalence:", round(Prevalence, 2), "%",
        "<br>Total Patients:", format(Total, big.mark = ","),
        "<br>Diabetic Count:", format(Diabetic_Count, big.mark = ",")
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Age Classification", 
          titlefont = list(size = 11, family = "Lucida Grande, sans-serif"),
          tickvals = c("Pediatric (<18)", "Young Adult (18-34)", "Adult (35-49)", "Middle-Aged (50-64)", "Senior (65+)"),
          ticktext = c("Pediatric<br>(<18)", "Young Adult<br>(18-34)", "Adult<br>(35-49)", "Middle-Aged<br>(50-64)", "Senior<br>(65+)")
        ),
        yaxis = list(title = "Prevalence Rate (%)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 20, b = 60, l = 40, r = 20)
      )
  })
  
  # Chart 4: Risk Tier Stacked Bar Chart by Smoking History
  output$smoking_plot <- renderPlotly({
    df_smoking <- df %>%
      group_by(smoking_history, risk_tier) %>%
      summarize(Count = n(), .groups = "drop") %>%
      group_by(smoking_history) %>%
      mutate(Percentage = (Count / sum(Count)) * 100)
      
    plot_ly(
      df_smoking, 
      x = ~smoking_history, 
      y = ~Percentage, 
      type = "bar",
      color = ~risk_tier,
      colors = c("#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
      hoverinfo = "text",
      hovertext = ~paste(
        "Smoking History:", smoking_history,
        "<br>Risk Category:", risk_tier,
        "<br>Patient Count:", format(Count, big.mark = ","),
        "<br>Percentage:", round(Percentage, 2), "%"
      )
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Smoking Category", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        yaxis = list(title = "Cohort Percentage (%)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        legend = list(title = list(text = "<b>Risk Tier</b>"), font = list(size = 10, family = "Lucida Grande, sans-serif")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 20, b = 40, l = 40, r = 20)
      )
  })
  
  # --- TAB: Biomarker Correlation Server Output ---
  
  # Chart 5: Biomarker Scatter Plot (HbA1c vs Blood Glucose Level)
  output$biomarker_scatter <- renderPlotly({
    set.seed(42)
    df_sample <- df %>%
      sample_n(2000) %>%
      mutate(Diabetes_Status = ifelse(diabetes == 1, "Diabetic", "Non-Diabetic"))
      
    plot_ly(
      df_sample, 
      x = ~blood_glucose_level, 
      y = ~HbA1c_level, 
      type = "scatter",
      mode = "markers",
      color = ~Diabetes_Status,
      colors = c("#ff5d5d", "#4fa0f9"),
      marker = list(size = 7, opacity = 0.75, line = list(width = 0.75, color = "#fff")),
      hoverinfo = "text",
      hovertext = ~paste(
        "Status:", Diabetes_Status,
        "<br>HbA1c:", HbA1c_level, "%",
        "<br>Glucose:", blood_glucose_level, "mg/dL",
        "<br>Age:", age, "yrs",
        "<br>BMI:", bmi
      )
    ) %>%
      add_trace(
        x = c(min(df_sample$blood_glucose_level), max(df_sample$blood_glucose_level)),
        y = c(6.5, 6.5),
        type = "scatter",
        mode = "lines",
        name = "Diabetes Threshold (6.5%)",
        line = list(color = "#d9534f", width = 2, dash = "dash"),
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Blood Glucose Level (mg/dL)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        yaxis = list(title = "HbA1c Level (%)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        legend = list(title = list(text = "<b>Diagnosis</b>"), font = list(size = 10, family = "Lucida Grande, sans-serif")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 20, b = 40, l = 40, r = 20)
      )
  })
  
  # Chart 6: Cardiovascular Comorbidities & Diabetes Risk Bar Chart
  output$comorbidity_plot <- renderPlotly({
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
      
    plot_ly(
      df_comorb, 
      x = ~Comorbidity, 
      y = ~Prevalence, 
      type = "bar",
      marker = list(
        color = "#ff9833", # Orange gel color
        line = list(color = "#b34a00", width = 1)
      ),
      hoverinfo = "text",
      hovertext = ~paste(
        "Comorbidities:", Comorbidity,
        "<br>Diabetes Prevalence:", round(Prevalence, 2), "%",
        "<br>Cohort Patients:", format(Total, big.mark = ","),
        "<br>Diabetic Count:", format(Diabetic_Count, big.mark = ",")
      )
    ) %>%
      layout(
        xaxis = list(title = "Comorbidity Profile", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        yaxis = list(title = "Prevalence Rate (%)", titlefont = list(size = 11, family = "Lucida Grande, sans-serif")),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 20, b = 40, l = 40, r = 20)
      )
  })
}

# 3. Launch Shiny Application
shinyApp(ui = ui, server = server)
