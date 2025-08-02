<p align="center">
  <img src="docs/images/phenomenals_logo.png" alt="PhenoMeNals logo" width="200"/>
</p>

# PhenoMeNals - Phenology Memory sigNals for Grapevine Yield and Quality Prediction

[![License: CC BY-NC 3.0](https://img.shields.io/badge/License-CC%20BY--NC%203.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/3.0/)
[![Platform](https://img.shields.io/badge/platform-Windows--only-blue)](https://microsoft.com)  
[![Language](https://img.shields.io/badge/language-R%20%7C%20C%23-purple)](https://cran.r-project.org/)  
![Status](https://img.shields.io/badge/status-active-brightgreen)

---

## 📖 Overview

**PhenoMeNals** is an open-source framework for **predicting grapevine yield and quality traits**.  
It integrates **two-season phenology modeling**, **ecophysiological functions**, and **statistical methods** to generate cumulative environmental memory signals (PhenoMeNals) driving reproductive processes.

---

## Table of Contents

- [Highlights](#highlights)
- [Description](#description)
- [Installation](#installation)
- [Getting Started](#getting-started)
- [Equations](#equations)
- [Support](#support)
- [License](#license)
- [How It Works](#how-it-works)

---

## Highlights

- 🌱 **Two-season chill-forcing phenology model** (dormancy → harvest)  
- 🧠 **8 environmental signals**:  
  `TempF`, `ColdF`, `HeatF`, `LightF`, `WindF`, `DiseaseF`, `VPDefF`, `DroughtF`  
- ⚙️ Signals are **sigmoid-normalized**, **weighted by correlation** with target traits, and **aggregated into memory signals**  
- 🔬 Handles multicollinearity (VIF) and performs **automatic feature selection (AIC)**  
- 🪟 **Windows-only** (C# core) – cross-platform support in development

---

## Description

**PhenoMeNals** addresses the challenge of integrating weather effects across grapevine's **two-year reproductive cycle**, where yield components are determined across multiple phenophases.

### Workflow

1. **Calibrate phenology:** BBCH observations calibrate a chill-forcing model providing a standardized phenological timeline (0–400 units across two seasons).
2. **Compute environmental functions:**  
   Each environmental driver is expressed as a daily function (e.g., temperature forcing, vapor pressure deficit stress, etc.).
3. **Normalize and weight:**  
   Signals are normalized using long-term percentile distributions and weighted by correlation strength with the target trait.
4. **Aggregate into memory signals:**  
   Weighted daily values are cumulatively summed along the phenological timeline to form the **PhenoMeNals**, which are then used as predictors.

<figure>
  <p align="center">
    <img src="docs/images/phenomenals_schema.png" width="600">
  </p>
  <figcaption align="center"><em>PhenoMeNals workflow: from phenology to cumulative memory signals</em></figcaption>
</figure>

---

## Installation

> ⚠️ **Platform notice:**  
> PhenoMeNals currently runs **only on Windows** due to its compiled C# backend.  
> macOS/Linux support is under development.

### 1. Install R

Make sure you have **R (≥ 4.0)** installed:  
🔗 [https://cran.r-project.org/](https://cran.r-project.org/)

### 2. Install Required R Packages

```r
install.packages(c("devtools", "caret", "MASS", "data.table"))
