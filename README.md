# PhenoMeNals – Phenology Memory Signals

<!--img src="man/figures/logo.png" alt="phenomenals logo" width="250" align="right"/> -->

[![License: CC BY-NC 3.0](https://img.shields.io/badge/License-CC%20BY--NC%203.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/3.0/)
[![Platform](https://img.shields.io/badge/platform-Windows--only-blue)](https://microsoft.com)  
[![Language](https://img.shields.io/badge/language-R%20%7C%20C%23-purple)](https://cran.r-project.org/)  
![Status](https://img.shields.io/badge/status-active-brightgreen)

---

## 📖 Overview
**PhenoMeNals** is a phenology-aware predictive framework for **grapevine yield and quality forecasting**, integrating **phenology simulation**, **eco-physiological functions**, and **statistical modeling**.

The framework explicitly accounts for the **two-year reproductive cycle of grapevine**, combining a **dormancy-forcing phenology model** with cumulative “memory signals” derived from environmental cues (temperature, radiation, water status, biotic and abiotic stresses). These signals are aligned along a standardized phenological timeline and weighted by their correlation with yield- and quality-related traits, enabling **biologically meaningful predictions** and interpretation of carry-over effects.

> **Note:** PhenoMeNals is released as an **R package** with **C# routines** invoked from R. It is currently available only on **Windows**, but cross-platform compatibility is under development.

---

## Highlights

- 🍇 Predicts grapevine yield and quality using **phenology-aware environmental signals**
- 🧠 Integrates phenology simulation, eco-physiological functions, and statistical modeling
- 📈 Accurate forecasting across **multiple sites, varieties, and years**
- 🔍 Analyzes **carry-over effects** on yield and quality
- ⚙️ Hybrid architecture: **R interface + C# computation**
- 🪟 **Windows-only** (Mac/Linux support coming soon)

---

## Installation

PhenoMeNals is available from GitHub. You need:

- R (≥ 4.0)
- `devtools` for installation

```r
install.packages("devtools")
devtools::install_github("GeoModelLab/phenomenals")
```

This installs the R interface and the compiled C# executable backend (Windows only).

---

## Getting Started

You can start with the included sample datasets:

```r
library(phenomenals)

head(colliOrientali)
head(bbchSample)
```

Calibrate phenology with:

```r
res <- phenologyCalibration(
  weather_data = colliOrientali,
  referenceBBCH = bbchSample,
  phenomenalsParameters = phenomenalsParameters,
  sites = "ColliOrientali",
  varieties = "Carmenere",
  iterations = 50,
  start_year = 2004,
  end_year = 2015,
  timestep = "daily"
)
```

Run the full pipeline with:

```r
res <- runPhenomenals(
  weather_data = colliOrientali,
  target_data = targetSample,
  phenomenalsParameters = phenomenalsParameters,
  sites = "ColliOrientali",
  varieties = "Carmenere",
  timestep = "daily",
  target_traits = c("yield"),
  rolling_window = 5,
  evaluation_range = list(c(0, 200)),
  multicollinearity_threshold = 0.8,
  max_phenomenals = 4,
  bin_size = 1
)
```

---

## Documentation

Use `?phenologyCalibration` and `?runPhenomenals` to access documentation inside R.

To explore more, visit the pkgdown website:

📚 https://geomodellab.github.io/phenomenals

---

## License

Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0)

---

## Authors

*Simone Bregaglio & Sofia Bajocco*  
© GeoModelLab, CREA 2025

---

