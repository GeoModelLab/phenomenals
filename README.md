# The PhenoMeNals Framework for Grapevine Yield and Quality Prediction

**Simone Bregaglio¹\*, Sofia Bajocco¹**  
¹ CREA - Council for Agricultural Research and Economics, Research Centre for Agriculture and Environment, Italy  
\* corresponding author: simoneugomaria.bregaglio@crea.gov.it  

---

## 📖 Abstract

Timely prediction of grapevine yield and quality is a major scientific challenge with direct implications for vineyard management, enological planning, and winery logistics. Despite numerous advances, most existing approaches remain constrained to specific sites or varieties and do not explicitly account for the two-year grapevine cycle, thereby overlooking the temporal dynamics underlying reproductive physiology.

We introduce **PhenoMeNals**, an open-source framework integrating **phenology simulation**, **ecophysiological functions**, and **statistical modeling** to capture environmental carry-over effects. The coupled dormancy-forcing model, calibrated with BBCH observations (RMSE 6-9 days), provides the temporal structure spanning the two-season cycle. Weather cues are sigmoidal-normalized, weighted by correlation strength with traits, and cumulatively summed along the phenological timeline to derive memory signals (*PhenoMeNals*). 

Multicollinearity is addressed through pairwise filtering and stepwise feature selection, and model robustness is evaluated using **leave-one-out cross-validation**. The framework was validated on multi-site, multi-variety datasets for yield and quality traits, achieving R² consistently above 0.8. 

**Keywords**: grapevine phenology, memory signals, yield prediction, ecophysiological functions, BBCH, LOOCV, open-source R package.

---

## 🌱 Overview

**PhenoMeNals** integrates:

1. **Phenology simulation** (two-season dormancy-forcing model)
2. **Eco-physiological functions** aligned with phenology:
   - Temperature suitability (TempF)
   - Cold and heat stress (ColdF, HeatF)
   - Light and vapor pressure deficit limitations (LightF, VPDefF)
   - Drought index (DroughtF)
   - Wind and disease stress (WindF, DiseaseF)
3. **Statistical methods** to construct memory signals and perform robust predictions.

The framework is implemented as an **R package** with C# backend for Windows.

---

## 🔢 Core Equations

### 1. Temperature suitability (TempF)

\[
TempF = 
\begin{cases}
0 & T < T_{min} \, \text{or} \, T > T_{max} \\
\frac{(T_{max} - T)}{(T_{max} - T_{opt})}
\left( \frac{T - T_{min}}{T_{opt} - T_{min}} \right)^{\frac{T_{opt} - T_{min}}{T_{max} - T_{opt}}}
& T_{min} \leq T \leq T_{max}
\end{cases}
\]

### 2. Cold and Heat stress (ColdF, HeatF)

\[
ColdF = \frac{1}{1 + e^{-k_{cold}(T - T_{50})}}
\quad
HeatF = \frac{1}{1 + e^{k_{heat}(T - T_{50})}}
\]

### 3. Light and VPD functions

\[
LightF = 1 - e^{-k_{light} \cdot PAR / L_{max}}
\quad
VPDeF = \frac{1}{1 + e^{k_{vpd}(VPD - VPD_{50})}}
\]

### 4. Memory signals

Each normalized function is weighted by correlation (r) and significance (p):

\[
weighted_i = scaled_i \cdot r \cdot (1 - p)
\]

Final **PhenoMeNal** for year *i* and function *j*:

\[
PhenoMeNal_{i,j} = \sum_{bin=1}^{400} weighted_{i,j}(bin)
\]

---

## 📦 Installation

> ⚠️ **Windows only (for now)**. macOS/Linux support is under development.

### 1. Install R (≥ 4.0)

https://cran.r-project.org/

### 2. Install required R packages

    install.packages(c("devtools", "caret", "MASS", "data.table"))

### 3. Install PhenoMeNals from GitHub

    devtools::install_github("YourOrg/PhenoMeNals")

---

## 🚀 Getting Started

### 1. Calibrate phenology model

    calib <- phenologyCalibration(
      weather_data = weather_df,
      referenceBBCH = bbch_df,
      sites = "COL",
      varieties = "Carmenere",
      iterations = 200
    )

### 2. Run full workflow

    results <- runPhenomenals(
      weather_data = weather_df,
      target_data = yield_df,
      start_year = 2006,
      end_year = 2021,
      rolling_window = 3,
      bin_size = 1,
      multicollinearity_threshold = 0.8,
      max_phenomenals = 4
    )

**Outputs include**:
- Selected PhenoMeNals (weighted environmental memory signals)
- Predictions for yield, quality traits
- Cross-validation metrics: R², RMSE, MAE
- Variable importance and diagnostic plots

---

## 📊 Validation

- Multi-site, multi-variety datasets (Europe, Australia, USA)
- Phenology RMSE: **6–9 days**
- Yield and quality R²: **>0.8** in LOOCV  
- Predictive accuracy improves as the season advances

---

## 📜 References

The full methodological details, validation datasets and equations are published in:

**Bregaglio & Bajocco (2025).**  
*The PhenoMeNals framework for grapevine yield and quality prediction.*  
[Preprint / Journal link]

---

## 📂 License & Data

- Code released under **CC BY-NC 3.0**
- Validation dataset included (BBCH, weather, yield, quality traits)

---

## 🤝 Acknowledgments

This work was supported by:
- **AGRARSENSE** project (EU & Italian Ministry of Enterprises and Made in Italy)
- **MISFITS** project (Italian Ministry of Agriculture, Food Sovereignty and Forestry)

---

## ✉️ Contact

Open issues at: [GitHub Issues](https://github.com/YourOrg/PhenoMeNals/issues)  
Or email **simoneugomaria.bregaglio@crea.gov.it**

