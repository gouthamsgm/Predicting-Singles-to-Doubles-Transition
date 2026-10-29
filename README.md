# Predicting Singles-to-Doubles Career Transitions (ATP 2000–2020)

[![R Version](https://img.shields.io/badge/R-%3E%3D%204.1-blue.svg)](https://www.r-project.org/)
[![Framework](https://img.shields.io/badge/Framework-tidymodels%20%7C%20shiny-indigo.svg)](https://www.tidymodels.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Data Source](https://img.shields.io/badge/Data-Jeff%20Sackmann%20ATP-orange.svg)](https://github.com/JeffSackmann/tennis_atp)

An end-to-end R data science pipeline and interactive Shiny dashboard that identifies ATP tennis players who transition ("pivot") from singles to doubles careers, clusters early-career ranking paths into archetypes, and predicts pivot probability with machine learning.

---

## 🔬 Key Research Findings

- **Analyzed Cohort:** 4,360 professional ATP players debuting between 2001 and 2011 with full 9-year observation horizons (4-year early career + 5-year follow-up).
- **Pivot Rate:** 141 players (3.23%) executed an empirical career transition to doubles.
- **Career Archetypes ($K = 3$):**
  - **A1: Mid-tier, rising** (796 players): **12.94% pivot rate** (95% CI: 10.7%–15.5%). Prime candidates for a strategic doubles pivot.
  - **A2: Low-ranked, declining** (1,717 players): **0.23% pivot rate** (95% CI: 0.07%–0.64%). Rarely transition; largely exit professional tennis.
  - **A3: Low-ranked, rising** (1,847 players): **1.84% pivot rate** (95% CI: 1.30%–2.59%). Modest transition rate.
- **Statistical Significance:**
  - $\chi^2 = 369.3$, $p < 10^{-15}$ (Chi-square test of independence).
  - Logistic Odds Ratio: A1 players are $>60\times$ more likely to pivot than A2 (OR = 0.016, $p = 4.65 \times 10^{-16}$) and $\approx 8\times$ more likely than A3 (OR = 0.126, $p = 1.81 \times 10^{-24}$).
- **Survival Analysis (Cox Proportional Hazards):**
  - Significant divergence in time-to-pivot across archetypes (Log-rank test: $p < 0.0001$).
  - Hazard Ratio for A2 vs A1: $0.059$ ($p = 7.41 \times 10^{-6}$).
  - Hazard Ratio for A3 vs A1: $0.186$ ($p = 2.58 \times 10^{-5}$).
- **Supervised Machine Learning:**
  - Top model (XGBoost / Random Forest with SMOTE class rebalancing): **PR-AUC 0.998**, **ROC-AUC 0.938**, **Accuracy 92.5%**, **Balanced Accuracy 77.5%** on held-out test data.

---

## ⚡ Quick Start

```r
source("setup_packages.R")   # Install required CRAN packages (run once)
source("run_all.R")          # Execute full 7-step pipeline (~5-10 min)
shiny::runApp("app")         # Launch interactive Shiny dashboard
```

---

## 📂 Repository Structure

```
.
├── 00_config.R                 <- Central configuration and parameters
├── 01_clean.R                  <- Data extraction & cleaning
├── check_outputs.R             <- Verification of outputs
├── run_all.R                   <- Pipeline master runner
├── setup_packages.R            <- CRAN dependencies installer
│
├── R/
│   ├── 00_config.R             <- Shared constants, parameters, plot helpers
│   ├── 01_clean.R              <- Ingest raw CSVs → tidy rankings & activity
│   ├── 02_trajectories.R       <- 16-quarter career-aligned ranking series & features
│   ├── 03_clustering.R         <- K-means clustering (CVI: Silhouette, CH, DB)
│   ├── 04_pivot_labels.R       <- Pivot definition, labels, sensitivity analysis
│   ├── 05_analysis_models.R    <- Chi-square, CV model training (LR, RF, XGBoost)
│   ├── 06_survival.R           <- Kaplan-Meier curves, log-rank test, Cox regression
│   └── 07_prep_dashboard.R     <- Dashboard bundle preparation
│
├── app/
│   └── app.R                   <- Full interactive Shiny web app (bslib + plotly)
│
├── data/
│   ├── raw/                    <- Extracted ATP match and ranking CSVs
│   └── tennis-sackmann-archive-main.zip
│
├── outputs/                    <- Data artifacts, models, CSV predictions & metrics
└── figures/                    <- High-resolution plots and visualizations
```

---

## 🔄 Pipeline Workflow

| Step | Script | Objective |
|:---:|---|---|
| **0** | `R/00_config.R` | Tunable parameters, paths, scaling and log-rank helpers |
| **1** | `R/01_clean.R` | Ingest Sackmann raw files $\rightarrow$ tidy player rankings and singles/doubles activity |
| **2** | `R/02_trajectories.R` | Align debuts, construct 16-quarter trajectories and engineer player features |
| **3** | `R/03_clustering.R` | Discover career archetypes with K-means and compute cluster validity indices |
| **4** | `R/04_pivot_labels.R` | Formulate objective pivot labels across 5-year follow-up and conduct sensitivity analysis |
| **5** | `R/05_analysis_models.R` | Chi-square test, SMOTE oversampling, 5-fold CV across glmnet, ranger, and xgboost |
| **6** | `R/06_survival.R` | Kaplan-Meier survival curves, log-rank test, and Cox proportional hazards modeling |
| **7** | `R/07_prep_dashboard.R` | Assemble `dashboard_data.rds` combining scores, PCA mappings, and medoid series |
| **UI**| `app/app.R` | Modern Shiny dashboard with 4 tabs: Overview, Cluster Map, Player Lookup, and About |

---

## ⚙️ Configuration (`R/00_config.R`)

| Parameter | Default | Meaning |
|---|---|---|
| `EARLY_YEARS` | 4 | Window length in years for early-career trajectory tracking |
| `HORIZON_YEARS` | 5 | Follow-up window in years for pivot evaluation |
| `SHARE_THR` | 0.30 | Minimum doubles match share in follow-up window |
| `MIN_DOUBLES` | 5 | Minimum doubles match count in follow-up window |
| `K_CHOSEN` | 3 | Chosen number of career archetypes |
| `RANK_CAP` | 2000 | Baseline ranking cap for unranked or low-ranked periods |
| `SEED` | 42 | Reproducible global random seed |

---

## 📈 Cross-Validation & Test Benchmarks

### 5-Fold Stratified Cross-Validation

| Workflow | Model Type | PR-AUC (Mean ± SE) | ROC-AUC (Mean ± SE) | Balanced Acc | Accuracy |
|---|---|---|---|---|---|
| **`full_xgboost`** | **XGBoost (Full)** | **0.9981 ± 0.0002** | **0.9493 ± 0.0034** | **0.8368 ± 0.0255** | 92.1% |
| `full_random_forest` | Random Forest (Full) | 0.9980 ± 0.0003 | 0.9461 ± 0.0042 | 0.7330 ± 0.0210 | 95.6% |
| `full_logistic` | Logistic (Full) | 0.9969 ± 0.0005 | 0.9178 ± 0.0064 | 0.8190 ± 0.0087 | 87.5% |
| `trajectory_logistic` | Logistic (Traj-only) | 0.9955 ± 0.0007 | 0.8952 ± 0.0053 | 0.7938 ± 0.0242 | 87.0% |
| `trajectory_xgboost` | XGBoost (Traj-only) | 0.9953 ± 0.0006 | 0.8898 ± 0.0064 | 0.7963 ± 0.0175 | 87.6% |
| `trajectory_random_forest`| Random Forest (Traj-only)| 0.9946 ± 0.0007 | 0.8736 ± 0.0076 | 0.6689 ± 0.0238 | 91.7% |

### Held-Out Test Set Metrics
- **PR-AUC:** 0.9980
- **ROC-AUC:** 0.9383
- **Accuracy:** 92.55%
- **Balanced Accuracy:** 77.52%
- **Precision:** 98.75%
- **Sensitivity:** 93.50%
- **F1 Score:** 0.9605

---

## 🌟 Notable Detected Transitions

The model accurately identifies real-world grand slam doubles champions from their early career patterns:
- **Austin Krajicek** (USA) – *Archetype A1, Best Early Rank 636, pivoted month 6* $\rightarrow$ ATP Doubles World No. 1, Roland Garros Champion.
- **Andreas Mies** (GER) – *Archetype A3, Best Early Rank 814, pivoted month 50* $\rightarrow$ 2-time Roland Garros Doubles Champion.
- **Albano Olivetti** (FRA) – *Archetype A1, Best Early Rank 210, pivoted month 8* $\rightarrow$ ATP Tour Doubles Titleist.
- **Alex Bolt** (AUS) – *Archetype A1, Best Early Rank 296, pivoted month 3* $\rightarrow$ Australian Open Doubles Quarterfinalist.
- **Ben McLachlan** (JPN) – *Archetype A3, Best Early Rank 740, pivoted month 29* $\rightarrow$ ATP Top 20 Doubles Specialist.
