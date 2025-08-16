# 📂 Project Folder Layout

```
customer-churn-dashboard/
│
├── app.R                      # Main Shiny app entry point
├── README.md                  # Project overview, instructions, screenshots
├── renv.lock                  # Reproducible R environment lock file
├── .Rprofile                   # Ensures renv activation on load
│
├── R/                         # All R modules and helper functions
│   ├── mod_filters.R          # Shiny module: filters panel
│   ├── mod_kpis.R             # Shiny module: KPI boxes
│   ├── mod_eda.R              # Shiny module: interactive EDA plots
│   ├── mod_cohorts.R          # Shiny module: retention/cohort analysis
│   ├── mod_model.R            # Shiny module: churn prediction
│   ├── utils_data.R           # Data loading & cleaning functions
│   ├── utils_viz.R            # Common plotting functions
│   ├── utils_model.R          # ML model training, evaluation
│   └── utils_cache.R          # Caching/memoization helpers
│
├── data/                      # Raw and cleaned data
│   ├── raw/                   # Original dataset(s) (untouched)
│   │   └── telecom_churn.csv
│   ├── processed/             # Cleaned & feature-engineered data
│   │   └── churn_clean.csv
│   └── external/              # Any supplementary datasets (e.g., demographics)
│
├── models/                    # Saved trained models
│   ├── rf_churn_model.rds
│   └── model_metadata.json
│
├── www/                       # Web assets for Shiny
│   ├── custom.css             # Custom styles
│   ├── logo.png
│   └── scripts.js
│
├── quarto/                    # Narrative report files
│   ├── churn_analysis.qmd     # Full EDA + model + recommendations
│   ├── _quarto.yml
│   └── images/                # Plots/screenshots for report
│
├── tests/                     # Automated testing
│   ├── testthat/              # Unit tests for data/model functions
│   │   ├── test_data_cleaning.R
│   │   ├── test_model_training.R
│   │   └── test_utils.R
│   └── shinytest2/            # End-to-end UI tests
│       ├── test_app.R
│       └── recordings/
│
├── scripts/                   # Standalone scripts
│   ├── prepare_data.R         # One-off data prep pipeline
│   ├── train_model.R          # Script to retrain churn model
│   └── generate_report.R      # Script to render Quarto report
│
└── Dockerfile                 # For containerized deployment (optional)
```

# Set up `renv` (reproducible R env)

In **R console** from the project root:

```{r}
install.packages("renv")
library(renv)
renv::init()
renv::snapshot() # choose 1
renv::activate(project = ".")

renv::install(c(
  "shiny","bslib","tidyverse","plotly","DT","lubridate","data.table",
  "arrow","duckdb","memoise","pins",
  "tidymodels","vip","yardstick",
  "testthat","shinytest2","glue","readr","janitor"
))

renv::snapshot(prompt = F)
```

# Reproduce

```{r}
# 1. Install renv globally if ineeded
install.package("renv")

# 2. Restore project environment
renv::restore()

# 3. Run the Shiny app
shiny::runApp()
```

# Tasks

## 1. **Univariate & Distributional Analysis**

* **Numeric distributions**: Age, Tenure, MonthlyCharges, TotalCharges → histograms, KDEs, boxplots.
* **Check skew/outliers** in charges and tenure — might explain unusual churn patterns.
* **Categorical balances**: Gender, PhoneService, InternetService, Contract → churn vs. non-churn proportions.

---

## 2. **Bivariate (Churn vs. Features)**

* **Churn rate by tenure bins** (0–6 months, 6–12, etc.).
* **Contract type vs. churn** (Month-to-month likely has higher churn).
* **Internet service vs. churn** — e.g., fiber optic vs DSL vs None.
* **MonthlyCharges vs. churn** (density plots).
* **TotalCharges vs. churn** (boxplots or violin plots).

---

## 3. **Multivariate Relationships**

* **Age × Tenure × Churn** → are younger short-tenure customers churning more?
* **MonthlyCharges × Contract × Churn** → is churn high for high-charge, short contracts?
* **InternetService × Contract** → see combined churn risk.

---

## 4. **Feature Engineering for EDA**

* **Customer Lifetime Value proxy** = `MonthlyCharges * Tenure` vs. churn.
* **Normalized charges** = `TotalCharges / Tenure` (average spend per month).
* **“At-risk” cohorts** → e.g., high charges, month-to-month, short tenure.

---

## 5. **Segmented Churn Profiles**

* **Gender × Churn**: is churn rate different between male/female?
* **PhoneService × Churn**: does having phone service reduce churn likelihood?
* **InternetService × Churn**: are “Fiber optic” customers more likely to churn due to high costs?

---

## 6. **Correlation & Association**

* Correlation matrix (numeric only: Age, Tenure, MonthlyCharges, TotalCharges).
* **Cramer’s V / Chi-square tests** for categorical predictors vs. churn.
* Mutual information (if you want more advanced measure).

---

## 7. **Survival-style Analysis (Tenure)**

* Treat **Tenure** like “time until churn”.
* Plot “survival curves” (Kaplan–Meier style) by contract type.
* Example: Month-to-month contracts may have much shorter survival.

---

## 8. **Clustering for Customer Segmentation**

* Cluster customers on Age, Charges, Tenure → then compare churn rates by cluster.
* Helps identify high-risk customer groups visually.

---

## 9. **Advanced Visuals**

* **Heatmaps** of churn rate across two dimensions (e.g., Tenure × MonthlyCharges).
* **Stacked bar plots** for Contract × Churn.
* **Sankey diagrams** for service usage paths (PhoneService → InternetService → Churn).

---

## 10. **Feature Importance (pre-ML)**

* Use logistic regression or random forest just for **exploratory feature importance**.
* Helps decide which variables to highlight in Shiny dashboard.

---

## 🔹 Portfolio Angle

In your Quarto EDA report, you should:

* Walk through **descriptive → bivariate → multivariate → survival → clustering**.
* End with **business insights**:
  *“Short-tenure, month-to-month, fiber optic customers paying >\$80/month churn at 2.5× the average rate.”*
  This shows you not only did EDA, but also **translated findings into business terms**.

---
