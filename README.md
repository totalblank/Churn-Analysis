# Set up `renv` (reproducible R env)

In **R console** from the project root:

```{r}
install.packages("renv")
library(renv)
renv::init()
renv::snapshot() # choose 1
renv::activate(project = ".")

renv::install(c(
  "dplyr",
  "purrr",
  "tibble",
  "showtext",
  "knitr",
  "kableExtra",
  "reshape2",
  "ggcorrplot",
  "viridis",
  "rstatix",
  "DescTools",
  "FSelectorRcpp",
  "shiny",
  "bslib",
  "tidyverse",
  "DT",
  "lubridate",
  "data.table",
  "arrow",
  "duckdb",
  "memoise",
  "pins",
  "tidymodels",
  "vip",
  "yardstick",
  "testthat",
  "shinytest2",
  "glue",
  "readr",
  "janitor"
))

renv::snapshot()
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

# Analysis Steps

## 1. **Univariate & Distributional Analysis** (done)

* **Numeric distributions**: Age, Tenure, MonthlyCharges, TotalCharges → histograms, KDEs, boxplots.
* **Check skew/outliers** in charges and tenure — might explain unusual churn patterns.
* **Categorical balances**: Gender, PhoneService, InternetService, Contract → churn vs. non-churn proportions.

---

## 2. **Bivariate (Churn vs. Features)** (done)

* **Churn rate by tenure bins** (0–6 months, 6–12, etc.).
* **Contract type vs. churn** (Month-to-month likely has higher churn).
* **Internet service vs. churn** — e.g., fiber optic vs DSL vs None.
* **MonthlyCharges vs. churn** (density plots).
* **TotalCharges vs. churn** (boxplots or violin plots).

---

## 3. **Multivariate Relationships** (done)

* **Age × Tenure × Churn** → are younger short-tenure customers churning more?
* **MonthlyCharges × Contract × Churn** → is churn high for high-charge, short contracts?
* **InternetService × Contract** → see combined churn risk.

---

## 4. **Feature Engineering for EDA** (done)

* **Customer Lifetime Value proxy** = `MonthlyCharges * Tenure` vs. churn.
* **Normalized charges** = `TotalCharges / Tenure` (average spend per month).
* **“At-risk” cohorts** → e.g., high charges, month-to-month, short tenure.

---

## 5. **Segmented Churn Profiles** (done)

* **Gender × Churn**: is churn rate different between male/female?
* **PhoneService × Churn**: does having phone service reduce churn likelihood?
* **InternetService × Churn**: are “Fiber optic” customers more likely to churn due to high costs?

---

## 6. **Correlation & Association** (done)

* Correlation matrix (numeric only: Age, Tenure, MonthlyCharges, TotalCharges).
* **Cramer’s V / Chi-square tests** for categorical predictors vs. churn.
* Mutual information (if you want more advanced measure).

---

## 7. **Survival-style Analysis (Tenure)** (done)

* Treat **Tenure** like “time until churn”.
* Plot “survival curves” (Kaplan–Meier style) by contract type.
* Example: Month-to-month contracts may have much shorter survival.

---

## 8. **Clustering for Customer Segmentation** (done)

* Cluster customers on Age, Charges, Tenure → then compare churn rates by cluster.
* Helps identify high-risk customer groups visually.

---

## 9. **Advanced Visuals** (done)

* **Heatmaps** of churn rate across two dimensions (e.g., Tenure × MonthlyCharges).
* **Stacked bar plots** for Contract × Churn.
* **Sankey diagrams** for service usage paths (PhoneService → InternetService → Churn).

---

## 10. **Feature Importance (pre-ML)** (done)

* Use logistic regression or random forest just for **exploratory feature importance**.
* Helps decide which variables to highlight in Shiny dashboard.

