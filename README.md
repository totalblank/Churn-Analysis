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
renv::activate(project = ".")

install.packages(c(
  "shiny","bslib","tidyverse","plotly","DT","lubridate","data.table",
  "arrow","duckdb","memoise","pins",
  "tidymodels","vip","yardstick",
  "testthat","shinytest2","glue","readr","janitor"
))

revn::snapshot(prompt = F)
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