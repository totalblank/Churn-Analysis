

# Set up `renv` (reproducible R env)

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
install.package("renv")
renv::restore()
```
