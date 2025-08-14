SCHEMA=data/schema.json
PROCESSED=data/processed/telecom_churn_clean.csv

schema:
	Rscript -e "library(renv); renv::activate();source('R/utils_schema.R');lock_schema('$(PROCESSED)','$(SCHEMA)')"

schema-check:
	Rscript -e "library(renv); renv::activate();source('R/utils_schema.R');check_schema('$(PROCESSED)','$(SCHEMA)')"
