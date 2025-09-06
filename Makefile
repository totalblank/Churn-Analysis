DATA_PROCESSED = data/processed/customer_churn_data_clean.csv
SCHEMA_FILE    = data/schema.json

# Schema locking target
schema:
	Rscript --vanilla -e "renv::activate('.'); \
	                      renv::restore(prompt=FALSE); \
	                      source('R/utils_schema.R'); \
	                      lock_schema('$(DATA_PROCESSED)', '$(SCHEMA_FILE)', metadata = list(stage='processed'))"

# Schema checking target
schema-check:
	Rscript --vanilla -e "renv::activate('.'); \
	                      renv::restore(prompt=FALSE); \
	                      source('R/utils_schema.R'); \
	                      check_schema('$(DATA_PROCESSED)', '$(SCHEMA_FILE)', \
	                                   strict_names=F, strict_types=T, allow_reorder=T)"
