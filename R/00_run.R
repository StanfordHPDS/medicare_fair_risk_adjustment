# Author: Marissa Reitsma
# Script that Medicare risk adjustment analysis pipeline

# Load Packages and Functions --------------------------------------------------
source(file.path("R", "functions.R"))
source(file.path("R", "01_prep_hccs.R"))
source(file.path("R", "02_prep_costs.R"))
source(file.path("R", "03_prep_demographics.R"))
source(file.path("R", "04_regression_prep.R"))
source(file.path("R", "04_regression.R"))
source(file.path("R", "05_disparities_targets_regression.R"))
source(file.path("R", "05_prep_policy_levels.R"))

# Set Flags --------------------------------------------------------------------
source_packages()
use_virtualenv("medicare_risk_adjustment")
options(scipen = 999)
year <- 2018

data_dir <- set_data_dir(year)
sensitive_intermediates_dir <- set_intermediates_dir(year)

if (!dir.exists(sensitive_intermediates_dir)) {
  message("Creating sensitive intermediates directory")
  dir.create(sensitive_intermediates_dir, recursive = TRUE)
}

# Read in and prepare the CMS ICD to HCC mappings ------------------------------
hcc_map <- prep_hcc_map(year)

# Append split files
if (!file.exists(file.path(data_dir, "Demo.csv"))) {
  message("Appending Split Extract")
  rbind(
    fread(file.path(data_dir, "export_y0_demo_output.csv"), colClasses = c(STATE_CODE = "character", COUNTY_CD = "character", BENE_DEATH_DT = "IDate")),
    fread(file.path(data_dir, "export_y1_demo_output.csv"), colClasses = c(STATE_CODE = "character", COUNTY_CD = "character"))
  ) %>%
    fwrite(file.path(data_dir, "Demo.csv"), na = "", row.names = FALSE)
}
if (!file.exists(file.path(data_dir, "Carrier.csv"))) {
  message("Appending Split Extract")
  rbind(
    fread(file.path(data_dir, "carrier_1.csv")),
    fread(file.path(data_dir, "carrier_2.csv"))
  ) %>%
    fwrite(file.path(data_dir, "Carrier.csv"), na = "", row.names = FALSE)
}

# Map ICDs to HCCs -------------------------------------------------------------
prep_hccs(year = year)

# Prepare Data on Costs --------------------------------------------------------
prep_costs(year = year)

# Prepare Demographic Data -----------------------------------------------------
prep_demographics(year = year, county_threshold_percentile = 0.1)

# Prepare Combined Data --------------------------------------------------------
combined <- combine_predictors_costs(year = year)
combined <- combined[, cost := ifelse(cost_claims < 0, 0, cost_claims)] ## Bottomcode at 0
combined <- combined[, c("cost_claims", "cost_out_in_phys") := NULL]
sensitive_intermediates_dir <- set_intermediates_dir(year = year)
fwrite(combined, file.path(sensitive_intermediates_dir, "combined_df.csv"), na = "", row.names = FALSE)

# Prepare Data for Regression and Cross-Validation -----------------------------
prep_regression(year = year)

# Regressions ------------------------------------------------------------------

## Run baseline regression with full sample to establish in-sample net compensation that is used to set fair spending targets
run_regression(year = year, geo_level = "county", run_fair = FALSE, run_cv = FALSE, include_num_hcc = FALSE, lambda = 0, policy = "baseline", non_negative_hcc = TRUE)

## Run Disparities Regression
run_disparities_regression(year = year)

## Set Potential Fair Spending Levels
prep_policy_targets(year = year)

## Full Model
for (policy in c("equity_policy", "five_pct")) {
  message(paste0(policy, ": ", year))
  run_regression(year = year, geo_level = "county", run_fair = TRUE, run_cv = FALSE, include_num_hcc = FALSE, lambda = 0, policy = policy, non_negative_hcc = TRUE)
}

## Cross-Validation
run_regression(year = year, geo_level = "county", run_fair = FALSE, run_cv = TRUE, include_num_hcc = FALSE, lambda = 0, policy = "baseline", non_negative_hcc = TRUE)

for (policy in c("equity_policy", "five_pct")) {
  message(paste0(policy, ": ", year))
  run_regression(year = year, geo_level = "county", run_fair = TRUE, run_cv = TRUE, include_num_hcc = FALSE, lambda = 0, policy = policy, non_negative_hcc = TRUE)
}

## Manuscript Results
if (year == 2018) {
  source(file.path("R", "figures_tables", "race_ethnicity_definition_compare_figure.R"))
}
source(file.path("R", "figures_tables", "number_plug_manuscript.R"))
source(file.path("R", "figures_tables", "svi_figure.R"))
