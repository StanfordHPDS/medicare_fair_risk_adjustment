# Author: Marissa Reitsma
# Script that prepares the data for regression and cross-validation

prep_regression <- function(year) {
  message(paste0("Running Script to Prepare Modeling Frame and Cross-Validation: ", year))

  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  # Read in the prepped data frame that includes demographics, HCCs, and costs
  mod_df <- fread(file.path(sensitive_intermediates_dir, "combined_df.csv"))

  # Cast HCCs wide (binary indicator for each HCC)
  mod_df <- mod_df[, indic := 1]
  mod_df <- dcast(mod_df, BENE_ID + STATE_CODE + BENE_RACE_CD + RTI_RACE_CD + age_grp + sex_grp + county_id + county_n + grp_county + n_grp_county + cost + died + orig_dis ~ HCC, value.var = "indic")
  mod_df[is.na(mod_df)] <- 0
  mod_df <- mod_df[, `V1` := NULL]

  # Age and sex together as a single indcator
  mod_df <- mod_df[, age_sex := paste0(sex_grp, "_", age_grp)]

  # Separate male/female originally disabled indcators
  mod_df <- mod_df[, orig_dis_f := ifelse(sex_grp == "Female" & orig_dis == 1, 1, 0)]
  mod_df <- mod_df[, orig_dis_m := ifelse(sex_grp == "Male" & orig_dis == 1, 1, 0)]

  # Prepare cross-validation folds
  set.seed(12345) # Setting a seed for reproducibility
  mod_df <- mod_df[, fold := sample(1:10, .N, replace = TRUE)]
  bene_fold <- unique(mod_df[, .(BENE_ID, fold)])

  # Check folds to confirm that all county fixed effects will be estimated
  all_counties <- unique(mod_df$grp_county)
  for (fold_id in unique(mod_df$fold)) {
    if (length(unique(mod_df$grp_county[mod_df$fold != fold_id])) < length(all_counties)) {
      message("MISSING COUNTY FIXED EFFECTS FOLD: ", fold_id)
    }
  }

  # Save
  fwrite(mod_df, file.path(sensitive_intermediates_dir, "mod_df.csv"), na = "", row.names = FALSE)
  fwrite(bene_fold, file.path(sensitive_intermediates_dir, "bene_fold.csv"), na = "", row.names = FALSE)
}
