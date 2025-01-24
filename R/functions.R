# Clean HCC Mappings Downloaded from CMS Website ----------------------
prep_hcc_map <- function(year) {
  if (year %in% c(2020, 2019, 2018, 2017)) {
    hcc_map <- fread(file.path("safe_input", "2020 Midyear_Final ICD-10-CM Mappings.csv"))
    setnames(
      hcc_map,
      c("Diagnosis\nCode", "CMS-HCC\nModel\nCategory\nV24", "CMS-HCC\nModel\nCategory V24\nfor 2020\nPayment Year"),
      c("Diagnosis_Code", "CMS_HCC_V24", "CMS_HCC_V24_Included")
    )
    hcc_map <- hcc_map[, .(Diagnosis_Code, CMS_HCC_V24, CMS_HCC_V24_Included)]
    hcc_map <- hcc_map[CMS_HCC_V24_Included == "Yes"]
    hcc_map <- hcc_map[, .(Diagnosis_Code, CMS_HCC_V24)]
    setnames(hcc_map, "CMS_HCC_V24", "HCC")
  } else if (year == 2014) {
    hcc_map <- fread(file.path(data_dir, paste0("hcc_map_", year, ".csv")))
    setnames(hcc_map, "2014 CMS-HCC Model Category", "CMS_HCC_Model_Category")
    setnames(hcc_map, "2014 CMS-HCC Model for 2014 Payment Year", "CMS_HCC_Model_for_2014_Payment_Year")
    setnames(hcc_map, "Diagnosis Code", "Diagnosis_Code")

    hcc_map <- hcc_map[, .(Diagnosis_Code, CMS_HCC_Model_Category, CMS_HCC_Model_for_2014_Payment_Year)]
    hcc_map <- hcc_map[CMS_HCC_Model_for_2014_Payment_Year == "--", CMS_HCC_Model_for_2014_Payment_Year := ""]
    hcc_map <- hcc_map[CMS_HCC_Model_for_2014_Payment_Year == "Yes"]

    hcc_map <- hcc_map[!is.na(CMS_HCC_Model_Category)]
    hcc_map <- hcc_map[, .(Diagnosis_Code, CMS_HCC_Model_Category)]
  }
}

# Combine demograhpics, costs, and hccs --------------------
combine_predictors_costs <- function(year) {
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)
  df <- fread(file.path(sensitive_intermediates_dir, "demographics.csv"))
  message("N Demo: ", nrow(df))
  temp <- fread(file.path(sensitive_intermediates_dir, "costs.csv"))
  temp <- temp[BENE_ENROLLMT_REF_YR == year + 1]
  temp <- temp[, BENE_ENROLLMT_REF_YR := NULL]
  message("N Costs: ", nrow(temp))
  df <- merge(df, temp, by = "BENE_ID", all.x = T)
  temp <- fread(file.path(sensitive_intermediates_dir, "mapped_hcc.csv"))
  message("N Bene HCC: ", length(unique(temp$BENE_ID)))
  df <- merge(df, temp, by = "BENE_ID", all.x = T)
}

# Map Race/Ethnicity Codes to Labels --------------------
label_race <- function(df, race_code_version) {
  if (tolower(race_code_version) == "base") {
    message("Labeling Base Race/Ethnicity Codes")
    df <- df[, base_race := fcase(
      BENE_RACE_CD == 0, "Unknown",
      BENE_RACE_CD == 1, "Non-Hispanic White",
      BENE_RACE_CD == 2, "Black",
      BENE_RACE_CD == 3, "Other",
      BENE_RACE_CD == 4, "Asian/Pacific Islander",
      BENE_RACE_CD == 5, "Hispanic",
      BENE_RACE_CD == 6, "American Indian/Alaska Native"
    )]
  } else if (tolower(race_code_version) == "rti") {
    message("Labeling RTI Race/Ethnicity Codes")
    df <- df[, rti_race := fcase(
      RTI_RACE_CD == 0, "Unknown",
      RTI_RACE_CD == 1, "Non-Hispanic White",
      RTI_RACE_CD == 2, "Black",
      RTI_RACE_CD == 3, "Other",
      RTI_RACE_CD == 4, "Asian/Pacific Islander",
      RTI_RACE_CD == 5, "Hispanic",
      RTI_RACE_CD == 6, "American Indian/Alaska Native"
    )]
  } else if (!(tolower(race_code_version) %in% c("rti", "base"))) {
    message("version does not match base or rti, nothing labeled")
  }
}

# Compute performance measures --------------------

compute_performance <- function(df) {
  df <- df[, c("rmse", "mae", "r2", "oos_net_compensation_rti", "is_net_compensation_rti", "median_abs_error", "pred_ratio") := NULL]
  df <- df[fold == fold_coefs & !is.na(pred), rmse := sqrt(mean((pred - cost)^2)), by = "fold_coefs"]
  df <- df[, rmse := mean(rmse, na.rm = TRUE), by = "fold_coefs"]
  df <- df[fold == fold_coefs & !is.na(pred), mae := mean(abs(pred - cost)), by = "fold_coefs"]
  df <- df[, mae := mean(mae, na.rm = TRUE), by = "fold_coefs"]
  df <- df[fold == fold_coefs & !is.na(pred), r2 := 1 - ((sum((cost - pred)^2)) / (sum((cost - mean(cost))^2))), by = "fold_coefs"]
  df <- df[, r2 := mean(r2, na.rm = TRUE), by = "fold_coefs"]
  df <- df[fold == fold_coefs & !is.na(pred), oos_net_compensation_rti := mean(pred) - mean(cost), by = c("fold_coefs", "rti_race")]
  df <- df[, oos_net_compensation_rti := mean(oos_net_compensation_rti, na.rm = TRUE), by = c("fold_coefs", "rti_race")]
  df <- df[fold != fold_coefs & !is.na(pred), is_net_compensation_rti := mean(pred) - mean(cost), by = c("fold_coefs", "rti_race")]
  df <- df[, is_net_compensation_rti := mean(is_net_compensation_rti, na.rm = TRUE), by = c("fold_coefs", "rti_race")]
  df <- df[fold_coefs == fold & !is.na(pred), median_abs_error := median(abs(pred - cost)), by = c("fold_coefs")]
  df <- df[, median_abs_error := mean(median_abs_error, na.rm = TRUE), by = "fold_coefs"]
  df <- df[fold_coefs == fold & !is.na(pred), pred_ratio := mean(pred) / mean(cost), by = c("fold_coefs", "rti_race")]
  df <- df[, pred_ratio := mean(pred_ratio, na.rm = T), by = c("fold_coefs", "rti_race")]
}

set_data_dir <- function(year) {
  file.path(Sys.getenv("MEDICARE_DATA_PATH"), "Raw", year)
}

set_intermediates_dir <- function(year) {
  file.path(Sys.getenv("MEDICARE_DATA_PATH"), "sensitive_intermediates", year)
}

source_packages <- function() {
  library(data.table)
  library(ggplot2)
  library(haven)
  library(reticulate)
  library(Matrix)
  library(lubridate)
  library(magrittr)
  library(purrr)
  library(dplyr)
  library(reticulate)
  library(foreach)
  library(doParallel)
  library(scales)
}

create_dirs <- function(data_dir, sensitive_intermediates_dir) {
  dir.create(file.path(data_dir), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(sensitive_intermediates_dir), showWarnings = FALSE, recursive = TRUE)
}
