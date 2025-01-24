# Author: Marissa Reitsma
# Function that prepares demographic data for risk adjustment equation

prep_demographics <- function(year, county_threshold_percentile) {
  message(paste0("Running Script to Prepare Demographic Data: ", year))

  data_dir <- set_data_dir(year = year)
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  # Read in demographic data from Redivis ----------------------------------
  demo_df <- fread(file.path(data_dir, "/Demo.csv"), colClasses = c(STATE_CODE = "character", COUNTY_CD = "character"))

  # Filter to eligible benes -----------------------------------------------

  #### Very rarely found benes with multiple death dates, remove
  death_df <- demo_df[!is.na(BENE_DEATH_DT), .(BENE_ID, BENE_DEATH_DT)]
  death_df <- death_df[, death_year := year(BENE_DEATH_DT)]
  death_df <- dcast(death_df, BENE_ID ~ death_year, value.var = "BENE_DEATH_DT")
  if (year %in% colnames(death_df)) {
    bene_drop <- death_df[!is.na(get(paste0(year)))]
    bene_drop <- bene_drop$BENE_ID
  } else {
    bene_drop <- c()
  }

  #### Confirm eligible until death
  death_df <- demo_df[!is.na(BENE_DEATH_DT) & !(BENE_ID %in% bene_drop), .(BENE_ID, BENE_DEATH_DT, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, DUAL_ELGBL_MONS, ESRD_IND)]
  death_df <- death_df[, month_death := month(BENE_DEATH_DT, label = FALSE)]
  death_df <- death_df[BENE_HI_CVRAGE_TOT_MONS == month_death & BENE_SMI_CVRAGE_TOT_MONS == month_death & BENE_HMO_CVRAGE_TOT_MONS == 0 & DUAL_ELGBL_MONS == 0 & ESRD_IND == 0, eligible := 1]
  bene_drop <- c(bene_drop, death_df$BENE_ID[is.na(death_df$eligible)])

  #### Drop ineligible benes
  demo_df <- demo_df[!(BENE_ID %in% bene_drop)]

  # Extract
  demo_df <- demo_df[BENE_ENROLLMT_REF_YR == year]
  demo_df <- demo_df[AGE_AT_END_REF_YR >= 65]
  demo_df <- demo_df[ENTLMT_RSN_ORIG %in% c(0, 1)]

  demo_df <- demo_df[, .(BENE_ID, STATE_CODE, COUNTY_CD, AGE_AT_END_REF_YR, SEX_IDENT_CD, BENE_RACE_CD, RTI_RACE_CD, ENTLMT_RSN_ORIG)]

  # Map ages to age categories ----------------------------------
  demo_df <- demo_df[, age_grp := fcase(
    AGE_AT_END_REF_YR <= 69 & AGE_AT_END_REF_YR >= 65, "65-69",
    AGE_AT_END_REF_YR <= 74 & AGE_AT_END_REF_YR >= 70, "70-74",
    AGE_AT_END_REF_YR <= 79 & AGE_AT_END_REF_YR >= 75, "75-79",
    AGE_AT_END_REF_YR <= 84 & AGE_AT_END_REF_YR >= 80, "80-84",
    AGE_AT_END_REF_YR <= 89 & AGE_AT_END_REF_YR >= 85, "85-89",
    AGE_AT_END_REF_YR >= 90, "90+"
  )]
  demo_df <- demo_df[, AGE_AT_END_REF_YR := NULL]

  # Map sex to string ----------------------------------
  demo_df <- demo_df[, sex_grp := fcase(
    SEX_IDENT_CD == 1, "Male",
    SEX_IDENT_CD == 2, "Female"
  )]
  demo_df <- demo_df[, SEX_IDENT_CD := NULL]

  # Originally disabled indicator ----------------------
  setnames(demo_df, "ENTLMT_RSN_ORIG", "orig_dis")

  # Died in payment year -------------------------------
  demo_df <- demo_df[, died := ifelse(BENE_ID %in% death_df$BENE_ID, 1, 0)]

  # Create relevant geographic identifiers -------------
  ssa_fips <- fread(file.path(data_dir, paste0("ssa_fips_state_county", year, ".csv")))
  if (year %in% c(2018, 2019)) {
    ssa_state <- fread(file.path(data_dir, "ssa_state.csv"))
    ssa_fips <- merge(ssa_fips, ssa_state, by = "state")
    setnames(ssa_fips, "ssacd", "ssacounty")
  }
  demo_df <- demo_df[, ssastate := as.numeric(STATE_CODE)]
  demo_df <- demo_df[ssastate <= 53 & !is.na(ssastate) & ssastate != 48 & ssastate != 0] ## Exclude territories
  demo_df <- demo_df[, ssacounty := as.numeric(paste0(ssastate, COUNTY_CD))]
  demo_df <- demo_df[, county_id := paste0(as.numeric(STATE_CODE), "_", COUNTY_CD)]
  demo_df <- demo_df[, c("COUNTY_CD") := NULL]
  demo_df <- demo_df[, county_n := .N, by = "county_id"]
  county_sizes <- unique(demo_df[, .(ssastate, ssacounty, county_n)])
  ssa_fips <- merge(ssa_fips, county_sizes, by = c("ssastate", "ssacounty"))
  county_threshold <- quantile(ssa_fips$county_n, county_threshold_percentile)
  message("Group County Threshold is: ", county_threshold)
  write.csv(data.table(year = year, min_ss = county_threshold), file.path(sensitive_intermediates_dir, "county_ss_threshold.csv"), na = "", row.names = FALSE)
  demo_df <- demo_df[, grp_county := ifelse(county_n >= county_threshold, county_id, paste0(as.numeric(STATE_CODE), "_grouped"))]

  # Further aggregation for very small grouped counties
  demo_df <- demo_df[, n_grp_county := .N, by = "grp_county"]
  demo_df <- demo_df[, grp_county := ifelse(n_grp_county >= county_threshold, grp_county, "national_grouped")]
  demo_df <- demo_df[, c("ssastate", "ssacounty") := NULL]

  # Save Output -------------------------------------------
  write.csv(demo_df, file.path(sensitive_intermediates_dir, "demographics.csv"), na = "", row.names = FALSE)

  message("Finished Script to Prepare Demographic Data")
}
