# Author: Marissa Reitsma
# Script that prepares cost data for risk adjustment equation

prep_costs <- function(year) {
  message(paste0("Running Script to Prepare Cost Data: ", year))

  data_dir <- set_data_dir(year = year)
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  # Read in Cost Data from Redivis --------------------------
  cost_df <- purrr::reduce(
    list(
      fread(file.path(data_dir, "cost.csv")),
      fread(file.path(data_dir, "carrier_line_cost.csv")),
      fread(file.path(data_dir, "outpatient_cost.csv")),
      fread(file.path(data_dir, "medpar_cost.csv"))
    ),
    dplyr::full_join,
    by = c("BENE_ENROLLMT_REF_YR", "BENE_ID")
  )

  cost_df[is.na(cost_df)] <- 0

  # Create a Total Cost Outcome Variable --------------------
  ## Pending decision on whether to include DME/HH/SNF costs, pass-through costs, and approach to computing costs (ie., from MBSF or from claims files).
  cost_df <- cost_df[, cost := ACUTE_MDCR_PMT +
    OIP_MDCR_PMT + HOP_MDCR_PMT +
    ASC_MDCR_PMT + PTB_DRUG_MDCR_PMT + EM_MDCR_PMT + ANES_MDCR_PMT +
    DIALYS_MDCR_PMT + OPROC_MDCR_PMT + IMG_MDCR_PMT +
    TEST_MDCR_PMT + OTHC_MDCR_PMT + PHYS_MDCR_PMT +
    DME_MDCR_PMT + HH_MDCR_PMT + SNF_MDCR_PMT +
    ACUTE_PERDIEM_PMT + OIP_PERDIEM_PMT]
  cost_df <- cost_df[, cost_out_in_phys := ACUTE_MDCR_PMT +
    OIP_MDCR_PMT + HOP_MDCR_PMT +
    ASC_MDCR_PMT + PTB_DRUG_MDCR_PMT + EM_MDCR_PMT + ANES_MDCR_PMT +
    DIALYS_MDCR_PMT + OPROC_MDCR_PMT + IMG_MDCR_PMT +
    TEST_MDCR_PMT + OTHC_MDCR_PMT + PHYS_MDCR_PMT +
    ACUTE_PERDIEM_PMT + OIP_PERDIEM_PMT]
  cost_df <- cost_df[, cost_claims := tot_nch_pmt + tot_outpatient_pmt + tot_medpar_pmt]
  cost_df <- cost_df[, .(BENE_ID, BENE_ENROLLMT_REF_YR, cost, cost_out_in_phys, cost_claims)]

  # Save Output -------------------------------------------
  write.csv(cost_df, file.path(sensitive_intermediates_dir, "costs.csv"), na = "", row.names = FALSE)

  message("Finished Script to Prepare Cost Data")
}
