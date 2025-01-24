# Author: Marissa Reitsma
# Function that defines alternative levels of payment that meet policy-defined goals

prep_policy_targets <- function(year) {
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  base_pred <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_full.csv"))
  spend_base <- mean(base_pred$cost)
  base_pred <- base_pred[, is_net_compensation_rti := mean(pred) - mean(cost), by = c("rti_race")]
  base_pred <- unique(base_pred[, .(RTI_RACE_CD, rti_race, is_net_compensation_rti)])

  base_pred <- base_pred[, net_zero := 0]

  disparities_targets <- fread(file.path(sensitive_intermediates_dir, "disparities_targets.csv"))
  disparities_targets <- disparities_targets[, equity_policy := pred_county - cost]

  base_pred <- merge(base_pred, disparities_targets[, .(rti_race, equity_policy)], by = "rti_race")
  base_pred <- base_pred[(rti_race %in% c("Non-Hispanic White", "Unknown", "Other")), equity_policy := NA]
  base_pred <- base_pred[!(rti_race %in% c("Non-Hispanic White", "Unknown", "Other")), five_pct := is_net_compensation_rti + (spend_base * 0.05)]

  write.csv(base_pred, file.path(sensitive_intermediates_dir, "policy_levels.csv"), na = "", row.names = FALSE)
}
