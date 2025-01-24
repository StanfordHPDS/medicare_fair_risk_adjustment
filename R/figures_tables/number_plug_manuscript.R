add_number <- function(number_df, label, number) {
  number_df <- rbind(number_df, data.table(label = label, number = number), fill = TRUE)
}

num_df <- NULL
combined <- fread(file.path(sensitive_intermediates_dir, "combined_df.csv"))

bene_df <- unique(combined[, .(BENE_ID, cost, RTI_RACE_CD, orig_dis, age_grp, sex_grp, died)])

## Sample size
num <- length(unique(combined$BENE_ID))
num_df <- add_number(num_df, label = "num_benes", number = num)

## Died
num <- length(unique(combined$BENE_ID[combined$died == 1]))
num_df <- add_number(num_df, label = "num_died", number = num)

## Spending
num <- mean(bene_df$cost)
num_df <- add_number(num_df, label = "mean_spending", number = num)

num <- median(bene_df$cost)
num_df <- add_number(num_df, label = "median_spending", number = num)

num <- quantile(bene_df$cost, .25)
num_df <- add_number(num_df, label = "iqr_lower_spending", number = num)

num <- quantile(bene_df$cost, .75)
num_df <- add_number(num_df, label = "iqr_upper_spending", number = num)

num <- nrow(bene_df[cost > 250000])
num_df <- add_number(num_df, label = "n_cost_exceed_250K", number = num)

num <- nrow(bene_df[sex_grp == "Male"])
num_df <- add_number(num_df, label = "n_male", number = num)

num <- nrow(bene_df[orig_dis == 1])
num_df <- add_number(num_df, label = "n_orig_disabled", number = num)

## ADD SINGLE-YEAR AGE
demo_df <- fread(file.path(data_dir, "Demo.csv"))
demo_df <- demo_df[BENE_ENROLLMT_REF_YR == year]
bene_df <- merge(bene_df, demo_df[, .(BENE_ID, AGE_AT_END_REF_YR)], by = "BENE_ID", all.x = TRUE)
rm(demo_df)

num <- mean(bene_df$AGE_AT_END_REF_YR)
num_df <- add_number(num_df, label = "mean_age", number = num)

n_hcc <- copy(combined)
n_hcc <- n_hcc[!(HCC %in% c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10P"))]
n_hcc <- n_hcc[!(HCC %like% "_")]
n_hcc <- n_hcc[, n_hcc := .N, by = "BENE_ID"]
n_hcc <- n_hcc[HCC == "", n_hcc := 0]

n_hcc <- unique(n_hcc[, .(BENE_ID, n_hcc)])

num <- nrow(n_hcc[n_hcc == 0])
num_df <- add_number(num_df, label = "n_zero_hcc", number = num)

num <- nrow(n_hcc[n_hcc == 1])
num_df <- add_number(num_df, label = "n_1_hcc", number = num)

num <- nrow(n_hcc[n_hcc == 2])
num_df <- add_number(num_df, label = "n_2_hcc", number = num)

num <- nrow(n_hcc[n_hcc %in% c(3, 4, 5)])
num_df <- add_number(num_df, label = "n_3_4_5_hcc", number = num)

num <- nrow(n_hcc[n_hcc >= 6])
num_df <- add_number(num_df, label = "n_gt6_hcc", number = num)

bene_df <- merge(bene_df, n_hcc, by = "BENE_ID", all.x = T)

## Table 1
table1 <- copy(bene_df)

preds <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_full.csv"))
table1 <- merge(table1, preds[, .(BENE_ID, pred)], by = "BENE_ID")

table1 <- label_race(df = table1, race_code_version = "rti")

table1_temp <- copy(table1)
table1_temp <- table1_temp[, rti_race := "All"]

table1 <- rbind(table1_temp, table1)
table1 <- table1[, male := ifelse(sex_grp == "Male", 1, 0)]

table1 <- table1[, mean_observed_cost := mean(cost), by = "rti_race"]
table1 <- table1[, median_observed_cost := median(cost), by = "rti_race"]
table1 <- table1[, mean_predicted_cost := mean(pred), by = "rti_race"]
table1 <- table1[, mean_age := mean(AGE_AT_END_REF_YR), by = "rti_race"]
table1 <- table1[, sample_size := .N, by = "rti_race"]
table1 <- table1[, pct_male := mean(male) * 100, by = "rti_race"]
table1 <- table1[, mean_n_hcc := mean(n_hcc), by = "rti_race"]
table1 <- table1[, pct_died := mean(died) * 100, by = "rti_race"]
table1 <- table1[, pct_orig_disabled := mean(orig_dis) * 100, by = "rti_race"]

table1 <- unique(table1[, .(rti_race, sample_size, mean_observed_cost, mean_predicted_cost, mean_age, pct_male, mean_n_hcc, pct_died, pct_orig_disabled)])
table1 <- table1[order(rti_race)]
table1 <- table1[, lapply(.SD, round, 0), by = c("rti_race", "sample_size", "mean_age", "pct_male", "mean_n_hcc", "pct_died", "pct_orig_disabled")]
table1 <- table1[, lapply(.SD, round, 1), by = c("rti_race", "sample_size", "mean_observed_cost", "mean_predicted_cost")]

table1 <- melt(table1, id.vars = "rti_race")
table1 <- dcast(table1, variable ~ rti_race, value.var = "value")

write.csv(table1, file.path("safe_figures", year, "table1.csv"), na = "", row.names = FALSE)

coefs_countyn <- fread(file.path(sensitive_intermediates_dir, "beta_county_baseline_lambda_0_full.csv"))
num <- nrow(coefs_countyn[coefs_countyn$HCC %like% "grp_county"])
num_df <- add_number(num_df, label = "n_counties_noref", number = num)

write.csv(num_df, file.path("safe_figures", year, "number_plugging_df.csv"), na = "", row.names = FALSE)

bene_df <- bene_df[, topcode_cost := ifelse(cost > 100000, 100000, cost)]
bene_df <- bene_df[, topcode_cost := ifelse(topcode_cost < 100, 100, topcode_cost)]

ggplot(data = bene_df, aes(x = topcode_cost)) +
  geom_histogram(aes(y = after_stat(count / sum(count)))) +
  theme_classic() +
  coord_cartesian(expand = 0) +
  labs(x = "Medicare Spending", y = "Percent of Beneficiaries") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::dollar) +
  theme(text = element_text(size = 16), plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(plot.caption = element_text(hjust = 0, size = 10), plot.caption.position = "plot")
if (year == 2018) {
  ggsave(file.path("safe_figures", year, "cost_histogram.png"), device = "png", width = 10, height = 7)
}

ggplot(data = bene_df, aes(x = topcode_cost)) +
  geom_histogram(aes(y = after_stat(count / sum(count)))) +
  theme_classic() +
  coord_cartesian(expand = 0) +
  labs(x = "Medicare Spending", y = "Percent of Beneficiaries") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10(labels = scales::dollar) +
  theme(text = element_text(size = 16), plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(plot.caption = element_text(hjust = 0, size = 10), plot.caption.position = "plot")
if (year == 2018) {
  ggsave(file.path("safe_figures", year, "cost_histogram_log.png"), device = "png", width = 10, height = 7)
}

bene_df <- bene_df[, topcode_hcc := ifelse(n_hcc > 10, 10, n_hcc)]

ggplot(data = bene_df, aes(x = topcode_hcc)) +
  geom_histogram(aes(y = after_stat(count / sum(count)))) +
  theme_classic() +
  coord_cartesian(expand = 0) +
  labs(x = "Number of Hierarchical Condition Categories", y = "Percent of Beneficiaries") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = c(0:9, "10+"), breaks = c(0:10)) +
  theme(text = element_text(size = 16), plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(plot.caption = element_text(hjust = 0, size = 10), plot.caption.position = "plot")
if (year == 2018) {
  ggsave(file.path("safe_figures", year, "n_hcc_histogram.png"), device = "png", width = 10, height = 7)
}

#### Baseline vs. Fair
bene_died <- bene_df$BENE_ID[bene_df$died == 1]

for (excl_payment_deaths in c(FALSE, TRUE)) {
  message("Excluding Payment Year Deaths: ", excl_payment_deaths)
  baseline <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_10cv.csv"))
  if (isTRUE(excl_payment_deaths)) {
    baseline <- baseline[!(BENE_ID %in% bene_died)]
  }
  baseline <- compute_performance(df = baseline)

  baseline <- unique(baseline[, .(rti_race, rmse, mae, median_abs_error, r2, is_net_compensation_rti, oos_net_compensation_rti, pred_ratio)])
  baseline <- baseline[, lapply(.SD, mean), by = "rti_race"]
  baseline <- baseline[, spec := "Baseline"]

  ## Post-Hoc Adjustment
  policy <- fread(file.path(sensitive_intermediates_dir, "policy_levels.csv"))
  naive <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_10cv.csv"))
  naive <- naive[, orig_pred := pred]
  for (race in unique(policy$rti_race)) {
    naive <- naive[
      rti_race == race & !is.na(policy$equity_policy[policy$rti_race == race]),
      pred := pred + (policy$equity_policy[policy$rti_race == race] - policy$is_net_compensation_rti[policy$rti_race == race])
    ]
  }
  naive <- naive[fold != fold_coefs, tot_diff := sum(pred - orig_pred), by = "fold_coefs"]
  naive <- naive[, tot_diff := mean(tot_diff, na.rm = T), by = "fold_coefs"]
  naive <- naive[fold != fold_coefs & rti_race == "Non-Hispanic White", n_nonhispanicwhite := .N, by = "fold_coefs"]
  naive <- naive[, n_nonhispanicwhite := mean(n_nonhispanicwhite, na.rm = T), by = "fold_coefs"]
  naive <- naive[rti_race == "Non-Hispanic White", pred := pred - (tot_diff / n_nonhispanicwhite)]

  if (isTRUE(excl_payment_deaths)) {
    naive <- naive[!(BENE_ID %in% bene_died)]
  }
  naive <- compute_performance(df = naive)

  naive <- unique(naive[, .(rti_race, rmse, mae, median_abs_error, r2, is_net_compensation_rti, oos_net_compensation_rti, pred_ratio)])
  naive <- naive[, lapply(.SD, mean), by = "rti_race"]
  naive <- naive[, spec := "Post-Process Disparities"]

  naive_disparities <- copy(naive)

  ## Post-Hoc Adjustment Five Pct
  policy <- fread(file.path(sensitive_intermediates_dir, "policy_levels.csv"))
  naive <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_10cv.csv"))
  naive <- naive[, orig_pred := pred]
  for (race in unique(policy$rti_race)) {
    naive <- naive[
      rti_race == race & !is.na(policy$five_pct[policy$rti_race == race]),
      pred := pred + (policy$five_pct[policy$rti_race == race] - policy$is_net_compensation_rti[policy$rti_race == race])
    ]
  }
  naive <- naive[fold != fold_coefs, tot_diff := sum(pred - orig_pred), by = "fold_coefs"]
  naive <- naive[, tot_diff := mean(tot_diff, na.rm = T), by = "fold_coefs"]
  naive <- naive[fold != fold_coefs & rti_race == "Non-Hispanic White", n_nonhispanicwhite := .N, by = "fold_coefs"]
  naive <- naive[, n_nonhispanicwhite := mean(n_nonhispanicwhite, na.rm = T), by = "fold_coefs"]
  naive <- naive[rti_race == "Non-Hispanic White", pred := pred - (tot_diff / n_nonhispanicwhite)]

  if (isTRUE(excl_payment_deaths)) {
    naive <- naive[!(BENE_ID %in% bene_died)]
  }
  naive <- compute_performance(df = naive)

  naive <- unique(naive[, .(rti_race, rmse, mae, median_abs_error, r2, is_net_compensation_rti, oos_net_compensation_rti, pred_ratio)])
  naive <- naive[, lapply(.SD, mean), by = "rti_race"]
  naive <- naive[, spec := "Post-Process Five Percent"]

  ## Fair Pro-Equity
  equity <- fread(file.path(sensitive_intermediates_dir, "pred_county_equity_policy_lambda_0_10cv.csv"))
  if (isTRUE(excl_payment_deaths)) {
    equity <- equity[!(BENE_ID %in% bene_died)]
  }
  equity <- compute_performance(df = equity)
  equity <- unique(equity[, .(rti_race, rmse, mae, median_abs_error, r2, is_net_compensation_rti, oos_net_compensation_rti, pred_ratio)])
  equity <- equity[, lapply(.SD, mean), by = "rti_race"]
  equity <- equity[, spec := "Constrained Regression Disparities"]

  ## Fair Pro-Equity
  five_pct <- fread(file.path(sensitive_intermediates_dir, "pred_county_five_pct_lambda_0_10cv.csv"))
  if (isTRUE(excl_payment_deaths)) {
    five_pct <- five_pct[!(BENE_ID %in% bene_died)]
  }
  five_pct <- compute_performance(df = five_pct)
  five_pct <- unique(five_pct[, .(rti_race, rmse, mae, median_abs_error, r2, is_net_compensation_rti, oos_net_compensation_rti, pred_ratio)])
  five_pct <- five_pct[, lapply(.SD, mean), by = "rti_race"]
  five_pct <- five_pct[, spec := "Constrained Regression Five Percent"]

  reg_fit <- rbind(baseline, naive_disparities, equity, naive, five_pct)

  reg_fit <- reg_fit[, is_net_compensation_rti := NULL]
  reg_fit <- reg_fit[, lapply(.SD, round), by = c("rti_race", "r2", "pred_ratio", "spec")]
  reg_fit <- reg_fit[, lapply(.SD, round, digits = 4), by = c("rti_race", "spec", "rmse", "mae", "median_abs_error", "oos_net_compensation_rti")]

  if (isFALSE(excl_payment_deaths)) {
    write.csv(reg_fit, file.path("safe_figures", year, "table2.csv"), na = "", row.names = FALSE)
  } else {
    write.csv(reg_fit, file.path("safe_figures", year, "table2_excl_pmt_deaths.csv"), na = "", row.names = FALSE)
  }
}

## Coefficient Comparison
baseline_coef <- fread(file.path(sensitive_intermediates_dir, "beta_county_baseline_lambda_0_full.csv"))
equity_coef <- fread(file.path(sensitive_intermediates_dir, "beta_county_equity_policy_lambda_0_full.csv"))
setnames(equity_coef, "beta", "equity")

coefs <- merge(baseline_coef, equity_coef, by = c("fold_coefs", "HCC"))
coefs <- coefs[!HCC %like% "grp_county"]
coefs <- coefs[!HCC %like% "Intercept"]
coefs <- coefs[!(HCC %in% c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10P"))]
coefs <- coefs[, equity_diff := equity - beta]
coefs <- coefs[, lapply(.SD, mean), by = "HCC"]
coefs <- coefs[, fold_coefs := NULL]

hcc_labs <- as.data.table(readxl::read_xlsx(file.path("safe_input", "CMS_Tbl_5_20ab.xlsx"), range = readxl::cell_cols("A:B")))
coefs <- merge(coefs, hcc_labs, by = "HCC", all.x = TRUE)
coefs <- coefs[, .(HCC, `HCC label`, beta, equity, equity_diff)]
coefs <- coefs[, lapply(.SD, round, digits = 2), by = c("HCC", "HCC label")]

write.csv(coefs, file.path("safe_figures", year, "coefs_table.csv"), na = "", row.names = FALSE)

## Prevalence
prevalence <- copy(combined)
prevalence <- prevalence[!(HCC %in% c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10P"))]
prevalence <- prevalence[, n_hcc_race := .N, by = c("HCC", "RTI_RACE_CD")]
prevalence <- prevalence[, n_hcc_all := .N, by = c("HCC")]
prevalence <- unique(prevalence[, .(HCC, RTI_RACE_CD, n_hcc_race, n_hcc_all)])
prevalence <- prevalence[HCC != ""]

#### Originally Disabled
orig_dis <- copy(combined)
orig_dis <- orig_dis[, n_hcc_race := sum(orig_dis), by = c("RTI_RACE_CD", "sex_grp")]
orig_dis <- orig_dis[, n_hcc_all := sum(orig_dis), by = c("sex_grp")]
orig_dis <- unique(orig_dis[, .(sex_grp, RTI_RACE_CD, n_hcc_race, n_hcc_all)])
orig_dis <- orig_dis[sex_grp == "Male", HCC := "orig_dis_m"]
orig_dis <- orig_dis[sex_grp == "Female", HCC := "orig_dis_f"]
orig_dis <- orig_dis[, sex_grp := NULL]

prevalence <- rbind(prevalence, orig_dis)

prevalence <- label_race(df = prevalence, race_code_version = "rti")

sample_size <- table1[variable == "sample_size"]
sample_size <- melt(sample_size, id.vars = "variable", variable.name = "rti_race")
prevalence <- merge(prevalence, sample_size[, .(rti_race, value)], by = "rti_race")
prevalence <- prevalence[, ss_all := sample_size$value[sample_size$rti_race == "All"]]
setnames(prevalence, "value", "ss_race")

prevalence <- prevalence[n_hcc_race < 50, n_hcc_race := NA]
prevalence <- prevalence[, ratio := (n_hcc_race / ss_race) / (n_hcc_all / ss_all)]

plot_data <- merge(coefs, prevalence, by = c("HCC"), all.x = TRUE)
plot_data <- plot_data[rti_race == "Other", rti_race := "Additional Group"]
plot_data <- plot_data[, rti_race := factor(rti_race, levels = c(
  "American Indian/Alaska Native", "Asian/Pacific Islander",
  "Black", "Hispanic", "Non-Hispanic White", "Additional Group"
))]

ratio_top <- 2.5
change_bottom <- -350
change_top <- 350

plot_data <- plot_data[, topcode_ratio := ratio]
plot_data <- plot_data[ratio > ratio_top, topcode_ratio := ratio_top]
plot_data <- plot_data[ratio > ratio_top, topcode_indic := 1]
plot_data <- plot_data[, topcode_change := equity_diff]
plot_data <- plot_data[equity_diff > change_top, topcode_change := change_top]
plot_data <- plot_data[equity_diff < change_bottom, topcode_change := change_bottom]
plot_data <- plot_data[ratio > ratio_top | equity_diff < change_bottom | equity_diff > change_top, topcode_indic := 1]
plot_data <- plot_data[!is.na(ratio) & !is.na(equity_diff) & is.na(topcode_indic), topcode_indic := 0]

ggplot(data = plot_data[!is.na(rti_race) & !(HCC %like% "age_sex")], aes(x = topcode_ratio, y = topcode_change, shape = as.factor(topcode_indic))) +
  geom_point(aes(size = n_hcc_race / ss_race), alpha = .7) +
  geom_vline(aes(xintercept = 1)) +
  geom_hline(aes(yintercept = 0)) +
  coord_cartesian(ylim = c(change_bottom, change_top), xlim = c(0, ratio_top)) +
  facet_wrap(~rti_race, scales = "free") +
  theme_bw() +
  labs(x = "Prevalence Ratio", y = "Change in HCC Payment", size = "HCC Prevalence") +
  scale_y_continuous(labels = scales::dollar, limits = c(change_bottom, change_top)) +
  scale_x_continuous(limits = c(0, ratio_top)) +
  scale_size_continuous(labels = scales::percent, breaks = c(0.05, 0.1, 0.15, 0.2)) +
  scale_shape_manual(values = c(16, 1), guide = "none") +
  theme(legend.position = "bottom", text = element_text(size = 16), plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(plot.caption = element_text(hjust = 0, size = 10), plot.caption.position = "plot", panel.grid.minor = element_blank())
ggsave(file.path("safe_figures", year, "coef_change_plot_equity.png"), device = "png", width = 10, height = 7)

##### Spending bar chart
policy <- fread(file.path(sensitive_intermediates_dir, "policy_levels.csv"))

baseline <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_full.csv"))
equity <- fread(file.path(sensitive_intermediates_dir, "pred_county_equity_policy_lambda_0_full.csv"))
five_pct <- fread(file.path(sensitive_intermediates_dir, "pred_county_five_pct_lambda_0_full.csv"))

baseline <- baseline[, baseline_pred := pred]
for (race in unique(policy$rti_race)) {
  baseline <- baseline[
    rti_race == race & !is.na(policy$equity_policy[policy$rti_race == race]),
    pred_disparities_pp := pred + (policy$equity_policy[policy$rti_race == race] - policy$is_net_compensation_rti[policy$rti_race == race])
  ]
}
for (race in unique(policy$rti_race)) {
  baseline <- baseline[
    rti_race == race & !is.na(policy$five_pct[policy$rti_race == race]),
    pred_fivepct_pp := pred + (policy$five_pct[policy$rti_race == race] - policy$is_net_compensation_rti[policy$rti_race == race])
  ]
}
baseline <- baseline[is.na(pred_disparities_pp), pred_disparities_pp := baseline_pred]
baseline <- baseline[is.na(pred_fivepct_pp), pred_fivepct_pp := baseline_pred]
baseline <- baseline[, tot_diff_disparities := sum(pred_disparities_pp - baseline_pred)]
baseline <- baseline[, tot_diff_fivepct := sum(pred_fivepct_pp - baseline_pred)]

baseline <- baseline[rti_race == "Non-Hispanic White", n_nonhispanicwhite := .N]
baseline <- baseline[rti_race == "Non-Hispanic White", pred_disparities_pp := pred_disparities_pp - (tot_diff_disparities / n_nonhispanicwhite)]
baseline <- baseline[rti_race == "Non-Hispanic White", pred_fivepct_pp := pred_fivepct_pp - (tot_diff_fivepct / n_nonhispanicwhite)]
baseline <- baseline[, .(BENE_ID, rti_race, cost, baseline_pred, pred_disparities_pp, pred_fivepct_pp)]

baseline <- merge(baseline, equity[, .(BENE_ID, pred)], by = "BENE_ID")
setnames(baseline, "pred", "constrained_disparities")
baseline <- merge(baseline, five_pct[, .(BENE_ID, pred)], by = "BENE_ID")
setnames(baseline, "pred", "constrained_fivepct")

baseline <- baseline[, BENE_ID := NULL]
baseline <- baseline[, lapply(.SD, mean), by = c("rti_race")]

setnames(
  baseline, c("cost", "baseline_pred", "pred_disparities_pp", "pred_fivepct_pp", "constrained_disparities", "constrained_fivepct"),
  c("Observed", "Baseline Regression", "Disparities: Post-Process", "Five Percent: Post-Process", "Disparities: Constrained Regression", "Five Percent: Constrained Regression")
)

baseline <- melt(baseline, id.vars = c("rti_race", "Observed"))
baseline <- baseline[rti_race == "Other", rti_race := "Additional Group"]
baseline <- baseline[, rti_race := factor(rti_race, levels = c(
  "American Indian/Alaska Native", "Asian/Pacific Islander",
  "Black", "Hispanic", "Non-Hispanic White", "Additional Group"
))]

baseline <- baseline[, variable := factor(variable, levels = c(
  "Baseline Regression", "Disparities: Constrained Regression", "Disparities: Post-Process",
  "Five Percent: Constrained Regression", "Five Percent: Post-Process"
))]

ggplot(data = baseline[rti_race != "Unknown"], aes(x = variable, y = value - Observed)) +
  geom_bar(stat = "identity") +
  facet_wrap(~rti_race) +
  theme_bw() +
  labs(x = "", y = "Net Compensation") +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

write.csv(baseline, file.path("safe_figures", year, "net_comp_table.csv"), na = "", row.names = FALSE)

##### Policy Scenarios
policy <- fread(file.path(Sys.getenv("MEDICARE_DATA_PATH"), "sensitive_intermediates", year, "policy_levels.csv"))
policy <- policy[, year := year]
policy <- policy[, .(rti_race, is_net_compensation_rti, equity_policy, five_pct, year)]
policy <- policy[order(rti_race, year)]
policy <- policy[, is_net_compensation_rti := round(is_net_compensation_rti)]
policy <- policy[, five_pct := round(five_pct)]
write.csv(policy, file.path("safe_figures", year, "policy_targets.csv"), na = "", row.names = FALSE)

disparities <- fread(file.path(Sys.getenv("MEDICARE_DATA_PATH"), "sensitive_intermediates", year, "disparities_targets.csv"))
disparities <- disparities[, year := year]
disparities <- disparities[order(rti_race, year)]
write.csv(disparities, file.path("safe_figures", year, "disparities_targets.csv"), na = "", row.names = FALSE)
