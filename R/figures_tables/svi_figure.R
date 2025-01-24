## SVI ANALYSIS
policy <- fread(file.path(sensitive_intermediates_dir, "policy_levels.csv"))

baseline <- fread(file.path(sensitive_intermediates_dir, "pred_county_baseline_lambda_0_full.csv"))

cost_df <- baseline[, .(BENE_ID, cost)]
mean_cost <- mean(baseline$cost)

equity <- fread(file.path(sensitive_intermediates_dir, "pred_county_equity_policy_lambda_0_full.csv"))

baseline <- baseline[, baseline_pred := pred]
for (race in unique(policy$rti_race)) {
  baseline <- baseline[
    rti_race == race & !is.na(policy$equity_policy[policy$rti_race == race]),
    pred := pred + (policy$equity_policy[policy$rti_race == race] - policy$is_net_compensation_rti[policy$rti_race == race])
  ]
}
baseline <- baseline[, tot_diff := sum(pred - baseline_pred)]
baseline <- baseline[rti_race == "Non-Hispanic White", n_nonhispanicwhite := .N]
baseline <- baseline[rti_race == "Non-Hispanic White", pred := pred - (tot_diff / n_nonhispanicwhite)]
setnames(baseline, "pred", "post_process")
baseline <- baseline[, .(BENE_ID, rti_race, baseline_pred, post_process)]

preds <- merge(baseline, equity[, .(BENE_ID, pred)], by = "BENE_ID")
setnames(preds, "pred", "constrained")
setnames(preds, "baseline_pred", "baseline")

demo <- fread(file.path(data_dir, "Demo.csv"), colClasses = c(COUNTY_CD = "character"))
demo <- demo[BENE_ENROLLMT_REF_YR == year]
demo <- demo[, .(BENE_ID, STATE_CODE, COUNTY_CD)]
demo <- demo[, ssacd := as.numeric(paste0(STATE_CODE, COUNTY_CD))]
demo <- demo[, .(BENE_ID, ssacd)]

preds <- merge(preds, demo, by = "BENE_ID", all.x = TRUE)

svi <- fread(file.path("safe_input", "SVI_2018_US_county.csv"))
svi <- svi[, .(FIPS, RPL_THEME1)]
svi <- svi[RPL_THEME1 < 0, RPL_THEME1 := NA]

fips <- fread(file.path("safe_input", "ssa_fips_state_county2018.csv"))

preds <- merge(preds, fips, by = "ssacd", all.x = TRUE)
preds <- merge(preds, svi, by.x = "fipscounty", by.y = "FIPS", all.x = TRUE)
nrow(preds[!is.na(RPL_THEME1)]) / nrow(preds)

for (decile in seq(0.2, 1, .2)) {
  for (v in c("baseline", "post_process", "constrained")) {
    if (decile != 1) {
      preds <- preds[RPL_THEME1 >= (decile - 0.20001) & RPL_THEME1 < (decile), paste0("all_", v) := mean(get(v))]
      preds <- preds[RPL_THEME1 >= (decile - 0.20001) & RPL_THEME1 < (decile), paste0("race_", v) := mean(get(v)), by = "rti_race"]
      preds <- preds[RPL_THEME1 >= (decile - 0.20001) & RPL_THEME1 < (decile), svi_dec := paste0("SVI ", decile - 0.2, "-", decile - .01)]
    } else {
      preds <- preds[RPL_THEME1 >= (decile - 0.20001), paste0("all_", v) := mean(get(v))]
      preds <- preds[RPL_THEME1 >= (decile - 0.20001), paste0("race_", v) := mean(get(v)), by = "rti_race"]
      preds <- preds[RPL_THEME1 >= (decile - 0.20001), svi_dec := "SVI 0.8-1.0"]
    }
  }
}
preds <- preds[!is.na(RPL_THEME1)]
preds <- preds[, ss_race := .N, by = c("rti_race", "svi_dec")]
preds <- preds[, ss_race_tot := .N, by = "rti_race"]

preds <- merge(preds, cost_df, by = "BENE_ID")
preds <- preds[, cost_svi := mean(cost), by = c("rti_race", "svi_dec")]

preds <- unique(preds[, .(rti_race, svi_dec, ss_race_tot, ss_race, cost_svi, all_baseline, race_baseline, all_post_process, race_post_process, all_constrained, race_constrained)])

preds <- preds[, diff_post_process := race_post_process - race_baseline]
preds <- preds[, diff_constrained := race_constrained - race_baseline]
preds <- preds[, diff_algos := race_constrained - race_post_process]
preds <- preds[, diff_obs := cost_svi - race_baseline]
preds <- preds[, pct_race_svi_ss := ss_race / ss_race_tot]

preds <- preds[rti_race == "Other", rti_race := "Additional Group"]
preds <- preds[, rti_race := factor(rti_race, levels = c(
  "American Indian/Alaska Native", "Asian/Pacific Islander",
  "Black", "Hispanic", "Non-Hispanic White", "Additional Group"
))]

## Difference vs. Post-Process

plot_data <- preds[, .(rti_race, svi_dec, diff_algos, pct_race_svi_ss)]
plot_data <- plot_data[!is.na(rti_race)]

plot_data <- melt(plot_data, id.vars = c("rti_race", "svi_dec", "pct_race_svi_ss"))

ggplot(data = plot_data, aes(x = as.numeric(as.factor(svi_dec)), y = value)) +
  geom_line(linewidth = 1.5, alpha = 0.8, color = "#F98125") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~rti_race, scales = "free") +
  scale_y_continuous(labels = scales::dollar, limits = c(-770, 700), breaks = c(-500, -250, 0, 250, 500)) +
  scale_x_continuous(limits = c(0.75, 5.25)) +
  labs(
    x = "Quintile of Exposure to Adverse Socioeconomic Factors",
    y = "Difference", color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 16), panel.grid.minor = element_blank()) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10), plot.caption.position = "plot",
    legend.text = element_text(size = 18), legend.key.size = unit(2, "cm")
  ) +
  geom_text(aes(y = -770, label = label_percent(accuracy = 1)(pct_race_svi_ss)), color = "black") +
  guides(color = guide_legend(override.aes = list(linewidth = 3)))
ggsave(file.path("safe_figures", year, "svi_lines.png"), device = "png", width = 10, height = 7)
