df <- fread(file.path(sensitive_intermediates_dir, "demographics.csv"))

race <- df[, .(BENE_ID, BENE_RACE_CD, RTI_RACE_CD)]
race <- race[, n_bene := .N, by = c("BENE_RACE_CD", "RTI_RACE_CD")]
race <- unique(race[, .(BENE_RACE_CD, RTI_RACE_CD, n_bene)])
race <- race[, pct_bene := n_bene / sum(n_bene)]

race <- label_race(df = race, race_code_version = "rti")
race <- label_race(df = race, race_code_version = "base")

race <- race[rti_race == "Other", rti_race := "Additional Group"]
race <- race[base_race == "Other", base_race := "Additional Group"]

race <- race[, rti_race := factor(rti_race, levels = rev(c(
  "American Indian/Alaska Native", "Asian/Pacific Islander",
  "Black", "Hispanic", "Non-Hispanic White", "Additional Group", "Unknown"
)))]

race <- race[, base_race := factor(base_race, levels = c(
  "American Indian/Alaska Native", "Asian/Pacific Islander",
  "Black", "Hispanic", "Non-Hispanic White", "Additional Group", "Unknown"
))]

plot <- ggplot(data = race[n_bene > 100], aes(x = base_race, y = rti_race)) +
  geom_tile(data = race[n_bene > 100 & rti_race == base_race], fill = "darkgray", color = "white", width = 1, height = 1) +
  geom_tile(data = race[n_bene > 100 & rti_race != base_race], fill = "lightgray", color = "white", width = 1, height = 1) +
  geom_text(aes(label = round(n_bene / 1000, 1))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Social Security Administration\nRace/Ethnicity Label", y = "Research Triangle Institute\nEnhanced Race/Ethnicity Label"
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme(text = element_text(size = 12)) +
  theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot")
ggsave(file.path("safe_figures", "race_eth_dist.svg"), plot = plot, device = "svg", width = 8, height = 7)
