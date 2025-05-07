## Estimates based on health status as an alternative exploration
run_disparities_regression <- function(year) {
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  bene_df <- fread(file.path(sensitive_intermediates_dir, "combined_df.csv"))

  ## Only keep raw HCCs (no interactions)
  bene_df <- bene_df[HCC == "" | (HCC %like% "HCC" & !HCC %like% "_")]
  bene_df <- bene_df[, num_hcc := .N, by = "BENE_ID"]
  bene_df <- bene_df[HCC == "", num_hcc := 0]

  bene_df <- unique(bene_df[, .(BENE_ID, cost, RTI_RACE_CD, orig_dis, age_grp, sex_grp, died, county_id, STATE_CODE, grp_county, num_hcc)])

  bene_df <- label_race(df = bene_df, race_code_version = "rti")
  bene_df <- bene_df[, age_sex := paste0(sex_grp, "_", age_grp)]

  mod_df <- bene_df[rti_race == "Non-Hispanic White"]

  mod_df <- mod_df[, county_n := .N, by = "county_id"]

  county_threshold <- fread(file.path(sensitive_intermediates_dir, "county_ss_threshold.csv"))
  mod_df <- mod_df[, grp_county := ifelse(county_n >= county_threshold$min_ss, county_id, paste0(as.numeric(STATE_CODE), "_grouped"))]

  # Further aggregation for very small grouped counties
  mod_df <- mod_df[, n_grp_county := .N, by = "grp_county"]
  mod_df <- mod_df[, grp_county := ifelse(n_grp_county >= county_threshold$min_ss, grp_county, "national_grouped")]

  # County to modeled county map
  county_map <- unique(mod_df[, .(county_id, grp_county)])

  bene_df <- bene_df[, orig_grp_county := grp_county]
  bene_df <- bene_df[, grp_county := NULL]
  bene_df <- merge(bene_df, county_map, by = "county_id", all.x = TRUE)

  bene_df <- bene_df[is.na(grp_county), grp_county := paste0(as.numeric(STATE_CODE), "_grouped")]
  bene_df <- bene_df[!(grp_county %in% unique(mod_df$grp_county)), grp_county := "national_grouped"]

  y <- mod_df$cost

  mat_df <- mod_df[, c("BENE_ID", "RTI_RACE_CD", "STATE_CODE", "county_id", "county_n", "cost", "died", "age_grp", "sex_grp", "rti_race", "n_grp_county") := NULL]
  X <- sparse.model.matrix(~., data = mat_df)

  rm(mod_df, mat_df)

  cv <- import("cvxpy", convert = TRUE)
  pd <- import("pandas", convert = TRUE)
  np <- import("numpy", convert = TRUE)

  py$pyX <- r_to_py(X, convert = FALSE)
  py$pyy <- r_to_py(y, convert = FALSE)

  py$beta <- cv$Variable(py_eval("pyX.shape[1]"))

  py$objective <- cv$norm2(py_eval("pyX @ beta - pyy"))
  py$prob <- cv$Problem(cv$Minimize(py$objective))
  py$result <- py$prob$solve(solver = "ECOS")
  py$prob$status

  beta_est <- data.table(HCC = colnames(X), beta = py$beta$value)

  ## GEO BETAS
  geo_beta <- beta_est[HCC %like% "grp_county"]
  geo_beta <- geo_beta[, HCC := gsub("grp_county", "", HCC, fixed = TRUE)]
  geo_beta <- rbind(data.table(HCC = "1_000", beta = 0), geo_beta)
  setnames(geo_beta, c("HCC", "beta"), c("grp_county", "geo_beta"))
  beta_est <- beta_est[!(HCC %like% "grp_county")]

  write.csv(beta_est, file.path("safe_figures", year, "disparities_targets_regression_coefficients.csv"))
  
  ## DEMO BETAS
  age_sex_beta <- beta_est[HCC %like% "age_sex"]
  age_sex_beta <- age_sex_beta[, HCC := gsub("age_sex", "", HCC, fixed = TRUE)]
  age_sex_beta <- rbind(data.table(HCC = "Female_65-69", beta = 0), age_sex_beta)
  setnames(age_sex_beta, c("HCC", "beta"), c("age_sex", paste0("age_sex_beta")))
  beta_est <- beta_est[!(HCC %like% "age_sex")]

  pred_df <- merge(bene_df, age_sex_beta, by = "age_sex")
  pred_df <- merge(pred_df, geo_beta, by = c("grp_county"))
  pred_df <- pred_df[, num_hcc_beta := beta_est$beta[beta_est$HCC == "num_hcc"]]
  pred_df <- pred_df[, intercept := beta_est$beta[beta_est$HCC == "(Intercept)"]]
  pred_df <- pred_df[, orig_dis_beta := beta_est$beta[beta_est$HCC == "orig_dis"]]

  pred_df <- pred_df[orig_dis == 0, orig_dis_beta := 0]
  pred_df <- pred_df[, num_hcc_beta := num_hcc_beta * num_hcc]

  pred_df <- pred_df[, .(rti_race, cost, age_sex_beta, geo_beta, num_hcc_beta, intercept, orig_dis_beta, orig_dis, num_hcc)]
  pred_df <- pred_df[, pred_county := intercept + age_sex_beta + num_hcc_beta + geo_beta + orig_dis_beta]

  pred_df <- pred_df[, .(cost, orig_dis, num_hcc, pred_county, rti_race)]
  pred_df <- pred_df[, lapply(.SD, mean), by = "rti_race"]
  pred_df <- pred_df[, orig_dis := orig_dis * 100]
  pred_df <- pred_df[, lapply(.SD, round), by = c("rti_race", "num_hcc")]
  pred_df <- pred_df[, num_hcc := round(num_hcc, 1)]

  write.csv(pred_df, file.path(sensitive_intermediates_dir, "disparities_targets.csv"), na = "", row.names = FALSE)
}
