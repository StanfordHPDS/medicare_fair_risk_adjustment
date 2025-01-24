run_regression <- function(year, geo_level, run_fair, run_cv, include_num_hcc, lambda, policy, non_negative_hcc) {
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  message("Running ", policy, " with lambda of ", lambda, " for year: ", year)

  if (isTRUE(run_fair)) {
    policy_levels <- fread(file.path(sensitive_intermediates_dir, "policy_levels.csv"))
    policy_levels <- policy_levels[, c("RTI_RACE_CD", paste0(policy)), with = FALSE][order(RTI_RACE_CD)]
  }

  if (isTRUE(run_cv)) {
    n.cores <- 10
  } else {
    n.cores <- 1
  }

  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "FORK"
  )

  doParallel::registerDoParallel(cl = my.cluster)

  beta_est <- NULL
  beta_est <- foreach(
    fold_run = 1:n.cores, .combine = "rbind",
    .packages = c("data.table", "reticulate", "Matrix")
  ) %dopar% {
    cv <- import("cvxpy", convert = TRUE)
    pd <- import("pandas", convert = TRUE)
    np <- import("numpy", convert = TRUE)

    if (isTRUE(run_cv)) {
      sampled_df <- as.data.table(read.csv(file.path(sensitive_intermediates_dir, "mod_df.csv")))
      sampled_df <- sampled_df[fold != fold_run]
    } else {
      sampled_df <- fread(file.path(sensitive_intermediates_dir, "mod_df.csv"))
    }

    ## Exclude number of condition indicators (alternatively, just exclude D1-D3 as CMS does not include additional payment in V24)
    if (isFALSE(include_num_hcc)) {
      sampled_df <- sampled_df[, c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10P") := NULL]
    }

    if (isTRUE(run_fair)) {
      py$pydf <- r_to_py(sampled_df, convert = FALSE)
    }

    group_constraints <- sampled_df[, c("cost", "RTI_RACE_CD"), with = FALSE]
    group_constraints <- group_constraints[order(RTI_RACE_CD)]
    group_constraints <- group_constraints[, .(grp_sum = sum(cost, na.rm = TRUE)), by = "RTI_RACE_CD"]
    group_constraints <- setNames(group_constraints$grp_sum, group_constraints$RTI_RACE_CD)

    group_n <- sampled_df[, c("cost", "RTI_RACE_CD"), with = FALSE]
    group_n <- group_n[order(RTI_RACE_CD)]
    group_n <- group_n[, .(grp_n = .N), by = "RTI_RACE_CD"]
    group_n <- setNames(group_n$grp_n, group_n$RTI_RACE_CD)

    y <- sampled_df$cost

    if (geo_level == "national") {
      mat_df <- sampled_df[, c("BENE_ID", "BENE_RACE_CD", "RTI_RACE_CD", "STATE_CODE", "county_id", "county_n", "grp_county", "cost", "died", "age_grp", "sex_grp", "fold", "orig_dis", "n_grp_county") := NULL]
      X <- sparse.model.matrix(~., data = mat_df)

      names_mat <- colnames(X)
      start_hcc_index <- 0
      end_hcc_index <- length(names_mat)
    }
    if (geo_level == "county") {
      mat_df <- sampled_df[, c("BENE_ID", "BENE_RACE_CD", "RTI_RACE_CD", "STATE_CODE", "county_id", "county_n", "cost", "died", "age_grp", "sex_grp", "fold", "orig_dis", "n_grp_county") := NULL]
      X <- sparse.model.matrix(~ as.factor(grp_county) + . - grp_county, data = mat_df)

      names_mat <- colnames(X)
      start_hcc_index <- as.integer(which(!(names_mat) %like% "grp_county")[2] - 1) ## first match is the intercept, so we want the second match. shift by 1 for 0 indexing in python.
      end_hcc_index <- as.integer(which((names_mat) %like% "age_sex")[1] - 1)
      rm(sampled_df, mat_df)
    }

    py$pyX <- r_to_py(X, convert = FALSE)
    py$pyy <- r_to_py(y, convert = FALSE)

    py$start_index <- as.integer(start_hcc_index)
    py$end_index <- as.integer(end_hcc_index)

    py$beta <- cv$Variable(py_eval("pyX.shape[1]"))

    if (isTRUE(run_fair)) {
      py$group_constraints <- r_to_py(group_constraints, convert = FALSE)
      py$group_n <- r_to_py(group_n, convert = FALSE)

      # Group indicator
      py$grp0_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 0].tolist()")
      py$grp1_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 1].tolist()")
      py$grp2_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 2].tolist()")
      py$grp3_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 3].tolist()")
      py$grp4_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 4].tolist()")
      py$grp5_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 5].tolist()")
      py$grp6_ind <- py_eval("pydf.index[pydf[\"RTI_RACE_CD\"] == 6].tolist()")

      # Set constraints
      py$beta <- cv$Variable(py_eval("pyX.shape[1]"))
      py$pred <- py_eval("pyX @ beta")

      py$constraints_0 <- cv$sum(py$pred[py$grp0_ind]) / py$group_n[1] == (py$group_constraints[1] / py$group_n[1] + policy_levels[RTI_RACE_CD == 0, get(paste0(policy))])
      py$constraints_1 <- cv$sum(py$pred[py$grp1_ind]) / py$group_n[2] == (py$group_constraints[2] / py$group_n[2] + policy_levels[RTI_RACE_CD == 1, get(paste0(policy))])
      py$constraints_2 <- cv$sum(py$pred[py$grp2_ind]) / py$group_n[3] == (py$group_constraints[3] / py$group_n[3] + policy_levels[RTI_RACE_CD == 2, get(paste0(policy))])
      py$constraints_3 <- cv$sum(py$pred[py$grp3_ind]) / py$group_n[4] == (py$group_constraints[4] / py$group_n[4] + policy_levels[RTI_RACE_CD == 3, get(paste0(policy))])
      py$constraints_4 <- cv$sum(py$pred[py$grp4_ind]) / py$group_n[5] == (py$group_constraints[5] / py$group_n[5] + policy_levels[RTI_RACE_CD == 4, get(paste0(policy))])
      py$constraints_5 <- cv$sum(py$pred[py$grp5_ind]) / py$group_n[6] == (py$group_constraints[6] / py$group_n[6] + policy_levels[RTI_RACE_CD == 5, get(paste0(policy))])
      py$constraints_6 <- cv$sum(py$pred[py$grp6_ind]) / py$group_n[7] == (py$group_constraints[7] / py$group_n[7] + policy_levels[RTI_RACE_CD == 6, get(paste0(policy))])

      py$constraints_all <- cv$sum(py$pred) == sum(y)

      py$constraints <- py_eval("[constraints_all]")

      if (!is.na(policy_levels[RTI_RACE_CD == 0, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_0]")
      }
      if (!is.na(policy_levels[RTI_RACE_CD == 1, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_1]")
      }
      if (!is.na(policy_levels[RTI_RACE_CD == 2, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_2]")
      }
      if (!is.na(policy_levels[RTI_RACE_CD == 3, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_3]")
      }
      if (!is.na(policy_levels[RTI_RACE_CD == 4, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_4]")
      }
      if (!is.na(policy_levels[RTI_RACE_CD == 5, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_5]")
      }
      if (!is.na(policy_levels[RTI_RACE_CD == 6, get(paste0(policy))])) {
        py$constraints <- py_eval("constraints + [constraints_6]")
      }

      if (isTRUE(non_negative_hcc)) {
        py$constraints <- py_eval("constraints + [beta[j] >= 0 for j in range(start_index, (end_index+1))]")
      }
      if (isTRUE(include_num_hcc)) {
        py$constraints <- py_eval("constraints + [beta[start_index+4] >= beta[start_index+3],
                                 beta[start_index+5] >= beta[start_index+4],
                                 beta[start_index+6] >= beta[start_index+5],
                                 beta[start_index+7] >= beta[start_index+6],
                                 beta[start_index+8] >= beta[start_index+7],
                                 beta[start_index+2] >= beta[start_index+8]]")
      }
    } else if (isTRUE(non_negative_hcc)) {
      py$constraints <- py_eval("[beta[j] >= 0 for j in range(start_index, (end_index+1))]")
      if (isTRUE(include_num_hcc)) {
        py$constraints <- py_eval("constraints + [beta[start_index+4] >= beta[start_index+3],
                                 beta[start_index+5] >= beta[start_index+4],
                                 beta[start_index+6] >= beta[start_index+5],
                                 beta[start_index+7] >= beta[start_index+6],
                                 beta[start_index+8] >= beta[start_index+7],
                                 beta[start_index+2] >= beta[start_index+8]]")
      }
    } else {
      if (isTRUE(include_num_hcc)) {
        py$constraints <- py_eval("[beta[start_index+4] >= beta[start_index+3],
                                 beta[start_index+5] >= beta[start_index+4],
                                 beta[start_index+6] >= beta[start_index+5],
                                 beta[start_index+7] >= beta[start_index+6],
                                 beta[start_index+8] >= beta[start_index+7],
                                 beta[start_index+2] >= beta[start_index+8]]")
      } else {
        py$constraints <- NULL
      }
    }

    if (lambda > 0) {
      py$objective <- cv$norm2(py_eval("pyX @ beta - pyy")) + lambda * cv$norm1(py_eval("beta[start_index:end_index]"))
    } else {
      py$objective <- cv$norm2(py_eval("pyX @ beta - pyy"))
    }
    py$prob <- cv$Problem(cv$Minimize(py$objective), py$constraints)
    if (geo_level == "national") {
      py$result <- py$prob$solve(solver = "ECOS", feastol = 0.001)
    } else {
      py$result <- py$prob$solve(solver = "ECOS")
    }
    py$prob$status

    beta_fold <- data.table(fold_coefs = fold_run, HCC = colnames(X), beta = py$beta$value)
  }

  parallel::stopCluster(cl = my.cluster)

  # beta_est <- beta_est[, beta := round(beta, 4)] ## For penalized regression, set to effective zero

  if (isTRUE(non_negative_hcc)) {
    if (isTRUE(run_cv)) {
      write.csv(beta_est, file.path(sensitive_intermediates_dir, paste0("beta_", geo_level, "_", policy, "_lambda_", lambda, "_10cv.csv")), na = "", row.names = FALSE)
    } else {
      write.csv(beta_est, file.path(sensitive_intermediates_dir, paste0("beta_", geo_level, "_", policy, "_lambda_", lambda, "_full.csv")), na = "", row.names = FALSE)
    }
  } else {
    if (isTRUE(run_cv)) {
      write.csv(beta_est, file.path(sensitive_intermediates_dir, paste0("beta_", geo_level, "_", policy, "_lambda_", lambda, "_no_hcc_constraint_10cv.csv")), na = "", row.names = FALSE)
    } else {
      write.csv(beta_est, file.path(sensitive_intermediates_dir, paste0("beta_", geo_level, "_", policy, "_lambda_", lambda, "_no_hcc_constraint_full.csv")), na = "", row.names = FALSE)
    }
  }


  if (geo_level == "county") {
    ## GEO BETAS
    geo_beta <- beta_est[HCC %like% "grp_county"]
    geo_beta <- geo_beta[, HCC := gsub("as.factor(grp_county)", "", HCC, fixed = TRUE)]
    if (isTRUE(run_cv)) {
      geo_beta <- rbind(data.table(HCC = "1_000", beta = 0, fold_coefs = 1:10), geo_beta)
    } else {
      geo_beta <- rbind(data.table(HCC = "1_000", beta = 0, fold_coefs = 1), geo_beta)
    }
    setnames(geo_beta, c("HCC", "beta"), c("grp_county", "geo_beta"))
    beta_est <- beta_est[!(HCC %like% "grp_county")]
  }

  ## DEMO BETAS
  age_sex_beta <- beta_est[HCC %like% "age_sex"]
  age_sex_beta <- age_sex_beta[, HCC := gsub("age_sex", "", HCC, fixed = TRUE)]
  if (isTRUE(run_cv)) {
    age_sex_beta <- rbind(data.table(HCC = "Female_65-69", beta = 0, fold_coefs = 1:10), age_sex_beta)
  } else {
    age_sex_beta <- rbind(data.table(HCC = "Female_65-69", beta = 0, fold_coefs = 1), age_sex_beta)
  }
  setnames(age_sex_beta, c("HCC", "beta"), c("age_sex", paste0("age_sex_beta")))
  beta_est <- beta_est[!(HCC %like% "age_sex")]

  ## INTERCEPT
  intercept <- beta_est[beta_est$HCC == "(Intercept)"]
  intercept <- intercept[, "HCC" := NULL]
  setnames(intercept, "beta", "intercept")
  beta_est <- beta_est[HCC != "(Intercept)"]

  ## Originally Disabled Female
  disabled_f <- beta_est[beta_est$HCC == "orig_dis_f"]
  disabled_f <- disabled_f[, "HCC" := NULL]
  setnames(disabled_f, "beta", "orig_dis_f_val")
  beta_est <- beta_est[HCC != "orig_dis_f"]

  ## Originally Disabled Male
  disabled_m <- beta_est[beta_est$HCC == "orig_dis_m"]
  disabled_m <- disabled_m[, "HCC" := NULL]
  setnames(disabled_m, "beta", "orig_dis_m_val")
  beta_est <- beta_est[HCC != "orig_dis_m"]

  pred_df <- fread(file.path(sensitive_intermediates_dir, "combined_df.csv"))
  pred_df <- pred_df[, age_sex := paste0(sex_grp, "_", age_grp)]
  pred_df <- merge(pred_df, age_sex_beta, by = "age_sex", all.x = TRUE, allow.cartesian = TRUE)
  if (geo_level == "county") {
    pred_df <- merge(pred_df, geo_beta, by = c("grp_county", "fold_coefs"), all.x = TRUE)
  } else {
    pred_df <- pred_df[, geo_beta := 0]
  }
  pred_df <- merge(pred_df, beta_est, by = c("HCC", "fold_coefs"), all.x = TRUE)
  pred_df <- merge(pred_df, intercept, by = c("fold_coefs"), all.x = TRUE)
  pred_df <- merge(pred_df, disabled_f, by = c("fold_coefs"), all.x = TRUE)
  pred_df <- merge(pred_df, disabled_m, by = c("fold_coefs"), all.x = TRUE)
  pred_df <- pred_df[orig_dis == 0 | sex_grp == "Female", orig_dis_m_val := 0]
  pred_df <- pred_df[orig_dis == 0 | sex_grp == "Male", orig_dis_f_val := 0]

  pred_df <- pred_df[, hcc_beta := sum(beta, na.rm = TRUE), by = c("BENE_ID", "fold_coefs")]

  pred_df <- unique(pred_df[, .(STATE_CODE, BENE_ID, BENE_RACE_CD, RTI_RACE_CD, age_grp, sex_grp, county_id, grp_county, cost, age_sex_beta, geo_beta, hcc_beta, intercept, orig_dis_m_val, orig_dis_f_val, fold_coefs)])
  pred_df <- pred_df[, pred := intercept + age_sex_beta + hcc_beta + geo_beta + orig_dis_m_val + orig_dis_f_val]

  ## Metrics
  if (isTRUE(run_cv)) {
    bene_fold <- fread(file.path(sensitive_intermediates_dir, "bene_fold.csv"))
    pred_df <- merge(pred_df, bene_fold, by = "BENE_ID")
  } else {
    pred_df <- pred_df[, fold := 1]
  }

  pred_df <- label_race(df = pred_df, race_code_version = "base")
  pred_df <- label_race(df = pred_df, race_code_version = "rti")

  if (isTRUE(non_negative_hcc)) {
    if (isTRUE(run_cv)) {
      fwrite(pred_df, file.path(sensitive_intermediates_dir, paste0("pred_", geo_level, "_", policy, "_lambda_", lambda, "_10cv.csv")), na = "", row.names = FALSE)
    } else {
      fwrite(pred_df, file.path(sensitive_intermediates_dir, paste0("pred_", geo_level, "_", policy, "_lambda_", lambda, "_full.csv")), na = "", row.names = FALSE)
    }
  } else {
    if (isTRUE(run_cv)) {
      fwrite(pred_df, file.path(sensitive_intermediates_dir, paste0("pred_", geo_level, "_", policy, "_lambda_", lambda, "_no_hcc_constraint_10cv.csv")), na = "", row.names = FALSE)
    } else {
      fwrite(pred_df, file.path(sensitive_intermediates_dir, paste0("pred_", geo_level, "_", policy, "_lambda_", lambda, "_no_hcc_constraint_full.csv")), na = "", row.names = FALSE)
    }
  }
  gc()
}
