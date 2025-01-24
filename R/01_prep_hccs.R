# Author: Marissa Reitsma
# Script that reads in ICDs associated with inpatient, outpatient, and carrier claims and maps them to HCCs

prep_hccs <- function(year) {
  message(paste0("Running Script to Map ICDs to HCCs: ", year))

  data_dir <- set_data_dir(year = year)
  sensitive_intermediates_dir <- set_intermediates_dir(year = year)

  # PREPARE OUTPATIENT CLAIMS FOR MERGE TO HCC MAP -------------------------------
  message("Mapping Outpatient Claims")
  ## Read in data from Redivis, melt wide to long so each row is associated with one ICD
  claim_df <- fread(file.path(data_dir, "/outpatient.csv")) # Download from Redivis
  setnames(claim_df, "PRNCPAL_DGNS_CD", "ICD_DGNS_CD0") # Consistent naming for the melt
  claim_df <- melt(claim_df, id.vars = c("BENE_ID", "CLM_ID"), value.name = "Diagnosis_Code") # Transform wide to long
  claim_df <- claim_df[Diagnosis_Code != ""] # Not all claims have the maximum number of possible diagnosis codes
  claim_df <- unique(claim_df[, .(BENE_ID, Diagnosis_Code)]) # We only need one ICD per bene per year to assign HCC

  ## MERGE ICDs to HCC MAP --------------------------------------------
  ## We are merging the ICD codes to the HCC map (many-to-many). Use allow.cartesion to match all ICDs to all corresponding HCCs.
  outpatient_hcc <- merge(claim_df, hcc_map, by = "Diagnosis_Code", allow.cartesian = TRUE) ## Many ICDs are not part of included payment HCCs, these rows will be dropped and this is ok.

  outpatient_hcc <- unique(outpatient_hcc[, .(BENE_ID, HCC)]) ## Only need to record HCCs once per beneficiary-year

  rm(claim_df)

  # PREPARE CARRIER CLAIMS FOR MERGE TO HCC MAP -------------------------------
  message("Mapping Carrier Claims")

  claim_df <- fread(file.path(data_dir, "/Carrier.csv")) # Download from Redivis
  setnames(claim_df, "LINE_ICD_DGNS_CD", "Diagnosis_Code")

  ## MERGE ICDs to HCC MAP --------------------------------------------
  carrier_hcc <- merge(claim_df, hcc_map, by = "Diagnosis_Code", allow.cartesian = TRUE) ## Many ICDs are not part of included payment HCCs, these rows will be dropped and this is ok.
  carrier_hcc <- unique(carrier_hcc[, .(BENE_ID, HCC)])

  rm(claim_df)

  # PREPARE INPATIENT (MEDPAR) CLAIMS FOR MERGE TO HCC MAP -------------------------------
  message("Mapping MEDPAR Claims")

  claim_df <- fread(file.path(data_dir, "medpar.csv")) # Download from Redivis

  ## Similar transform (wide-to-long) as outpatient
  setnames(claim_df, "ADMTG_DGNS_CD", "DGNS_0_CD")
  claim_df <- melt(claim_df, id.vars = c("BENE_ID"), value.name = "Diagnosis_Code")
  claim_df <- claim_df[Diagnosis_Code != ""]
  claim_df <- unique(claim_df[, .(BENE_ID, Diagnosis_Code)])

  ## MERGE ICDs to HCC MAP --------------------------------------------
  medpar_hcc <- merge(claim_df, hcc_map, by = "Diagnosis_Code", allow.cartesian = TRUE) ## Many ICDs are not part of included payment HCCs, these rows will be dropped and this is ok.
  medpar_hcc <- unique(medpar_hcc[, .(BENE_ID, HCC)])

  rm(claim_df)

  # BRING SEPARATE SOURCES OF HCCs TOGETHER AND SAVE OUTPUT ----------------------------------
  ## Only need unique HCCs by beneficiary
  all_hcc <- rbind(carrier_hcc, medpar_hcc, outpatient_hcc)
  all_hcc <- unique(all_hcc)

  rm(carrier_hcc, medpar_hcc, outpatient_hcc)

  # IMPOSE HIERARCHIES ------------------------------------
  message("Imposing Hierarchies")
  all_hcc <- all_hcc[, indic := 1]
  all_hcc <- dcast(all_hcc, BENE_ID ~ HCC, value.var = "indic")

  # Neoplasms
  all_hcc <- all_hcc[`8` == 1, c("9", "10", "11", "12") := NA]
  all_hcc <- all_hcc[`9` == 1, c("10", "11", "12") := NA]
  all_hcc <- all_hcc[`10` == 1, c("11", "12") := NA]
  all_hcc <- all_hcc[`11` == 1, c("12") := NA]
  # Diabetes
  all_hcc <- all_hcc[`17` == 1, c("18", "19") := NA]
  all_hcc <- all_hcc[`18` == 1, c("19") := NA]
  # Liver
  all_hcc <- all_hcc[`27` == 1, c("28", "29", "80") := NA]
  all_hcc <- all_hcc[`28` == 1, c("29") := NA]
  # Blood
  all_hcc <- all_hcc[`46` == 1, c("48") := NA]
  # Cognitive
  all_hcc <- all_hcc[`51` == 1, c("52") := NA]
  # SUD
  all_hcc <- all_hcc[`54` == 1, c("55", "56") := NA]
  all_hcc <- all_hcc[`55` == 1, c("56") := NA]
  # Psychiatric
  all_hcc <- all_hcc[`57` == 1, c("58", "59", "60") := NA]
  all_hcc <- all_hcc[`58` == 1, c("59", "60") := NA]
  all_hcc <- all_hcc[`59` == 1, c("60") := NA]
  # Spinal
  all_hcc <- all_hcc[`70` == 1, c("71", "72", "103", "104", "169") := NA]
  all_hcc <- all_hcc[`71` == 1, c("72", "104", "169") := NA]
  all_hcc <- all_hcc[`72` == 1, c("169") := NA]
  # Arrest
  all_hcc <- all_hcc[`82` == 1, c("83", "84") := NA]
  all_hcc <- all_hcc[`83` == 1, c("84") := NA]
  # Heart
  all_hcc <- all_hcc[`86` == 1, c("87", "88") := NA]
  all_hcc <- all_hcc[`87` == 1, c("88") := NA]
  # CVD
  all_hcc <- all_hcc[`99` == 1, c("100") := NA]
  all_hcc <- all_hcc[`103` == 1, c("104") := NA]
  # Vascular
  all_hcc <- all_hcc[`106` == 1, c("107", "108", "161", "189") := NA]
  all_hcc <- all_hcc[`107` == 1, c("108") := NA]
  # Lung
  all_hcc <- all_hcc[`110` == 1, c("111", "112") := NA]
  all_hcc <- all_hcc[`111` == 1, c("112") := NA]
  all_hcc <- all_hcc[`114` == 1, c("115") := NA]
  # Kidney
  all_hcc <- all_hcc[`134` == 1, c("135", "136", "137", "138") := NA]
  all_hcc <- all_hcc[`135` == 1, c("136", "137", "138") := NA]
  all_hcc <- all_hcc[`136` == 1, c("137", "138") := NA]
  all_hcc <- all_hcc[`137` == 1, c("138") := NA]
  # Skin
  all_hcc <- all_hcc[`157` == 1, c("158", "159", "161") := NA]
  all_hcc <- all_hcc[`158` == 1, c("159", "161") := NA]
  all_hcc <- all_hcc[`159` == 1, c("161") := NA]
  # Injury
  all_hcc <- all_hcc[`166` == 1, c("80", "167") := NA]

  all_hcc <- melt(all_hcc, id.vars = "BENE_ID")
  all_hcc <- all_hcc[!is.na(value)]
  setnames(all_hcc, "variable", "HCC")
  all_hcc <- all_hcc[, value := NULL]

  ## Prepare rows that are indicators of joint HCCs (these are specific to V24)
  interaction_df <- copy(all_hcc)
  interaction_df <- interaction_df[HCC %in% c(8, 9, 10, 11, 12), cancer := 1]
  interaction_df <- interaction_df[HCC %in% c(17, 18, 19), diabetes := 1]
  interaction_df <- interaction_df[HCC %in% c(85), chf := 1]
  interaction_df <- interaction_df[HCC %in% c(110, 111, 112), copdcf := 1]
  interaction_df <- interaction_df[HCC %in% c(134, 135, 136, 137, 138), renal := 1]
  interaction_df <- interaction_df[HCC %in% c(82, 83, 84), cardresp := 1]

  ## To discuss: is there a faster way to yield the desired behavior of flagging all rows for beneficiary if any rows meet condition?
  for (i in names(interaction_df)[!names(interaction_df) %in% c("BENE_ID", "HCC")]) {
    message(i)
    interaction_df <- interaction_df[, paste0(i) := max(get(i), na.rm = T), by = "BENE_ID"]
    interaction_df <- interaction_df[, paste0(i) := ifelse(get(i) == 1, 1, 0)]
  }

  ## Make the names match what is used in the CMS software (for later merge)
  interaction_df <- interaction_df[HCC == 47 & cancer == 1, int1 := "HCC47_gCancer"]
  interaction_df <- interaction_df[diabetes == 1 & chf == 1, int2 := "DIABETES_CHF"]
  interaction_df <- interaction_df[chf == 1 & copdcf == 1, int3 := "CHF_gCopdCF"]
  interaction_df <- interaction_df[chf == 1 & renal == 1, int4 := "HCC85_gRenal_V24"]
  interaction_df <- interaction_df[copdcf == 1 & cardresp == 1, int5 := "gCopdCF_CARD_RESP_FAIL"]
  interaction_df <- interaction_df[chf == 1 & HCC == 96, int6 := "HCC85_HCC96"]

  ## Prepare these new rows to match original hcc_df format (for later append)
  interaction_df <- unique(interaction_df[, .(BENE_ID, int1, int2, int3, int4, int5, int6)])
  interaction_df <- melt(interaction_df, id.vars = "BENE_ID")
  interaction_df <- interaction_df[!is.na(value)]
  interaction_df <- interaction_df[, variable := NULL]
  setnames(interaction_df, "value", "HCC")
  interaction_df <- unique(interaction_df)

  ## Prepare rows that are indicators of the number of HCCs (not including joint HCCs prepared above)
  num_hcc_df <- copy(all_hcc)
  num_hcc_df <- num_hcc_df[, num_hcc := .N, by = "BENE_ID"]
  num_hcc_df <- num_hcc_df[, HCC := NULL]
  setnames(num_hcc_df, "num_hcc", "HCC")
  num_hcc_df <- unique(num_hcc_df)
  num_hcc_df <- num_hcc_df[, HCC := ifelse(HCC > 10, 10, HCC)] ## Upper limit is 10
  num_hcc_df <- num_hcc_df[, HCC := paste0("D", HCC)]
  num_hcc_df <- num_hcc_df[HCC == "D10", HCC := "D10P"]

  ## Create a full dataset with all three score components
  all_hcc <- all_hcc[, HCC := paste0("HCC", HCC)]
  all_hcc <- rbind(all_hcc, interaction_df, num_hcc_df)

  # Save Output -------------------------------------------
  write.csv(all_hcc, file.path(sensitive_intermediates_dir, "mapped_hcc.csv"), na = "", row.names = F)

  message("Finished Script to Map ICDs to HCCs")
}
