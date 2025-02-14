WITH step_1 AS (
	SELECT
		v_BENE_ID,
		v_BENE_ENROLLMT_REF_YR,
		v_STATE_CODE,
		v_COUNTY_CD,
		v_ZIP_CD,
		v_AGE_AT_END_REF_YR,
		v_VALID_DEATH_DT_SW,
		v_BENE_DEATH_DT,
		v_SEX_IDENT_CD,
		v_BENE_RACE_CD,
		v_RTI_RACE_CD,
		v_ENTLMT_RSN_ORIG,
		v_ENTLMT_RSN_CURR,
		v_ESRD_IND,
		v_BENE_HI_CVRAGE_TOT_MONS,
		v_BENE_SMI_CVRAGE_TOT_MONS,
		v_BENE_HMO_CVRAGE_TOT_MONS,
		v_DUAL_ELGBL_MONS
	FROM
		-- Step 1 (stack)
		(
			SELECT
				BENE_ID AS v_BENE_ID,
				BENE_ENROLLMT_REF_YR AS v_BENE_ENROLLMT_REF_YR,
				STATE_CODE AS v_STATE_CODE,
				COUNTY_CD AS v_COUNTY_CD,
				ZIP_CD AS v_ZIP_CD,
				AGE_AT_END_REF_YR AS v_AGE_AT_END_REF_YR,
				VALID_DEATH_DT_SW AS v_VALID_DEATH_DT_SW,
				BENE_DEATH_DT AS v_BENE_DEATH_DT,
				SEX_IDENT_CD AS v_SEX_IDENT_CD,
				BENE_RACE_CD AS v_BENE_RACE_CD,
				RTI_RACE_CD AS v_RTI_RACE_CD,
				ENTLMT_RSN_ORIG AS v_ENTLMT_RSN_ORIG,
				ENTLMT_RSN_CURR AS v_ENTLMT_RSN_CURR,
				ESRD_IND AS v_ESRD_IND,
				BENE_HI_CVRAGE_TOT_MONS AS v_BENE_HI_CVRAGE_TOT_MONS,
				BENE_SMI_CVRAGE_TOT_MONS AS v_BENE_SMI_CVRAGE_TOT_MONS,
				BENE_HMO_CVRAGE_TOT_MONS AS v_BENE_HMO_CVRAGE_TOT_MONS,
				DUAL_ELGBL_MONS AS v_DUAL_ELGBL_MONS
			FROM
				_source_
			UNION ALL
			SELECT
				BENE_ID AS v_BENE_ID,
				BENE_ENROLLMT_REF_YR AS v_BENE_ENROLLMT_REF_YR,
				STATE_CODE AS v_STATE_CODE,
				COUNTY_CD AS v_COUNTY_CD,
				ZIP_CD AS v_ZIP_CD,
				AGE_AT_END_REF_YR AS v_AGE_AT_END_REF_YR,
				VALID_DEATH_DT_SW AS v_VALID_DEATH_DT_SW,
				BENE_DEATH_DT AS v_BENE_DEATH_DT,
				SEX_IDENT_CD AS v_SEX_IDENT_CD,
				BENE_RACE_CD AS v_BENE_RACE_CD,
				RTI_RACE_CD AS v_RTI_RACE_CD,
				ENTLMT_RSN_ORIG AS v_ENTLMT_RSN_ORIG,
				ENTLMT_RSN_CURR AS v_ENTLMT_RSN_CURR,
				ESRD_IND AS v_ESRD_IND,
				BENE_HI_CVRAGE_TOT_MONS AS v_BENE_HI_CVRAGE_TOT_MONS,
				BENE_SMI_CVRAGE_TOT_MONS AS v_BENE_SMI_CVRAGE_TOT_MONS,
				BENE_HMO_CVRAGE_TOT_MONS AS v_BENE_HMO_CVRAGE_TOT_MONS,
				DUAL_ELGBL_MONS AS v_DUAL_ELGBL_MONS
			FROM
				`stanfordphs.medicare_20_2019_2020_enrollment_summary:2ewt:v1_0.master_beneficiary_summary_file_mbsf_base_a_b_c_d:pncm`
		)
) 
SELECT
	v_AGE_AT_END_REF_YR AS AGE_AT_END_REF_YR,
	v_BENE_DEATH_DT AS BENE_DEATH_DT,
	v_BENE_ENROLLMT_REF_YR AS BENE_ENROLLMT_REF_YR,
	v_BENE_HI_CVRAGE_TOT_MONS AS BENE_HI_CVRAGE_TOT_MONS,
	v_BENE_HMO_CVRAGE_TOT_MONS AS BENE_HMO_CVRAGE_TOT_MONS,
	v_BENE_ID AS BENE_ID,
	v_BENE_RACE_CD AS BENE_RACE_CD,
	v_BENE_SMI_CVRAGE_TOT_MONS AS BENE_SMI_CVRAGE_TOT_MONS,
	v_COUNTY_CD AS COUNTY_CD,
	v_DUAL_ELGBL_MONS AS DUAL_ELGBL_MONS,
	v_ENTLMT_RSN_CURR AS ENTLMT_RSN_CURR,
	v_ESRD_IND AS ESRD_IND,
	v_RTI_RACE_CD AS RTI_RACE_CD,
	v_SEX_IDENT_CD AS SEX_IDENT_CD,
	v_STATE_CODE AS STATE_CODE,
	v_VALID_DEATH_DT_SW AS VALID_DEATH_DT_SW,
	v_ZIP_CD AS ZIP_CD,
	v_ENTLMT_RSN_ORIG AS ENTLMT_RSN_ORIG
FROM
	`step_1`