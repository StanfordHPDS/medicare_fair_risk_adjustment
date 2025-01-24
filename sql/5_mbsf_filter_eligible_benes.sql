WITH steps_1_3 AS (
	SELECT
		-- Step 2 (create variables):
		AGE_AT_END_REF_YR,
		BENE_DEATH_DT,
		BENE_ENROLLMT_REF_YR,
		BENE_HI_CVRAGE_TOT_MONS,
		BENE_HMO_CVRAGE_TOT_MONS,
		BENE_ID,
		BENE_RACE_CD,
		BENE_SMI_CVRAGE_TOT_MONS,
		COUNTY_CD,
		DUAL_ELGBL_MONS,
		ENTLMT_RSN_CURR,
		ESRD_IND,
		RTI_RACE_CD,
		SEX_IDENT_CD,
		STATE_CODE,
		VALID_DEATH_DT_SW,
		ZIP_CD,
		ENTLMT_RSN_ORIG,
		COUNT(*) OVER (
			PARTITION BY
				BENE_ID
		) AS v_N_Years
	FROM
		_source_
	WHERE
		-- Step 1 (rowFilter):
		(
			(
				BENE_SMI_CVRAGE_TOT_MONS = 12
				AND BENE_HI_CVRAGE_TOT_MONS = 12
				AND BENE_HMO_CVRAGE_TOT_MONS = 0
				AND DUAL_ELGBL_MONS = 0
				AND ESRD_IND = '0'
				AND ENTLMT_RSN_CURR = 0
			)
			OR (
				BENE_DEATH_DT >= '2019-01-01'
				AND BENE_DEATH_DT <= '2019-12-31'
			)
		)
	QUALIFY
		-- Step 3 (rowFilter):
		(v_N_Years = 2)
) 
SELECT
	AGE_AT_END_REF_YR,
	BENE_DEATH_DT,
	BENE_ENROLLMT_REF_YR,
	BENE_HI_CVRAGE_TOT_MONS,
	BENE_HMO_CVRAGE_TOT_MONS,
	BENE_ID,
	BENE_RACE_CD,
	BENE_SMI_CVRAGE_TOT_MONS,
	COUNTY_CD,
	DUAL_ELGBL_MONS,
	ENTLMT_RSN_CURR,
	ESRD_IND,
	RTI_RACE_CD,
	SEX_IDENT_CD,
	STATE_CODE,
	VALID_DEATH_DT_SW,
	ZIP_CD,
	ENTLMT_RSN_ORIG
FROM
	`steps_1_3`