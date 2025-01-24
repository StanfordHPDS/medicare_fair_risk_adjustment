WITH step_1 AS (
	SELECT
		v_BENE_ID,
		v_CLM_ID,
		v_CLM_THRU_DT,
		v_CLM_LINE_NUM,
		v_HCPCS_CD
	FROM
		-- Step 1 (stack)
		(
			SELECT
				BENE_ID AS v_BENE_ID,
				CLM_ID AS v_CLM_ID,
				CLM_THRU_DT AS v_CLM_THRU_DT,
				CLM_LINE_NUM AS v_CLM_LINE_NUM,
				HCPCS_CD AS v_HCPCS_CD
			FROM
				_source_
			UNION ALL
			SELECT
				BENE_ID AS v_BENE_ID,
				CLM_ID AS v_CLM_ID,
				CLM_THRU_DT AS v_CLM_THRU_DT,
				CLM_LINE_NUM AS v_CLM_LINE_NUM,
				HCPCS_CD AS v_HCPCS_CD
			FROM
				`stanfordphs.medicare_20_2019_2020_outpatient:e3fc:v1_0.outpatient_revenue_center_file:aaph`
		)
) 
SELECT
	v_BENE_ID AS BENE_ID,
	v_CLM_ID AS CLM_ID,
	v_CLM_THRU_DT AS CLM_THRU_DT,
	v_CLM_LINE_NUM AS CLM_LINE_NUM,
	v_HCPCS_CD AS HCPCS_CD
FROM
	`step_1`