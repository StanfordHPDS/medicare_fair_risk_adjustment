WITH steps_1_3 AS (
	SELECT
		-- Step 2 (create variables):
		_source_.BENE_ID AS BENE_ID,
		_source_.CLM_ID AS CLM_ID,
		_source_.HCPCS_CD AS HCPCS_CD,
		_source_.LINE_ICD_DGNS_CD AS LINE_ICD_DGNS_CD,
		_source_.BENE_ENROLLMT_REF_YR AS BENE_ENROLLMT_REF_YR,
		MIN(_source_.BENE_ENROLLMT_REF_YR) OVER () AS v_min_year
	FROM
		_source_
		-- Step 1 (join):
		INNER JOIN `demo:1mds` AS t1 ON _source_.BENE_ID = t1.BENE_ID
		AND _source_.BENE_ENROLLMT_REF_YR = t1.BENE_ENROLLMT_REF_YR
	QUALIFY
		-- Step 3 (rowFilter):
		(BENE_ENROLLMT_REF_YR = v_min_year)
) 
SELECT DISTINCT
	BENE_ID,
	CLM_ID,
	HCPCS_CD,
	LINE_ICD_DGNS_CD
FROM
	`steps_1_3`