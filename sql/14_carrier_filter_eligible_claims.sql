WITH step_1 AS (
	SELECT
		_source_.BENE_ID AS BENE_ID,
		_source_.CLM_ID AS CLM_ID,
		_source_.HCPCS_CD AS HCPCS_CD,
		_source_.LINE_ICD_DGNS_CD AS LINE_ICD_DGNS_CD
	FROM
		_source_
		-- Step 1 (join):
		INNER JOIN `13_carrier_filter_eligible_hcpcs_output:ez0d` AS t1 ON _source_.BENE_ID = t1.BENE_ID
		AND _source_.CLM_ID = t1.CLM_ID
) 
SELECT
	BENE_ID,
	CLM_ID,
	HCPCS_CD,
	LINE_ICD_DGNS_CD
FROM
	`step_1`