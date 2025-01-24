WITH step_1 AS (
	SELECT
		_source_.BENE_ID AS BENE_ID,
		_source_.CLM_ID AS CLM_ID
	FROM
		_source_
		-- Step 1 (join):
		INNER JOIN `keep_included_hcpcs_output:23qf` AS t1 ON _source_.HCPCS_CD = t1.HCPCS_CPT_Code
) 
SELECT DISTINCT
	BENE_ID,
	CLM_ID
FROM
	`step_1`