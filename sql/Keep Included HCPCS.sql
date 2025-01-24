WITH step_1 AS (
	SELECT
		Year,
		HCPCS_CPT_Code
	FROM
		_source_
	WHERE
		-- Step 1 (rowFilter):
		(Included = 'yes')
) 
SELECT
	Year,
	HCPCS_CPT_Code
FROM
	`step_1`