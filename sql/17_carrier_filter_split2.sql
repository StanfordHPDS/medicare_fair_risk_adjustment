WITH step_1 AS (
	SELECT
		BENE_ID,
		LINE_ICD_DGNS_CD
	FROM
		_source_
	WHERE
		-- Step 1 (rowFilter):
		(split = 2)
) 
SELECT
	BENE_ID,
	LINE_ICD_DGNS_CD
FROM
	`step_1`