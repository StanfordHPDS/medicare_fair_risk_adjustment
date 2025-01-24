WITH step_1 AS (
	SELECT
		BENE_ID,
		LINE_ICD_DGNS_CD
	FROM
		_source_
		-- Step 1 (aggregate)
	GROUP BY
		BENE_ID,
		LINE_ICD_DGNS_CD
),
step_2 AS (
	SELECT
		-- Step 2 (create variables):
		BENE_ID,
		LINE_ICD_DGNS_CD,
		NTILE(2) OVER (
			ORDER BY
				BENE_ID ASC
		) AS v_split
	FROM
		step_1
) 
SELECT
	BENE_ID,
	LINE_ICD_DGNS_CD,
	v_split AS split
FROM
	`step_2`