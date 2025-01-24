WITH steps_1_2 AS (
	SELECT
		-- Step 1 (create variables):
		BENE_ID,
		BENE_ENROLLMT_REF_YR,
		SUM((MDCR_PMT_AMT + PASS_THRU_AMT)) AS v_tot_medpar_pmt
	FROM
		_source_
		-- Step 2 (aggregate)
	GROUP BY
		BENE_ID,
		BENE_ENROLLMT_REF_YR
),
step_3 AS (
	SELECT
		_source_.BENE_ID AS BENE_ID,
		_source_.BENE_ENROLLMT_REF_YR AS BENE_ENROLLMT_REF_YR,
		v_tot_medpar_pmt
	FROM
		steps_1_2 AS _source_
		-- Step 3 (join):
		INNER JOIN `demo:1mds` AS t1 ON _source_.BENE_ID = t1.BENE_ID
		AND _source_.BENE_ENROLLMT_REF_YR = t1.BENE_ENROLLMT_REF_YR
) 
SELECT DISTINCT
	BENE_ID,
	BENE_ENROLLMT_REF_YR,
	v_tot_medpar_pmt AS tot_medpar_pmt
FROM
	`step_3`