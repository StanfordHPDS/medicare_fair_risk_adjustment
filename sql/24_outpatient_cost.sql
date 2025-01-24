WITH step_1 AS (
	SELECT
		BENE_ID,
		BENE_ENROLLMT_REF_YR,
		SUM(CLM_PMT_AMT) AS v_tot_outpatient_pmt
	FROM
		_source_
		-- Step 1 (aggregate)
	GROUP BY
		BENE_ID,
		BENE_ENROLLMT_REF_YR
),
step_2 AS (
	SELECT
		_source_.BENE_ID AS BENE_ID,
		_source_.BENE_ENROLLMT_REF_YR AS BENE_ENROLLMT_REF_YR,
		v_tot_outpatient_pmt
	FROM
		step_1 AS _source_
		-- Step 2 (join):
		INNER JOIN `demo:1mds` AS t1 ON _source_.BENE_ID = t1.BENE_ID
		AND _source_.BENE_ENROLLMT_REF_YR = t1.BENE_ENROLLMT_REF_YR
) 
SELECT
	BENE_ID,
	BENE_ENROLLMT_REF_YR,
	v_tot_outpatient_pmt AS tot_outpatient_pmt
FROM
	`step_2`