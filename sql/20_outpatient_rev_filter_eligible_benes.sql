WITH steps_1_4 AS (
	SELECT
		-- Step 1 (create variables):
		-- Step 3 (create variables):
		_source_.BENE_ID AS BENE_ID,
		_source_.CLM_ID AS CLM_ID,
		_source_.CLM_THRU_DT AS CLM_THRU_DT,
		_source_.CLM_LINE_NUM AS CLM_LINE_NUM,
		_source_.HCPCS_CD AS HCPCS_CD,
		EXTRACT(
			YEAR
			FROM
				_source_.CLM_THRU_DT
		) AS v_BENE_ENROLLMT_REF_YR,
		MIN(
			EXTRACT(
				YEAR
				FROM
					_source_.CLM_THRU_DT
			)
		) OVER () AS v_min_year
	FROM
		_source_
		-- Step 2 (join):
		INNER JOIN `demo:1mds` AS t1 ON _source_.BENE_ID = t1.BENE_ID
		AND EXTRACT(
			YEAR
			FROM
				_source_.CLM_THRU_DT
		) = t1.BENE_ENROLLMT_REF_YR
	QUALIFY
		-- Step 4 (rowFilter):
		(v_BENE_ENROLLMT_REF_YR = v_min_year)
) 
SELECT DISTINCT
	BENE_ID,
	CLM_ID,
	CLM_THRU_DT,
	CLM_LINE_NUM,
	HCPCS_CD
FROM
	`steps_1_4`