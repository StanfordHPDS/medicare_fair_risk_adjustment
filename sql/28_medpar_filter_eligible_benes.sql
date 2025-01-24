WITH steps_1_3 AS (
	SELECT
		-- Step 2 (create variables):
		_source_.BENE_ID AS BENE_ID,
		_source_.ADMTG_DGNS_CD AS ADMTG_DGNS_CD,
		_source_.DGNS_1_CD AS DGNS_1_CD,
		_source_.DGNS_2_CD AS DGNS_2_CD,
		_source_.DGNS_3_CD AS DGNS_3_CD,
		_source_.DGNS_4_CD AS DGNS_4_CD,
		_source_.DGNS_5_CD AS DGNS_5_CD,
		_source_.DGNS_6_CD AS DGNS_6_CD,
		_source_.DGNS_7_CD AS DGNS_7_CD,
		_source_.DGNS_8_CD AS DGNS_8_CD,
		_source_.DGNS_9_CD AS DGNS_9_CD,
		_source_.DGNS_10_CD AS DGNS_10_CD,
		_source_.DGNS_11_CD AS DGNS_11_CD,
		_source_.DGNS_12_CD AS DGNS_12_CD,
		_source_.DGNS_13_CD AS DGNS_13_CD,
		_source_.DGNS_14_CD AS DGNS_14_CD,
		_source_.DGNS_15_CD AS DGNS_15_CD,
		_source_.DGNS_16_CD AS DGNS_16_CD,
		_source_.DGNS_17_CD AS DGNS_17_CD,
		_source_.DGNS_18_CD AS DGNS_18_CD,
		_source_.DGNS_19_CD AS DGNS_19_CD,
		_source_.DGNS_20_CD AS DGNS_20_CD,
		_source_.DGNS_21_CD AS DGNS_21_CD,
		_source_.DGNS_22_CD AS DGNS_22_CD,
		_source_.DGNS_23_CD AS DGNS_23_CD,
		_source_.DGNS_24_CD AS DGNS_24_CD,
		_source_.DGNS_25_CD AS DGNS_25_CD,
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
SELECT
	BENE_ID,
	ADMTG_DGNS_CD,
	DGNS_1_CD,
	DGNS_2_CD,
	DGNS_3_CD,
	DGNS_4_CD,
	DGNS_5_CD,
	DGNS_6_CD,
	DGNS_7_CD,
	DGNS_8_CD,
	DGNS_9_CD,
	DGNS_10_CD,
	DGNS_11_CD,
	DGNS_12_CD,
	DGNS_13_CD,
	DGNS_14_CD,
	DGNS_15_CD,
	DGNS_16_CD,
	DGNS_17_CD,
	DGNS_18_CD,
	DGNS_19_CD,
	DGNS_20_CD,
	DGNS_21_CD,
	DGNS_22_CD,
	DGNS_23_CD,
	DGNS_24_CD,
	DGNS_25_CD
FROM
	`steps_1_3`