SELECT DISTINCT
		    patient_id
		  , payer_category
		  , start_date
		  , end_date
FROM ODS.FIRN.VW_INSURANCE
WHERE source_name = 'MET_BREAST' and period = '{year_month}'

