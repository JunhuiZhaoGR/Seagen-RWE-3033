


SELECT DISTINCT
		    patient_id
		  , ecog_date
		  , ecog_value
FROM ODS.FIRN.VW_ECOG
WHERE source_name = 'MET_BREAST' and period = '{year_month}'