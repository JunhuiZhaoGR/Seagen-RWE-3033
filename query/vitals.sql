SELECT DISTINCT
		    patient_id
		  , test_date
		  , lab_component
		  , test_units_cleaned
		  , test_result_cleaned
FROM ODS.FIRN.VW_VITALS
WHERE source_name = 'MET_BREAST' and period = '{year_month}'
  and lab_component in ('Body Height', 'Body Weight')
  and test_result_cleaned is not null