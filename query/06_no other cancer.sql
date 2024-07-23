	CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_06 AS (
	
	with 
    diagnosis_table as (
    select patient_id, diagnosis_date, diagnosis_code
    from ODS.FIRN.VW_DIAGNOSIS 
    where source_name = 'MET_BREAST' 
      and period = '{year_month}'
      and diagnosis_code like any ({other_cancer_dx})
    )

	
	
	select a.*
	from GENESIS_TEMP.PUBLIC.JZ_RWE3033_05 as a
	left join diagnosis_table as b
	on a.patient_id = b.patient_id 
	    and b.DIAGNOSIS_DATE between index_date-183 and index_date-1
	having b.patient_id is null
	);