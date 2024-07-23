	CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_07 AS (
	select a.*
	from GENESIS_TEMP.PUBLIC.JZ_RWE3033_06 as a
	inner join ODS.FIRN.VW_DEMOGRAPHICS as b
	on a.patient_id = b.patient_id 
	where b.period = '{year_month}'
    and b.source_name = 'MET_BREAST' 
    and b.GENDER =  'F'
	);