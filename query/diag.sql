CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_diag AS (
  select distinct a.*
  from ODS.FIRN.VW_DIAGNOSIS as a 
  inner join GENESIS_TEMP.PUBLIC.JZ_RWE3033_HER2DRUG as b
  on a.patient_id = b.patient_id and diagnosis_date <= index_date 
  WHERE  source_name = 'MET_BREAST' and period = '{year_month}' 

)