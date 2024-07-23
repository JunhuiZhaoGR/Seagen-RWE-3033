CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_VISIT as
  select patient_id, max(visit_date) as last_visitdt
  from ODS.FIRN.VW_VISIT
  where period = '{year_month}' and source_name = 'MET_BREAST' 
  group by patient_id