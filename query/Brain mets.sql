CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_BrainMets AS (
  select a.*,(b.patient_id is not null) as brain_mets
  from GENESIS_TEMP.PUBLIC.JZ_RWE3033_HR as a
  left join ODS.FIRN.VW_ENHANCED_MET_BREAST_SITES as b
  ON a.patient_id = b.patient_id 
      and b.source_name = 'MET_BREAST' 
      and b.period = '{year_month}'
      and date(b.DATE_OF_METASTASIS||'-01') <= index_date 
      and b.site_of_metastasis = 'Brain'
  )