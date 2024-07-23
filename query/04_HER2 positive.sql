CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_04 AS (

  with HER2 AS (
    SELECT patient_id
      , coalesce(resultdate, specimen_received_date, SPECIMEN_COLLECTED_DATE) as biomarker_date
      , BIOMARKER_STATUS
      , case 
        when lower(biomarker_status) like '%positive%' then 'Positive'
        when lower(biomarker_status) like '%negative%' then 'Negative'
        when lower(biomarker_status) like '%equivocal%' then 'Equivocal'
        else 'Unknown' end as BIOMARKER_STATUS_TYPE
    FROM ODS.FIRN.VW_ENHANCED_MET_BREAST_BIOMARKERS
    WHERE biomarker_name = 'HER2' and period = '{year_month}'
  )
  ,
  HER2_pts AS (
    SELECT DISTINCT patient_id, biomarker_date
      , max(BIOMARKER_STATUS_TYPE = 'Positive') as HER2_Positive
    FROM HER2
    GROUP BY patient_id, biomarker_date
  )
  ,
  
  HER2_pts2 as (SELECT distinct a.patient_id, index_date, end_date_1L, met_diagnosis_date, biomarker_date, HER2_Positive
    , max(biomarker_date) OVER (PARTITION BY a.patient_id) as max_biomarker_date
  FROM GENESIS_TEMP.PUBLIC.JZ_RWE3033_03 AS a
  LEFT JOIN HER2_pts as b
  ON a.patient_id = b.patient_id 
    and b.biomarker_date <= index_date+28)
    
  SELECT patient_id, index_date, end_date_1L, met_diagnosis_date
  FROM HER2_pts2
  WHERE max_biomarker_date = biomarker_date and HER2_Positive = 1   

)