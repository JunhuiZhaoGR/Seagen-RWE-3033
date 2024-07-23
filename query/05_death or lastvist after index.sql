CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_05 AS (

WITH deathdate as (
  select patient_id, DATE_OF_DEATH
    , case when date_of_death like '____-__' then add_months(date(date_of_death||'-1'),1)-1
           else date(date_of_death||'-07-01')
      end as dateofdeath
  from ODS.FIRN.VW_ENHANCED_MORTALITY_V2
  where period = '{year_month}' and source_name = 'MET_BREAST' 
)
, last_visit as (
  select patient_id, max(visit_date) as last_dt
  from ODS.FIRN.VW_VISIT
  where period = '{year_month}' and source_name = 'MET_BREAST' 
  group by patient_id
)
, last_med_order as(
    select patient_id
           , max(ordered_date) as last_dt
    from ODS.FIRN.VW_MEDICATION_ORDER
    WHERE source_name = 'MET_BREAST' and period = '{year_month}'
    and is_canceled = FALSE
    GROUP BY patient_id
    )
  , pts_orals AS (
   SELECT patient_id
           , greatest(start_date,end_date) as larger_dt
      FROM ODS.FIRN.VW_ENHANCED_MET_BREAST_ORALS
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
  )
  , pts_oral_last_dt as (
  select patient_id
        , max(larger_dt) as last_dt
        from pts_orals
        group by patient_id
  )
  ,last_drug_episo as (
  SELECT patient_id
           , max(episode_date) as last_dt
      FROM ODS.FIRN.VW_DRUG_EPISODE
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
      GROUP BY patient_id
  )
  , pts_activity as (
  select *
  from last_visit
  union
  select *
  from last_med_order
  union
  select *
  from pts_oral_last_dt
  union
  select *
  from last_drug_episo
  )
  ,pts_last_activity as (
  select patient_id,
          max(last_dt) as last_activity_dt
  from pts_activity
  group by patient_id
  )
  SELECT a.patient_id, index_date, last_activity_dt, end_date_1L, met_diagnosis_date
  FROM GENESIS_TEMP.PUBLIC.JZ_RWE3033_04 as a
  LEFT JOIN deathdate as b
  on a.patient_id = b.patient_id 
  LEFT JOIN pts_last_activity as c
  on a.patient_id = c.patient_id 
  where dateofdeath > index_date or last_activity_dt > index_date
  
)  
  