


WITH 

    enhanced AS (
      SELECT patient_id
           , diagnosis_date
           , met_diagnosis_date
           , group_stage
      FROM ODS.FIRN.VW_ENHANCED_MET_BREAST
      WHERE period = '{year_month}'
  )
  
 , demographic AS (
      SELECT patient_id
           , birth_year
           , gender
           , race
           , ethnicity
           , state
      FROM ODS.FIRN.VW_DEMOGRAPHICS 
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
    )
    
  , practice_academic AS (
      SELECT distinct
              patient_id
            , 1 as flag_academic
      FROM ODS.FIRN.VW_PRACTICE
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
        and practice_type = 'ACADEMIC'
  )
  
  , practice_community AS (
      SELECT distinct
              patient_id
            , 1 as flag_community
      FROM ODS.FIRN.VW_PRACTICE
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
        and practice_type = 'COMMUNITY'
  )
    
  , practice AS (
      SELECT distinct
              enh.patient_id
            , CASE
                when flag_academic = 1 and flag_community = 1 then 'Both'
                when flag_academic = 1 and flag_community is null then 'Academic'
                when flag_academic is null and flag_community = 1 then 'Community'
                when flag_academic is null and flag_community is null then 'Missing'
              END as practice_type
      FROM enhanced enh
      LEFT JOIN practice_academic academic
        ON enh.patient_id = academic.patient_id
      LEFT JOIN practice_community community
        ON enh.patient_id = community.patient_id
  )
    
  , last_visit AS (
      SELECT patient_id
           , max(visit_date) as last_dt
      FROM ODS.FIRN.VW_VISIT
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
      GROUP BY patient_id
  )
  
  , last_admin AS (
      SELECT patient_id
           , max(administered_date) as last_admin_dt
      FROM ODS.FIRN.VW_MEDICATION_ADMINISTRATION
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
      GROUP BY patient_id
  )
  
  , visit_90d AS (
      SELECT distinct
              visit.patient_id
            , min(visit_date) as first_visit_dt_90d
      FROM ODS.FIRN.VW_VISIT visit
      INNER JOIN enhanced enh
        ON visit.patient_id = enh.patient_id
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
        and (visit_date - met_diagnosis_date between 0 and 90)
      GROUP BY visit.patient_id
  )
  
  , admin_90d AS (
      SELECT distinct
              admin.patient_id
            , min(administered_date) as first_admin_dt_90d
      FROM ODS.FIRN.VW_MEDICATION_ADMINISTRATION admin
      INNER JOIN enhanced enh
        ON admin.patient_id = enh.patient_id
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
        and (administered_date - met_diagnosis_date between 0 and 90)
      GROUP BY admin.patient_id
  )
  
  , mortality AS (
      SELECT patient_id
           , CASE WHEN date_of_death like '____-__' THEN add_months(date(date_of_death||'-1'),1)-1
            ELSE DATE(date_of_death || '-07-01')
            END AS death_date
      FROM ODS.FIRN.VW_ENHANCED_MORTALITY_V2
      WHERE source_name = 'MET_BREAST' and period = '{year_month}'
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


SELECT DISTINCT a.patient_id, enh.met_diagnosis_date as index_date
     , enh.diagnosis_date
     , enh.met_diagnosis_date
     , enh.group_stage
     , demo.birth_year
     , demo.gender
     , demo.race
     , demo.ethnicity
     , demo.state
     , practice.practice_type
     , admin.last_admin_dt
     , visit_90d.first_visit_dt_90d
     , admin_90d.first_admin_dt_90d
     , mo.death_date
     , acticity.last_activity_dt
FROM  GENESIS_TEMP.PUBLIC.JZ_RWE3033_02 as a
LEFT JOIN enhanced as enh
  ON a.patient_id = enh.patient_id
LEFT JOIN demographic as demo
  ON a.patient_id = demo.patient_id
LEFT JOIN practice 
  ON a.patient_id = practice.patient_id
LEFT JOIN last_admin as admin
  ON a.patient_id = admin.patient_id
LEFT JOIN visit_90d
  ON a.patient_id = visit_90d.patient_id
LEFT JOIN admin_90d
  ON a.patient_id = admin_90d.patient_id
LEFT JOIN mortality as mo
  ON a.patient_id = mo.patient_id
LEFT JOIN pts_last_activity as acticity
  ON a.patient_id = acticity.patient_id

    
