CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_endocrine AS (

With endocrine as (
  SELECT distinct
          patient_id
        , start_date
        , end_date
  FROM ODS.FIRN.VW_ENHANCED_MET_BREAST_ORALS
  WHERE period = '{year_month}'
    and date_granularity in ('Day', 'Month')
    and drug_name in ('fulvestrant', 'exemestane', 'anastrozole', 'megestrol', 'tamoxifen',
                          'letrozole', 'toremifene', 'elacestrant', 'leuprolide', 'goserelin',
  			'triptorelin', 'medroxyprogesterone', 'megesterol', 'bicalutamide',
  			'ethinyl estradiol', 'ganirelix', 'enzalutamide', 'darolutamide')

union 

SELECT distinct
        patient_id
      , administered_date as start_date 
      , administered_date as end_date

FROM ODS.FIRN.VW_MEDICATION_ADMINISTRATION 
WHERE source_name = 'MET_BREAST' and period = '{year_month}'
  and drug_category = 'antineoplastic'
  and detailed_drug_category = 'hormone')


  select distinct a.*,(b.patient_id is not null) as endocrine
  from GENESIS_TEMP.PUBLIC.JZ_RWE3033_LOT as a
  left join endocrine as b
  ON a.patient_id = b.patient_id 
    and b.start_date >= line_start_date and b.start_date <= line_end_date

  )