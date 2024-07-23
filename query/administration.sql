
SELECT distinct
        patient_id
      , order_id
      , administered_date
      , drug_name
      , common_drug_name
      , route
      , drug_category
      , detailed_drug_category
      , administered_amount
      , administered_units
FROM ODS.FIRN.VW_MEDICATION_ADMINISTRATION 
WHERE source_name = 'MET_BREAST' and period = '{year_month}'
  and drug_category = 'antineoplastic'
  and detailed_drug_category != 'hormone'

