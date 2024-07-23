SELECT distinct
        patient_id
      , date_of_metastasis
      , site_of_metastasis
FROM ODS.FIRN.VW_ENHANCED_MET_BREAST_SITES
WHERE period = '{year_month}'