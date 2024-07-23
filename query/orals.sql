
SELECT distinct
        patient_id
      , drug_name
      , start_date
      , end_date
      , date_granularity
FROM ODS.FIRN.VW_ENHANCED_MET_BREAST_ORALS
WHERE period = '{year_month}'
  and date_granularity in ('Day', 'Month')
  and drug_name not in ('fulvestrant', 'exemestane', 'anastrozole', 'megestrol', 'tamoxifen',
                        'letrozole', 'toremifene', 'elacestrant', 'leuprolide', 'goserelin',
			'triptorelin', 'medroxyprogesterone', 'megesterol', 'bicalutamide',
			'ethinyl estradiol', 'ganirelix', 'enzalutamide', 'darolutamide')

