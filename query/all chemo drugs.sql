CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_chemo2 AS (

  SELECT patient_id, start_date, end_date, drug_name 
  FROM GENESIS_TEMP.PUBLIC.JZ_RWE3033_TREATMENTS 
  WHERE drug_name like any ('%capecitabine%','%cyclophosphamide%','%carboplatin%','%docetaxel%',
                       '%paclitaxel%','%eribulin%','%gemcitabine%','%doxorubicin%','%vinorelbine%',
                       '%ixabepilone%','%cisplatin%','%oxaliplatin%','%topotecan%','%irinotecan%',
                       '%etoposide%','%pemetrexed%','%azacitidine%','%cladribine%','%cytarabine%',
                       '%decitabine%','%methotrexate%','%trifluridine/tipiracil%','%fluorouracil%',
                       '%vincristine%','%epirubicin%','%mitoxantrone%','%mitomycin%','%temozolomide%',
                       '%lurbinectedin%','%tazemetostat%','%thiotepa%','%ifosfamide%')
)