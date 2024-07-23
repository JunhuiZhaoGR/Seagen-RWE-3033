CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_chemo AS (
  select distinct a.*
    , max(c.end_date) over (partition by a.patient_id) as last_chemo_date
  from GENESIS_TEMP.PUBLIC.JZ_RWE3033_BrainMets as a
  left join GENESIS_TEMP.PUBLIC.JZ_RWE3033_LOT as b
  on a.patient_id = b.patient_id and b.line_number = 1
  left join GENESIS_TEMP.PUBLIC.JZ_RWE3033_TREATMENTS as c
  on a.patient_id = c.patient_id 
    and c.end_date >= b.line_start_date and c.end_date <= b.line_end_date
    and c.drug_name like any ('%capecitabine%','%cyclophosphamide%','%carboplatin%','%docetaxel%',
                       '%paclitaxel%','%eribulin%','%gemcitabine%','%doxorubicin%','%vinorelbine%',
                       '%ixabepilone%','%cisplatin%','%oxaliplatin%','%topotecan%','%irinotecan%',
                       '%etoposide%','%pemetrexed%','%azacitidine%','%cladribine%','%cytarabine%',
                       '%decitabine%','%methotrexate%','%trifluridine/tipiracil%','%fluorouracil%',
                       '%vincristine%','%epirubicin%','%mitoxantrone%','%mitomycin%','%temozolomide%',
                       '%lurbinectedin%','%tazemetostat%','%thiotepa%','%ifosfamide%')
)