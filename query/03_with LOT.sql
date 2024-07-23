CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_03 AS (
  SELECT distinct a.*, b.line_start_date as index_date, b.line_end_date as end_date_1L
  FROM GENESIS_TEMP.PUBLIC.JZ_RWE3033_02 AS a
  INNER JOIN GENESIS_TEMP.PUBLIC.JZ_RWE3033_LOT as b
  ON a.patient_id = b.patient_id 
    and b.line_number = 1
)