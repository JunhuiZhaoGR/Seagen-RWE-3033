CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_01 AS (
  SELECT * 
  FROM ODS.FIRN.VW_ENHANCED_MET_BREAST
  WHERE period = '{year_month}'
)

