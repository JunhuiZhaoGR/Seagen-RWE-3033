CREATE OR REPLACE TABLE GENESIS_TEMP.PUBLIC.JZ_RWE3033_HR AS (
WITH 

HR AS (
		SELECT DISTINCT
			patient_id
			, specimen_collected_date
			, resultdate
			, COALESCE(resultdate, specimen_collected_date) as biomarker_date
			, biomarker_name
			, biomarker_status
			, CASE
				WHEN lower(biomarker_status) LIKE '%positive%' THEN 'Positive'
				WHEN lower(biomarker_status) LIKE '%negative%' THEN 'Negative'
				WHEN lower(biomarker_status) LIKE '%equivocal%' THEN 'Equivocal'
				ELSE 'Unknown'
			END AS BIOMARKER_STATUS_TYPE
		FROM ODS.FIRN.VW_ENHANCED_MET_BREAST_BIOMARKERS
		WHERE period = '{year_month}'
		  and biomarker_name in ('ER', 'PR')
	)
, HR_pts AS (
    SELECT DISTINCT patient_id, biomarker_date
      , max(BIOMARKER_STATUS_TYPE = 'Positive') as HR_Positive
      , min(BIOMARKER_STATUS_TYPE = 'Negative') as HR_Negative
    FROM HR
    GROUP BY patient_id, biomarker_date
  )	
	
, HR_pts_90days as (
    SELECT distinct
        bio.*
        , abs(bio.biomarker_date - index_date) as absdiff
    FROM HR_pts as bio
    INNER JOIN GENESIS_TEMP.PUBLIC.JZ_RWE3033_07 as enh
    ON bio.patient_id = enh.patient_id
      and bio.biomarker_date between index_date- 90 and index_date+ 90)
      
, minabsdiff as (
    select distinct patient_id
      , min(absdiff)  over (partition by patient_id) as minabsdiff
    from HR_pts_90days)

, HR_status as  (
    select a.*
    , case when HR_Positive = True then 'Positive'
           when HR_Negative = True then 'Negative'
           else 'Unknown'
      end as HR_status
    from HR_pts_90days as a
    inner join minabsdiff as b
    on a.patient_id = b.patient_id and a.absdiff = minabsdiff)
, HR_status2 as (
    select distinct a.*
	, MAX(b.BIOMARKER_STATUS_TYPE='Positive') over (partition by a.patient_id) as ER_positive
	, MAX(b.BIOMARKER_STATUS_TYPE='Negative') over (partition by a.patient_id) as ER_negative
	, MAX(b.BIOMARKER_STATUS_TYPE not in ('Positive', 'Negative')) over (partition by a.patient_id) as ER_unknown
	, MAX(c.BIOMARKER_STATUS_TYPE='Positive') over (partition by a.patient_id) as PR_positive
	, MAX(c.BIOMARKER_STATUS_TYPE='Negative') over (partition by a.patient_id) as PR_negative
	, MAX(c.BIOMARKER_STATUS_TYPE not in ('Positive', 'Negative')) over (partition by a.patient_id) as PR_unknown
	from HR_status as a
	left join HR as b
	on a.patient_id = b.patient_id and a.biomarker_date = b.biomarker_date and b.biomarker_name in ('ER') 
	left join HR as c
	on a.patient_id = c.patient_id and a.biomarker_date = c.biomarker_date and c.biomarker_name in ('PR') 	
	)
    
  select distinct a.*, b.hr_status
    , case when ER_positive = True then 'Positive'
           when ER_Negative = True then 'Negative'
           else 'Unknown'
      end as ER_status
    , case when PR_positive = True then 'Positive'
           when PR_Negative = True then 'Negative'
           else 'Unknown'
      end as PR_status
  from GENESIS_TEMP.PUBLIC.JZ_RWE3033_07 as a
  left join HR_status2 as b
  on a.patient_id = b.patient_id
)

