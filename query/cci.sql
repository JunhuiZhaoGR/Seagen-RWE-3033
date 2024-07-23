

WITH 

cci_claims AS (

	SELECT * FROM (
	
		SELECT DISTINCT
			dx_tbl.patient_id,
			CASE
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('I21%', 'I22%', 'I252%', '410%', '412%') THEN 'Myocardial Infarction'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('I099%', 'I110%', 'I130%', 'I132%', 'I255%', 'I420%', 'I425%', 'I426%', 'I427%', 'I428%', 'I429%', 'I43%', 'I50%', 'P290%', '39891%', '40201%', '40211%', '40291%', '40401%', '40403%', '40411%', '40413%', '40491%', '40493%', '4254%', '4255%', '4256%', '4257%', '4258%', '4259%', '428%') THEN 'Congestive Heart Failure'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('I70%', 'I71%', 'I731%', 'I738%', 'I739%', 'I771%', 'I790%', 'I792%', 'K551%', 'K558%', 'K559%', 'Z958%', 'Z959%', '0930%', '4373%', '440%', '441%', '4431%', '4432%', '4433%', '4434%', '4435%', '4436%', '4437%', '4438%', '4439%', '4471%', '5571%', '5579%', 'V434%') THEN 'Peripheral Vascular Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('G45%', 'G46%', 'H340%', 'I60%', 'I61%', 'I62%', 'I63%', 'I64%', 'I65%', 'I66%', 'I67%', 'I68%', 'I69%', '36234%', '430%', '431%', '432%', '433%', '434%', '435%', '436%', '437%', '438%') THEN 'Cerebrovascular Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('F00%', 'F01%', 'F02%', 'F03%', 'F051%', 'G30%', 'G311%', '290%', '2941%', '3312%') THEN 'Dementia'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('I278%', 'I279%', 'J40%', 'J41%', 'J42%', 'J43%', 'J44%', 'J45%', 'J46%', 'J47%', 'J60%', 'J61%', 'J62%', 'J63%', 'J64%', 'J65%', 'J66%', 'J67%', 'J684%', 'J701%', 'J703%', '4168%', '4169%', '490%', '491%', '492%', '493%', '494%', '495%', '496%', '497%', '498%', '499%', '500%', '501%', '502%', '503%', '504%', '505%', '5064%', '5081%', '5088%') THEN 'Chronic Pulmonary Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('M05%', 'M06%', 'M315%', 'M32%', 'M33%', 'M34%', 'M351%', 'M353%', 'M360%', '4465%', '7100%', '7101%', '7102%', '7103%', '7104%', '7140%', '7141%', '7142%', '7148%', '725%') THEN 'Rheumatic Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('K25%', 'K26%', 'K27%', 'K28%', '531%', '532%', '533%', '534%') THEN 'Peptic Ulcer Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('B18%', 'K700%', 'K701%', 'K702%', 'K703%', 'K709%', 'K713%', 'K714%', 'K715%', 'K717%', 'K73%', 'K74%', 'K760%', 'K762%', 'K763%', 'K764%', 'K768%', 'K769%', 'Z944%', '07022%', '07023%', '07032%', '07033%', '07044%', '07054%', '0706%', '0709%', '570%', '571%', '5733%', '5734%', '5738%', '5739%', 'V427%') THEN 'Mild Liver Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('E100%', 'E101%', 'E106%', 'E108%', 'E109%', 'E110%', 'E111%', 'E116%', 'E118%', 'E119%', 'E120%', 'E121%', 'E126%', 'E128%', 'E129%', 'E130%', 'E131%', 'E136%', 'E138%', 'E139%', 'E140%', 'E141%', 'E146%', 'E148%', 'E149%', '2500%', '2501%', '2502%', '2503%', '2508%', '2509%') THEN 'Diabetes Without Chronic Complication'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('E102%', 'E103%', 'E104%', 'E105%', 'E107%', 'E112%', 'E113%', 'E114%', 'E115%', 'E117%', 'E122%', 'E123%', 'E124%', 'E125%', 'E127%', 'E132%', 'E133%', 'E134%', 'E135%', 'E137%', 'E142%', 'E143%', 'E144%', 'E145%', 'E147%', '2504%', '2505%', '2506%', '2507%') THEN 'Diabetes With Chronic Complication'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('G041%', 'G114%', 'G801%', 'G802%', 'G81%', 'G82%', 'G830%', 'G831%', 'G832%', 'G833%', 'G834%', 'G839%', '3341%', '342%', '343%', '3440%', '3441%', '3442%', '3443%', '3444%', '3445%', '3446%', '3449%') THEN 'Hemiplegia or Paraplegia'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('I120%', 'I131%', 'N032%', 'N033%', 'N034%', 'N035%', 'N036%', 'N037%', 'N052%', 'N053%', 'N054%', 'N055%', 'N056%', 'N057%', 'N18%', 'N19%', 'N250%', 'Z490%', 'Z491%', 'Z492%', 'Z940%', 'Z992%', '40301%', '40311%', '40391%', '40402%', '40403%', '40412%', '40413%', '40492%', '40493%', '582%', '5830%', '5831%', '5832%', '5833%', '5834%', '5835%', '5836%', '5837%', '585%', '586%', '5880%', 'V420%', 'V451%', 'V56%') THEN 'Renal Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('C00%', 'C01%', 'C02%', 'C03%', 'C04%', 'C05%', 'C06%', 'C07%', 'C08%', 'C09%', 'C10%', 'C11%', 'C12%', 'C13%', 'C14%', 'C15%', 'C16%', 'C17%', 'C18%', 'C19%', 'C20%', 'C21%', 'C22%', 'C23%', 'C24%', 'C25%', 'C26%', 'C30%', 'C31%', 'C32%', 'C33%', 'C34%', 'C37%', 'C38%', 'C39%', 'C40%', 'C41%', 'C43%', 'C45%', 'C46%', 'C47%', 'C48%', 'C49%', 'C50%', 'C51%', 'C52%', 'C53%', 'C54%', 'C55%', 'C56%', 'C57%', 'C58%', 'C60%', 'C61%', 'C62%', 'C63%', 'C64%', 'C65%', 'C66%', 'C67%', 'C68%', 'C69%', 'C70%', 'C71%', 'C72%', 'C73%', 'C74%', 'C75%', 'C76%', 'C81%', 'C82%', 'C83%', 'C84%', 'C85%', 'C88%', 'C90%', 'C91%', 'C92%', 'C93%', 'C94%', 'C95%', 'C96%', 'C97%', '140%', '141%', '142%', '143%', '144%', '145%', '146%', '147%', '148%', '149%', '150%', '151%', '152%', '153%', '154%', '155%', '156%', '157%', '158%', '159%', '160%', '161%', '162%', '163%', '164%', '165%', '166%', '167%', '168%', '169%', '170%', '171%', '172%', '174%', '175%', '176%', '177%', '178%', '179%', '180%', '181%', '182%', '183%', '184%', '185%', '186%', '187%', '188%', '189%', '190%', '191%', '192%', '193%', '194%', '1950%', '1951%', '1952%', '1953%', '1954%', '1955%', '1956%', '1957%', '1958%', '200%', '201%', '202%', '203%', '204%', '205%', '206%', '207%', '208%', '2386%') THEN 'Malignancy'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('I850%', 'I859%', 'I864%', 'I982%', 'K704%', 'K711%', 'K721%', 'K729%', 'K765%', 'K766%', 'K767%', '4560%', '4561%', '4562%', '5722%', '5723%', '5724%', '5725%', '5726%', '5727%', '5728') THEN 'Liver Disease'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('C77%', 'C78%', 'C79%', 'C80%', '196%', '197%', '198%', '199%') THEN 'Metastatic Solid Tumor'
				WHEN REPLACE(diagnosis_code,'.','') LIKE ANY ('B20%', 'B21%', 'B22%', 'B24%', '042%', '043%', '044%') THEN 'AIDS'
			END AS comorbidity
		FROM ODS.FIRN.VW_DIAGNOSIS as dx_tbl
		INNER JOIN (
				SELECT
					patient_id,
					index_date
				FROM GENESIS_TEMP.PUBLIC.JZ_RWE3033_HER2DRUG as pts
				) pts
		ON dx_tbl.patient_id = pts.patient_id
		WHERE
		  source_name = 'MET_BREAST' and period = '{year_month}' and
			dx_tbl.diagnosis_date between pts.index_date - 180  and  pts.index_date 
	) subq
	WHERE
		comorbidity IS NOT NULL
)

, cci_flags AS (
	      SELECT DISTINCT
	              patient_id,
	              MAX(CASE WHEN comorbidity = 'Myocardial Infarction' THEN 1 ELSE 0 END) AS has_mi,
	              MAX(CASE WHEN comorbidity = 'Congestive Heart Failure' THEN 1 ELSE 0 END) AS has_chf,
	              MAX(CASE WHEN comorbidity = 'Peripheral Vascular Disease' THEN 1 ELSE 0 END) AS has_pvd,
	              MAX(CASE WHEN comorbidity = 'Cerebrovascular Disease' THEN 1 ELSE 0 END) AS has_cerebro_disease,
	              MAX(CASE WHEN comorbidity = 'Dementia' THEN 1 ELSE 0 END) AS has_dementia,
	              MAX(CASE WHEN comorbidity = 'Chronic Pulmonary Disease' THEN 1 ELSE 0 END) AS has_cpd,
	              MAX(CASE WHEN comorbidity = 'Rheumatic Disease' THEN 1 ELSE 0 END) AS has_rheumatic_disease,
	              MAX(CASE WHEN comorbidity = 'Peptic Ulcer Disease' THEN 1 ELSE 0 END) AS has_pep_ulcer_disease,
	              MAX(CASE WHEN comorbidity = 'Mild Liver Disease' THEN 1 ELSE 0 END) AS has_mild_liver,
	              MAX(CASE WHEN comorbidity = 'Diabetes Without Chronic Complication' THEN 1 ELSE 0 END) AS has_diab_without,
	              MAX(CASE WHEN comorbidity = 'Diabetes With Chronic Complication' THEN 1 ELSE 0 END) AS has_diab_with,
	              MAX(CASE WHEN comorbidity = 'Hemiplegia or Paraplegia' THEN 1 ELSE 0 END) AS has_hemiplegia,
	              MAX(CASE WHEN comorbidity = 'Renal Disease' THEN 1 ELSE 0 END) AS has_renal_disease,
	              MAX(CASE WHEN comorbidity = 'Malignancy' THEN 1 ELSE 0 END) AS has_malignancy,
	              MAX(CASE WHEN comorbidity = 'Liver Disease' THEN 1 ELSE 0 END) AS has_liver_disease,
	              MAX(CASE WHEN comorbidity = 'Metastatic Solid Tumor' THEN 1 ELSE 0 END) AS has_met_solid_tumor,
	              MAX(CASE WHEN comorbidity = 'AIDS' THEN 1 ELSE 0 END) AS has_aids
	       FROM cci_claims
	       GROUP BY 1
	   )

, qci_score AS (
	      SELECT
	          patient_id,
	          SUM(
	            CASE
	              WHEN comorbidity = 'Myocardial Infarction' THEN 0
	              WHEN comorbidity = 'Congestive Heart Failure' THEN 2
	              WHEN comorbidity = 'Peripheral Vascular Disease' THEN 0
	              WHEN comorbidity = 'Cerebrovascular Disease' THEN 0
	              WHEN comorbidity = 'Dementia' THEN 2
	              WHEN comorbidity = 'Chronic Pulmonary Disease' THEN 1
	              WHEN comorbidity = 'Rheumatic Disease' THEN 1
	              WHEN comorbidity = 'Peptic Ulcer Disease' THEN 0
	              WHEN comorbidity = 'Mild Liver Disease' THEN 2
	              WHEN comorbidity = 'Liver Disease' THEN 4
	              WHEN comorbidity = 'Hemiplegia or Paraplegia' THEN 2
	              WHEN comorbidity = 'Renal Disease' THEN 1
	              WHEN comorbidity = 'Diabetes Without Chronic Complication' THEN 0
	              WHEN comorbidity = 'Diabetes With Chronic Complication' THEN 1
	              WHEN comorbidity = 'Malignancy' THEN 2
	              WHEN comorbidity = 'Metastatic Solid Tumor' THEN 6
	              WHEN comorbidity = 'AIDS' THEN 4
	           END
	         ) AS QCI_Score
	     FROM cci_claims
	     GROUP BY 1
	 )
	 
, cci_score AS (
	SELECT
		patient_id,
		SUM(
			CASE
				WHEN comorbidity = 'Myocardial Infarction' THEN 1
				WHEN comorbidity = 'Congestive Heart Failure' THEN 1
				WHEN comorbidity = 'Peripheral Vascular Disease' THEN 1
				WHEN comorbidity = 'Cerebrovascular Disease' THEN 1
				WHEN comorbidity = 'Dementia' THEN 1
				WHEN comorbidity = 'Chronic Pulmonary Disease' THEN 1
				WHEN comorbidity = 'Rheumatic Disease' THEN 1
				WHEN comorbidity = 'Peptic Ulcer Disease' THEN 1
				WHEN comorbidity = 'Mild Liver Disease' THEN 1
				WHEN comorbidity = 'Liver Disease' THEN 3
				WHEN comorbidity = 'Hemiplegia or Paraplegia' THEN 2
				WHEN comorbidity = 'Renal Disease' THEN 2
				WHEN comorbidity = 'Diabetes Without Chronic Complication' THEN 1
				WHEN comorbidity = 'Diabetes With Chronic Complication' THEN 2
				WHEN comorbidity = 'Malignancy' THEN 2
				WHEN comorbidity = 'Metastatic Solid Tumor' THEN 6
				WHEN comorbidity = 'AIDS' THEN 6
			END
		) AS CCI_Score
	FROM cci_claims
	GROUP BY 1
)


SELECT distinct
	    cci_score.patient_id,
	    cci_score.CCI_Score,
	    qci_score.QCI_Score,
	    cci_flags.has_mi,
	    cci_flags.has_chf,
	    cci_flags.has_pvd,
	    cci_flags.has_cerebro_disease,
	    cci_flags.has_dementia,
	    cci_flags.has_cpd,
	    cci_flags.has_rheumatic_disease,
	    cci_flags.has_pep_ulcer_disease,
	    cci_flags.has_mild_liver,
	    cci_flags.has_liver_disease,
	    cci_flags.has_hemiplegia,
	    cci_flags.has_renal_disease,
	    cci_flags.has_diab_without,
	    cci_flags.has_diab_with,
	    cci_flags.has_malignancy,
	    cci_flags.has_met_solid_tumor,
	    cci_flags.has_aids
	 FROM cci_score
	 LEFT JOIN qci_score
	    ON cci_score.patient_id = qci_score.patient_id
	 LEFT JOIN cci_flags
	    ON cci_score.patient_id = cci_flags.patient_id