####################################################################################################################################
# Title:        Charlson Comorbidity Index Template                                                                                #
# Last updated: 2023-11-10                                                                                                         #
# Github:       https://github.com/GR-Analytics/GR-Macros/blob/master/code_samples/charlson_glasheen_2019.R                        #
####################################################################################################################################

options(useFancyQuotes = FALSE)

cc_codelist = codelist_load(filepath="~/GR-Macros/code_samples/CCI - Glasheen 2019.xlsx", sheets="Codes")
cc_names_long = c("Myocardial Infarction","Congestive Heart Failure","Peripheral Vascular Disease","Cerebrovascular Disease","Dementia","Chronic Pulmonary Disease","Rheumatic Disease","Peptic Ulcer Disease","Mild Liver Disease","Diabetes without chronic complication","Renal (Mild or Moderate)","Diabetes with Chronic Complications","Hemiplegia or Paraplegia","Any malignancy (except skin)","Moderate or Severe Liver Disease","Renal (Severe)","HIV Infection","Metastatic Solid Tumor","AIDS")
cc_names_short = c("cc_mi","cc_chf","cc_pvd","cc_cvd","cc_dem","cc_cpd","cc_rheum","cc_pud","cc_mld","cc_dwocc","cc_mmren","cc_dwcc","cc_pleg","cc_malig","cc_msld","cc_sevren","cc_hiv","cc_mst","cc_aids")
cc_codes = codelist_fetch(cc_names_long, df = cc_codelist)
cc_wgts_2019 = c("1","1","1","1","1","1","1","1","1","1","1","2","2","2","3","3","3","6","6")
n_loops = length(cc_names_long)

create_table("charlson", glue::glue("
  WITH combined AS (
    SELECT a.enrolid
      {paste0(lapply(1:(n_loops-1), function(i) glue::glue('
        , COALESCE(b{i}.{name}, {sQuote(0)}) AS {name}
        , COALESCE(b{i}.{name}_index, 0) AS {name}_index
      ', name=cc_names_short[i])), collapse = ' ')}
			, CASE WHEN b17.cc_hiv = '1' AND b19.cc_aids = '1' THEN '1' ELSE '0' END AS cc_aids
			, CASE WHEN b17.cc_hiv = '1' AND b19.cc_aids = '1' THEN 6 ELSE 0 END AS cc_aids_index
    FROM {prefix}sample_enrolids AS a
    {paste0(lapply(1:n_loops, function(i) glue::glue('
      LEFT JOIN (
        SELECT DISTINCT enrolid, {sQuote(1)} AS {cc_names_short[i]}, {cc_wgts_2019[i]} AS {cc_names_short[i]}_index
        FROM {prefix}diagnosis_claims
        WHERE DATEDIFF(svcdate, index_dt) BETWEEN -365 AND -1
					AND (
						(dxver={sQuote(9)} AND (
							SUBSTR(dx1,1,3) IN ({icd9cm}) OR SUBSTR(dx1,1,4) IN ({icd9cm}) OR SUBSTR(dx1,1,5) IN ({icd9cm})
							OR SUBSTR(dx2,1,3) IN ({icd9cm}) OR SUBSTR(dx2,1,4) IN ({icd9cm}) OR SUBSTR(dx2,1,5) IN ({icd9cm})
							OR SUBSTR(dx3,1,3) IN ({icd9cm}) OR SUBSTR(dx3,1,4) IN ({icd9cm}) OR SUBSTR(dx3,1,5) IN ({icd9cm})
							OR SUBSTR(dx4,1,3) IN ({icd9cm}) OR SUBSTR(dx4,1,4) IN ({icd9cm}) OR SUBSTR(dx4,1,5) IN ({icd9cm})
						))
						OR (dxver={sQuote(0)} AND (
							SUBSTR(dx1,1,3) IN ({icd10cm}) OR SUBSTR(dx1,1,4) IN ({icd10cm}) OR SUBSTR(dx1,1,5) IN ({icd10cm})
							OR SUBSTR(dx2,1,3) IN ({icd10cm}) OR SUBSTR(dx2,1,4) IN ({icd10cm}) OR SUBSTR(dx2,1,5) IN ({icd10cm})
							OR SUBSTR(dx3,1,3) IN ({icd10cm}) OR SUBSTR(dx3,1,4) IN ({icd10cm}) OR SUBSTR(dx3,1,5) IN ({icd10cm})
							OR SUBSTR(dx4,1,3) IN ({icd10cm}) OR SUBSTR(dx4,1,4) IN ({icd10cm}) OR SUBSTR(dx4,1,5) IN ({icd10cm})
						))
					)
      ) AS b{i} ON a.enrolid=b{i}.enrolid
    ', icd9cm=cc_codes[[i]][['ICD-9-CM']], icd10cm=cc_codes[[i]][['ICD-10-CM']])), collapse = ' ')}
  )
  SELECT enrolid, {paste0(cc_names_short, collapse=', ')}
			, cc_mi_index + cc_chf_index + cc_pvd_index + cc_dem_index + cc_cpd_index + cc_rheum_index + cc_pud_index 
				+ GREATEST(cc_pleg_index, cc_cvd_index)
				+ GREATEST(cc_msld_index, cc_mld_index)
				+ GREATEST(cc_dwcc_index, cc_dwocc_index)
				+ GREATEST(cc_sevren_index, cc_mmren_index)
				+ GREATEST(cc_mst_index, cc_malig_index)
				+ GREATEST(cc_aids_index, cc_hiv_index)
      AS cci
  FROM combined
"))
