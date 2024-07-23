

# need to merge in vitals to get BMI
# need to merge in insurance type 
# CCI
# ECOG
# Number of lots
# Number of metastatic sites at baseline
# Coexistence of other metastasis sites with brain lesions prior to 1L initiation
# first metastatic site
# presence of brain metastases: at baseline, at induction, at maintenance, after 1L end date

raw_metastasis <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/metastasis.sql')))%>% rename_all(tolower)
raw_ecog <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/ecog.sql')))%>% rename_all(tolower)
raw_vitals <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/vitals.sql')))%>% rename_all(tolower)
raw_cci <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/cci.sql'))) %>% rename_all(tolower)
raw_insurance <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/insurance.sql'))) %>% rename_all(tolower)

insurance_pt <- raw_insurance %>%
  inner_join(cohort %>% select(patient_id, index_date), by = 'patient_id') %>%
  filter(
    start_date <= index_date & !is.na(start_date)
  ) %>%
  group_by(patient_id) %>% 
  filter(
    start_date == min(start_date)
  ) %>% 
  select(patient_id,payer_category) %>% 
  distinct() %>% 
  mutate(
    flag_medicare = max(as.numeric(payer_category == 'Medicare')),
    flag_commercial = max(as.numeric(payer_category == 'Commercial Health Plan')),
    flag_medicaid = max(as.numeric(payer_category == 'Medicaid')),
    flag_missing = max(as.numeric(is.na(payer_category)))
  ) %>% 
  select(patient_id,flag_medicare,flag_commercial,flag_medicaid,flag_missing) %>% 
  distinct() %>% 
  mutate(
    insurance = case_when(
      flag_medicare == 1 ~ 'Medicare',
      flag_commercial == 1 ~ 'Commercial Health Plan',
      flag_medicaid == 1 ~ 'Medicaid',
      flag_missing == 1 ~ 'Missing',
      T ~ 'Others'
    )
  )



bmi_pt <- raw_vitals %>%
  inner_join(cohort %>% select(patient_id, index_date), by = 'patient_id') %>%
  filter(
    test_date >= index_date -30 & test_date <= index_date +7
  ) %>%
  mutate(days_diff = abs(test_date - index_date)) %>%
  group_by(patient_id, lab_component) %>%
  filter(days_diff == min(days_diff)) %>%
  filter(test_result_cleaned == max(test_result_cleaned)) %>%
  select(patient_id, lab_component, test_result_cleaned) %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(
    names_from = lab_component,
    values_from = test_result_cleaned
  ) %>%
  rename(
    `body_weight_kg` = `Body Weight`,
    `body_height_cm` = `Body Height`
  ) %>%
  mutate(
    bmi_index = round(body_weight_kg / (body_height_cm/100)^2, 1),
    bmi_cat = factor(case_when(
      bmi_index < 18.5 ~ 'Underweight',
      bmi_index >= 18.5 & bmi_index <= 24.9 ~ 'Normal weight',
      bmi_index >= 25.0 & bmi_index <= 29.9 ~ 'Overweight',
      bmi_index >= 30 ~ 'Obese',
      TRUE ~ 'Missing'
    ),
    levels = c('Underweight', 'Normal weight', 'Overweight', 'Obese', 'Missing'))
  )


ecog_pt <- raw_ecog %>%
  inner_join(cohort %>% select(patient_id, index_date),by = 'patient_id') %>%
  filter(
    ecog_date <= index_date +180
  ) %>%
  mutate(days_diff = abs(index_date - ecog_date)) %>%
  group_by(patient_id) %>%
  filter(days_diff == min(days_diff)) %>%
  filter(ecog_date == min(ecog_date)) %>%
  filter(ecog_value == max(ecog_value)) %>%
  ungroup() %>%
  select(patient_id, ecog_value) %>%
  distinct() %>%
  mutate(
    ecog_cat = case_when(
      ecog_value == 0 ~ '0',
      ecog_value == 1 ~ '1',
      ecog_value == 2 ~ '2',
      ecog_value >= 3 ~ '3+'
    )
  )


metastasis_pt_raw <- raw_metastasis %>%
  mutate(
    date_of_metastasis = ymd(paste0(date_of_metastasis, '-01'))
  ) %>% 
  inner_join(cohort %>% select(patient_id, index_date), by = 'patient_id') %>%
  # filter(date_of_metastasis<index_date) %>% 
  mutate(
    flag_bone = as.numeric(site_of_metastasis == 'Bone'),
    flag_lung = as.numeric(site_of_metastasis == 'Lung'),
    flag_distant_lymph_node = as.numeric(site_of_metastasis == 'Distant lymph node'),
    flag_liver = as.numeric(site_of_metastasis == 'Liver'),
    flag_brain = as.numeric(site_of_metastasis == 'Brain'),
    flag_pleura = as.numeric(site_of_metastasis == 'Pleura'),
    flag_visceral = as.numeric(site_of_metastasis %in% c('Liver', 'Lung')),
    flag_other = as.numeric(!site_of_metastasis %in% c('Liver', 'Lung', 'Bone', 'Brain', 'Distant lymph node', 'Pleura'))
  ) 

metastasis_pt_1st <- metastasis_pt_raw %>% 
  arrange(patient_id, date_of_metastasis) %>% 
  group_by(patient_id) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(
    site_of_metastasis_1st = site_of_metastasis
  ) %>% 
  select(patient_id, site_of_metastasis_1st)


presence_brain_met <- metastasis_pt_raw %>% 
  filter(site_of_metastasis == 'Brain') %>% 
  inner_join(trt_pattern2 %>%
              select(patient_id, induction, induction_startdt, induction_enddt, maintenance_startdt, maintenance_enddt, line_end_date),
              by = 'patient_id') %>%
  mutate(
    flag_brianmets_bl_all = case_when(date_of_metastasis <= index_date ~ 1,
                                    T ~ 0),
    flag_brianmets_bl = case_when(induction == 1 & date_of_metastasis <=  index_date  ~ 1, 
                                  induction == 1 ~ 0), 
    flag_brianmets_induction =  case_when(induction == 1 & 
                                            date_of_metastasis <= induction_enddt &
                                            date_of_metastasis >= induction_startdt 
                                          ~ 1, 
                                          induction == 1 ~ 0),
    flag_brianmets_maintenance = case_when(induction == 1 
                                           & date_of_metastasis <= maintenance_enddt 
                                           & date_of_metastasis >= maintenance_startdt ~ 1,
                                           induction == 1 ~ 0), 
    flag_brianmets_after1L = case_when(induction == 1 &
                                         date_of_metastasis >= line_end_date ~ 1, 
                                       induction == 1 ~ 0),
  ) %>% 
  group_by(patient_id) %>%
  mutate_at(vars(starts_with('flag_')), max) %>%
  ungroup() %>%
  select(patient_id, flag_brianmets_bl_all, 
         flag_brianmets_bl, flag_brianmets_induction, flag_brianmets_maintenance, flag_brianmets_after1L
         ) 





# drop_snowflake('GENESIS_TEMP.PUBLIC.JZ_RWE3033_PRESENCE')
# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_PRESENCE"), presence_brain_met %>% rename_all(toupper), overwrite = T)


  
metastasis_pt <- metastasis_pt_raw %>% 
  filter(
    date_of_metastasis <= index_date
  ) %>%
  group_by(patient_id) %>%
  mutate_at(vars(starts_with('flag_')), max) %>%
  mutate(
    metastasis_score = length(unique(site_of_metastasis))
  ) %>%
  ungroup() %>%
  select(patient_id, starts_with('flag_'), metastasis_score) %>%
  distinct() %>%
  right_join(cohort %>% select(patient_id), by = 'patient_id') %>%
  mutate_at(vars(starts_with('flag_')), function (x) {replace_na(x, 0)}) %>%
  mutate(
    metastasis_score = replace_na(metastasis_score, 0),
    metastasis_score_cat = case_when(
      metastasis_score == 0 ~ 'Missing',
      metastasis_score < 3 ~ '1 or 2',
      metastasis_score >= 3 ~ '3 or more'
    ),
    metastasis_score_cat2 = as.factor(metastasis_score),
    coexistence_other = case_when(
      flag_brain == 1 & metastasis_score == 1 ~ 'No (brian metastasis is the only metastaic lesion)',
      flag_brain == 1 & metastasis_score > 1 ~ 'Yes (lesions in other sites are presented)'
    ),
    
    coexist_flag_bone = case_when(flag_brain == 1 & metastasis_score > 1 & flag_bone == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),
    coexist_flag_lung = case_when(flag_brain == 1 & metastasis_score > 1 & flag_lung == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),
    coexist_flag_distant_lymph_node = case_when(flag_brain == 1 & metastasis_score > 1 & flag_distant_lymph_node == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),
    coexist_flag_liver = case_when(flag_brain == 1 & metastasis_score > 1 & flag_liver == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),
    coexist_flag_pleura = case_when(flag_brain == 1 & metastasis_score > 1 & flag_pleura == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),
    coexist_flag_visceral = case_when(flag_brain == 1 & metastasis_score > 1 & flag_visceral  == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),
    coexist_flag_other = case_when(flag_brain == 1 & metastasis_score > 1 & flag_other == 1 ~ 1, flag_brain == 1 & metastasis_score > 1 ~ 0),

  )

max_lot <- lot %>% rename_all(tolower) %>% 
  arrange(patient_id, line_number) %>% 
  group_by(patient_id) %>%
  filter(row_number() == n()) %>%
  ungroup() %>% 
  mutate(
    nlot = line_number
  ) %>% 
  select(patient_id, nlot) %>% 
  distinct()



# previous therapy
treatments_pre <- bind_rows(
  # admin
  admin %>%
    inner_join(cohort %>% 
                 filter(mbc_stage == 'Recurrent') %>% 
                 select(patient_id, index_date), 
               by = 'patient_id') %>%
    filter(administered_date < index_date ) %>%
    mutate(
      drug_name_sub = tolower(case_when(
        grepl('trastuzumab', common_drug_name) &
          !common_drug_name %in% c('ado-trastuzumab emtansine', 'fam-trastuzumab deruxtecan-nxki')
        ~ 'trastuzumab',
        grepl('paclitaxel', common_drug_name) |
          grepl('docetaxel', common_drug_name)
        ~ 'paclitaxel',
        grepl('cisplatin', common_drug_name) | 
          grepl('carboplatin', common_drug_name)
        ~ 'cisplatin-carboplatin',
        TRUE ~ common_drug_name
      ))
    ) %>%
    select(patient_id, 
           `drug_name` = drug_name_sub,
           `start_date` = administered_date,
           `end_date` = administered_date) %>%
    mutate(drug_source = 'administration'),
  
  # orals
  orals %>%
    inner_join(cohort %>% 
                 filter(mbc_stage == 'Recurrent') %>% 
                 select(patient_id, index_date), 
               by = 'patient_id') %>%
    filter(start_date < index_date ) %>%
    mutate(drug_name = tolower(drug_name)) %>%
    select(patient_id, drug_name, start_date, end_date) %>%
    mutate(drug_source = 'oral')
) %>%
  mutate(
    drug_name = case_when(
      drug_name == 'tucatinib' ~ 'tukysa',
      drug_name == 'fam-trastuzumab deruxtecan-nxki' ~ 'enhertu',
      drug_name == 'ado-trastuzumab emtansine' ~ 'kadcyla',
      TRUE ~ drug_name
    ),
    her2drug = as.numeric(grepl('tukysa|enhertu|kadcyla|margetuximab|neratinib|lapatinib|trastuzumab|pertuzumab', drug_name))==1
  ) %>% 
  filter(
    her2drug == 1
  ) %>% 
  select(patient_id, drug_name) %>% 
  distinct() %>% 
  arrange(patient_id, drug_name) %>% 
  group_by(patient_id) %>% 
  mutate(
    pre_her2_therapy = paste0(drug_name, collapse = ",")
  ) %>% 
  select(patient_id, pre_her2_therapy) %>% 
  distinct() %>% 
  ungroup()%>%
  right_join(cohort %>% 
               filter(mbc_stage == 'Recurrent') %>% 
               select(patient_id), 
             by = 'patient_id') %>% 
  mutate(
    flg_trastu = ifelse(grepl("trastuzumab", pre_her2_therapy),1,0),
    flg_pertuzumab = ifelse(grepl("pertuzumab", pre_her2_therapy),1,0),
    flg_tukysa = ifelse(grepl("tukysa", pre_her2_therapy),1,0),
    flg_enhertu = ifelse(grepl("enhertu", pre_her2_therapy),1,0),
    flg_kadcyla = ifelse(grepl("kadcyla", pre_her2_therapy),1,0),
    flg_neratinib = ifelse(grepl("neratinib", pre_her2_therapy),1,0),
    flg_lapatinib = ifelse(grepl("lapatinib", pre_her2_therapy),1,0),
    flg_margetuximab = ifelse(grepl("margetuximab", pre_her2_therapy),1,0),
  
    pre_her2_therapy_all = case_when(
      is.na(pre_her2_therapy) == T ~ 'Without previous HER2 therapy',
      T ~ 'With previous HER2 therapy'
    )
  )







baseline <- cohort %>% 
  mutate(
    age_at_index = year(index_date) - birth_year,
    age_at_index_cat = case_when(
      age_at_index>=18&age_at_index<=24 ~ '18-24',
      age_at_index>=25&age_at_index<=44 ~ '25-44',
      age_at_index>=45&age_at_index<=64 ~ '45-64',
      age_at_index>=65&age_at_index<=84 ~ '65-84',
      age_at_index>=85 ~ '85+'
    ),
    index_year_2020 = case_when(
      year(index_date) < 2020 ~ "Before 2020",
      year(index_date) >= 2020 ~ "In/After 2020",
      T ~ "No treatment"
    ),
    hr_status = case_when(
      is.na(hr_status) ~ 'Unknown',
      T ~ hr_status
    )
  )%>%
  left_join(bmi_pt, by = 'patient_id') %>%
  left_join(ecog_pt, by = 'patient_id') %>%
  left_join(metastasis_pt, by = 'patient_id') %>%
  left_join(presence_brain_met, by = 'patient_id') %>%
  left_join(metastasis_pt_1st, by = 'patient_id') %>%
  # left_join(raw_cci%>% select(patient_id,cci_score), by = 'patient_id')  %>% 
  left_join(max_lot, by = 'patient_id') %>% 
  left_join(treatments_pre, by ='patient_id') %>% 
  left_join(insurance_pt, by ='patient_id') %>%
  left_join(charlson %>% rename_all(tolower) %>%  select(patient_id, cci), by = "patient_id") %>% 
  mutate(
    bmi_cat = replace_na(bmi_cat, 'Missing'),
    ecog_cat = replace_na(ecog_cat, 'Missing'),
    cci_cat = case_when(
      cci == 0 ~ '0',
      cci == 1|cci == 2  ~ '1-2',
      cci == 3|cci == 4  ~ '3-4',
      cci >= 5 ~ '5+',
      T ~ 'Missing'
  ),
  met_year = as.factor(year(met_diagnosis_date)),
  index_year = as.factor(year(index_date)),
  time_met_to_line = (index_date - met_diagnosis_date + 1)/30.4375,
  time_diag_to_met =  case_when(
    mbc_stage == 'Recurrent'~ (met_diagnosis_date - diagnosis_date + 1)/30.4375
    )
)



#table(baseline$hr_status)




demo_label_pt <- list(
  age_at_index = 'Age at index date',
  age_at_index_cat = 'Age group at index date',
  gender = 'Sex',
  race = 'Race',
  ethnicity = 'Ethnicity',
  state = 'State',
  region = 'Region',
  bmi_index = 'BMI at Treatment initiation',
  bmi_cat = 'BMI group at Treatment initiation',
  practice_type = 'Practice type',
  insurance = 'Insurance type'
  
)

demo_label_trt <- list(
  cci = 'CCI',
  cci_cat = 'CCI categories',
  ecog_value = 'ECOG',
  ecog_cat = 'ECOG categories',
  hr_status = 'HR status at baseline',
  er_status = 'ER status at baselin',
  pr_status = 'PR status at baselin',
  group_stage = 'Stage at initial diagnosis',
  met_year = 'Year of metastatic diagnosis',
  index_year = '1L initiation index year',
  time_met_to_line = 'Time to Treatment initiation [months] (from met diagnosis)',
  nlot = 'Number of LOTs received in the metastatic setting',
  mbc_stage = 'De Novo vs. Recurrent mBC',
  time_diag_to_met = 'The time from breast cancer diagnosis (Stages 0-III) to the metastatic diagnosis date (stage IV)',
  pre_her2_therapy_all = 'previous exposure to HER2 blockers',
  flg_trastu = "Previous trastuzumab therapies for recurrent patient",
  flg_pertuzumab  = "Previous pertuzumab therapies for recurrent patient", 
  flg_tukysa = "Previous tukysa therapies for recurrent patient", 
  flg_enhertu = "Previous enhertu therapies for recurrent patient",
  flg_kadcyla = "Previous Kadcyla therapies for recurrent patient", 
  flg_neratinib = "Previous neratinib therapies for recurrent patient", 
  flg_lapatinib = "Previous lapatinib therapies for recurrent patient", 
  flg_margetuximab = "Previous margetuximab therapies for recurrent patient",
  
  
  metastasis_score_cat = 'Number of metastatic sites at baseline',
  flag_bone = 'Metastasis: Bone',
  flag_lung = 'Metastasis: Lung',
  flag_distant_lymph_node = 'Metastasis: Distant lymph node',
  flag_liver = 'Metastasis: Liver',
  flag_brain = 'Metastasis: Brain',
  flag_pleura = 'Metastasis: Pleura',
  flag_visceral = 'Metastasis: Visceral',
  flag_other = 'Metastasis: Other',
  coexistence_other = 'Coexistence of other metastatic sites with brain lesions prior to 1L initiation ',
  coexist_flag_bone = 'Coexistence of other metastatic sites with brain lesions: Bone',
  coexist_flag_lung = 'Coexistence of other metastatic sites with brain lesions: Lung',
  coexist_flag_distant_lymph_node = 'Coexistence of other metastatic sites with brain lesions: Distant lymph node',
  coexist_flag_liver = 'Coexistence of other metastatic sites with brain lesions: Liver',
  coexist_flag_pleura = 'Coexistence of other metastatic sites with brain lesions: Pleura',
  coexist_flag_visceral = 'Coexistence of other metastatic sites with brain lesions: Visceral',
  coexist_flag_other = 'Coexistence of other metastatic sites with brain lesions: Other',
  site_of_metastasis_1st = 'Site of first progression (first metastatic site)',
  flag_brianmets_bl_all = 'Presence of brain metastases at baseline (for all patients)',
  flag_brianmets_bl = 'Presence of brain metastases at baseline (for patients with 1L involving maintenance and induction)',
  flag_brianmets_induction = 'Presence of brain metastases at induction (for patients with 1L involving maintenance and induction)',
  flag_brianmets_maintenance = 'Presence of brain metastases at maintenance (for patients with 1L involving maintenance and induction)',
  flag_brianmets_after1L = 'Presence of brain metastases after 1L end date (for patients with 1L involving maintenance and induction)'
)


baseline_trt_table <- baseline %>% 
  select(names(demo_label_trt), brain_mets) %>% 
  set_attr_labels(demo_label_trt) %>% 
  tbl_summary(
    by = brain_mets,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    missing = "no",
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0,2)
    ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()





baseline_demo_table <- baseline %>% 
  select(names(demo_label_pt), brain_mets) %>% 
  set_attr_labels(demo_label_pt) %>% 
  tbl_summary(
    by = brain_mets,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    missing = "no",
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0,2)
    ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()





baseline_demo_table2 <- baseline %>% 
  select(names(demo_label_pt), hr_status) %>% 
  set_attr_labels(demo_label_pt) %>% 
  tbl_summary(
    by = hr_status,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    missing = "no",
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0,2)
    ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()




baseline_demo_table3 <- baseline %>% 
  select(names(demo_label_pt), index_year_2020) %>% 
  set_attr_labels(demo_label_pt) %>% 
  tbl_summary(
    by = index_year_2020,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    missing = "no",
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ c(0,2)
    ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()