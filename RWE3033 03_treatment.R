

firstL <- cohort %>% 
  left_join(lot %>% rename_all(tolower) %>% 
              filter(line_number == 1) , 
            by = 'patient_id') %>% 
  mutate(
    firstL_era = factor(
      case_when(
        year(line_start_date) < 2020 ~ 'Pre-2020',
        year(line_start_date) >= 2020 ~ 'Post-2020'
      ),
      levels = c('Pre-2020', 'Post-2020')
    ),
    brain_mets = case_when(
      brain_mets == FALSE ~ 'No Brian Metastases',
      brain_mets == TRUE ~ 'Brian Metastases'
        
    )
  ) %>% 
  left_join(lot %>% rename_all(tolower) %>% 
              filter(line_number == 2) %>% 
              mutate(
                line_name_2L = line_name
              ) %>% 
              select(line_name_2L, patient_id), 
            by = 'patient_id') 




trt_pattern <- firstL %>% 
  group_by(patient_id) %>% 
  mutate(
    treatment_cat = case_when(
      sum(flag_tukysa_line, flag_enhertu_line, flag_kadcyla_line,
          flag_margetuximab_line, flag_neratinib_line,
          flag_lapatinib_line, flag_trastuzumab_line, flag_pertuzumab_line) >=2
      & endocrine == T ~ 'Dual HER2 targeted with endocrine therapy',
      sum(flag_tukysa_line, flag_enhertu_line, flag_kadcyla_line,
          flag_margetuximab_line, flag_neratinib_line,
          flag_lapatinib_line, flag_trastuzumab_line, flag_pertuzumab_line) >=2
      & endocrine == F ~ 'Dual HER2 targeted without endocrine therapy',
      sum(flag_tukysa_line, flag_enhertu_line, flag_kadcyla_line,
          flag_margetuximab_line, flag_neratinib_line,
          flag_lapatinib_line, flag_trastuzumab_line, flag_pertuzumab_line) ==1
      & endocrine == T ~ 'Single HER2 targeted with endocrine therapy',
      sum(flag_tukysa_line, flag_enhertu_line, flag_kadcyla_line,
          flag_margetuximab_line, flag_neratinib_line,
          flag_lapatinib_line, flag_trastuzumab_line, flag_pertuzumab_line) ==1
      & endocrine == F ~ 'Single HER2 targeted without endocrine therapy', 
      sum(flag_tukysa_line, flag_enhertu_line, flag_kadcyla_line,
          flag_margetuximab_line, flag_neratinib_line,
          flag_lapatinib_line, flag_trastuzumab_line, flag_pertuzumab_line) ==0
      & endocrine == F & flag_chemotherapy_line == 1 ~ 'Chemotherapy only',
      T ~ 'Other'
    ),
    n_HER2_targeted = sum(flag_tukysa_line, flag_enhertu_line, flag_kadcyla_line,
                          flag_margetuximab_line, flag_neratinib_line,
                          flag_lapatinib_line, flag_trastuzumab_line, flag_pertuzumab_line),
    # taxane_type = case_when(
    #   flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1 & paclitaxel_protein_bound == T ~ 'paclitaxel protein-bound',
    #   flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1 & paclitaxel == T ~ 'paclitaxel',
    #   flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1 & docetaxel == T ~ 'docetaxel', 
    #   flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1 ~ 'missing'
    # )
  ) %>% 
  ungroup()

# table(trt_pattern$brain_mets)

# single anti-HER2 therapy
antiHER2 <- trt_pattern %>% 
  filter(n_HER2_targeted==1&line_name=='trastuzumab')

sort(table(antiHER2$line_name_2L),decreasing = T)

# other
Other <- trt_pattern %>% 
  filter(treatment_cat=='Other')
sort(table(Other$line_name),decreasing = T)



# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_TREATMENT"), trt_pattern %>% rename_all(toupper), overwrite = T)

trt_pattern2 <- trt_pattern %>% 
  mutate(
    induction = case_when(
      flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1
      & last_her2drug_date > last_chemo_date & line_end_date >= last_chemo_date + 28 ~ 1,
      flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1 ~ 0
    ),
    induction_startdt = case_when (
      induction == 1  ~ line_start_date
    ),
    induction_enddt = case_when (
      induction == 1  ~ last_chemo_date + 20
    ),
    maintenance_startdt = case_when(
      induction == 1  ~ induction_enddt + 1
    ),
    maintenance_enddt = case_when(
      induction == 1  ~ line_end_date
    ),
    dur_induction = (induction_enddt - induction_startdt + 1)/30.4375,
    dur_maintenance = (maintenance_enddt - maintenance_startdt + 1)/30.4375,
    dur_1L = (line_end_date - line_start_date + 1)/30.4375
  )

# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_TREATMENT2"), trt_pattern %>% rename_all(toupper), overwrite = T)
# table(trt_pattern2$induction)
# table(trt_pattern2$flag_trastuzumab_line)
# table(trt_pattern2$flag_pertuzumab_line)
# table(trt_pattern2$flag_taxane_line)


trt_pattern_all <- trt_pattern %>% 
  mutate(
    induction = case_when(
      last_her2drug_date > last_chemo_date & line_end_date >= last_chemo_date + 28 ~ 1,
      T ~ 0
    ),
    induction_startdt = case_when (
      induction == 1  ~ line_start_date
    ),
    induction_enddt = case_when (
      induction == 1  ~ last_chemo_date + 20
    ),
    maintenance_startdt = case_when(
      induction == 1  ~ induction_enddt + 1
    ),
    maintenance_enddt = case_when(
      induction == 1  ~ line_end_date
    ),
    dur_induction = (induction_enddt - induction_startdt + 1)/30.4375,
    dur_maintenance = (maintenance_enddt - maintenance_startdt + 1)/30.4375,
    dur_1L = (line_end_date - line_start_date + 1)/30.4375
  )

drop_snowflake('GENESIS_TEMP.PUBLIC.JZ_RWE3033_INDUCTION')
dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_INDUCTION"), trt_pattern_all %>% rename_all(toupper), overwrite = T)




chemo_treatments <- bind_rows(
  # admin
  admin %>%
    inner_join(patients %>% select(patient_id, index_date, endpoint_date), by = 'patient_id') %>%
    filter(administered_date >= index_date - 14 & administered_date < endpoint_date) %>%
    select(patient_id, drug_name,
           `start_date` = administered_date,
           `end_date` = administered_date) %>%
    mutate(drug_source = 'administration'),
  
  # orals
  orals %>%
    inner_join(patients %>% select(patient_id, index_date, endpoint_date), by = 'patient_id') %>%
    filter(start_date >= index_date - 14 & start_date < endpoint_date) %>%
    mutate(drug_name = tolower(drug_name)) %>%
    select(patient_id, drug_name, start_date, end_date) %>%
    mutate(drug_source = 'oral')

  ) %>% 
  filter(str_detect(drug_name, "capecitabine|cyclophosphamide|carboplatin|docetaxel|paclitaxel|eribulin|gemcitabine|doxorubicin|vinorelbine|ixabepilone|cisplatin|oxaliplatin|topotecan|irinotecan|etoposide|pemetrexed|azacitidine|cladribine|cytarabine|decitabine|methotrexate|trifluridine/tipiracil|fluorouracil|vincristine|epirubicin|mitoxantrone|mitomycin|temozolomide|lurbinectedin|tazemetostat|thiotepa|ifosfamide") == T)



  
  
obj2 <- trt_pattern2 %>% 
  filter(flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1
         &n_HER2_targeted == 2) 


obj_2chemo <- obj2 %>% filter(induction == 1) %>% 
  left_join(chemo_treatments %>% rename_all(tolower), by="patient_id") %>% 
  filter(start_date<=induction_enddt & end_date>=induction_startdt) %>% 
  select(-start_date, -end_date) %>% 
  distinct() %>% 
  arrange(patient_id, drug_name) %>% 
  group_by(patient_id) %>% 
  mutate(
    chemo_drug = paste0(drug_name, collapse = ",")
  ) %>% 
  select(patient_id, chemo_drug, brain_mets) %>% 
  distinct() %>% 
  ungroup()
 

trt_label <- list(
  induction = 'Proportion of patients with induction/maintenance phases',
  dur_induction = 'duration of induction',
  dur_maintenance = 'duration of maintenance'
)

trt_label1 <- list(
  treatment_cat = '1L treatment categories'
)

# treatment pattern by brain mets
table1_trt1 <- trt_pattern2 %>% 
  select(treatment_cat, brain_mets) %>% 
  set_attr_labels(trt_label1) %>% 
  tbl_summary(
    by = brain_mets,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    # digits = list(
    #   all_continuous() ~ 2,
    #   all_categorical() ~ c(0,2)
    # ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()


# proportion of patients with induction/maintenance phases
# duration of induction and maintenance 
table1_trt <- obj2 %>% 
  select(names(trt_label), brain_mets) %>% 
  set_attr_labels(trt_label) %>% 
  tbl_summary(
    by = brain_mets,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    missing = "no"
    # digits = list(
    #   all_continuous() ~ 2,
    #   all_categorical() ~ c(0,2)
    # ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()


label_chemodrug <- list(
  chemo_drug = 'Chemotherapy agents used in induction, n (%)'
)



# chemotherapy agents used in induction
table1_chemodrug <- obj_2chemo %>% 
  select(chemo_drug, brain_mets) %>% 
  set_attr_labels(label_chemodrug) %>% 
  tbl_summary(
    by = brain_mets,
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}")
    ),
    missing = "no"
    # digits = list(
    #   all_continuous() ~ 2,
    #   all_categorical() ~ c(0,2)
    # ),
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  add_stat_label()


#differentiate platinum and paclitaxel in linename, for 1L only

trt_pattern3 <- trt_pattern2 %>% 
  left_join(chemo_treatments, by = "patient_id") %>% 
  filter(end_date <= end_date_1l & start_date >= index_date) %>%
  select(-start_date, -end_date) %>% 
  distinct() %>% 
  arrange(patient_id, drug_name) %>% 
  group_by(patient_id, line_number) %>% 
  mutate(chemo_drugs = paste0(drug_name, collapse = ", ")) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(
    line_name_clean = str_replace_all(line_name, "paclitaxel", chemo_drugs)
  ) %>% 
  select(patient_id, line_name_clean) %>% 
  distinct() 


trt_pattern4 <- trt_pattern2 %>% 
  left_join(trt_pattern3, by = "patient_id") %>% 
  mutate(
    line_name = case_when(
      !is.na(line_name_clean) ~ line_name_clean,
      T ~ line_name
    ),
    trt_of_interest = case_when(
      flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_taxane_line == 1 ~ "trastuzumab + pertuzumab + taxane",
      flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_chemotherapy_line == 1 ~ "trastuzumab + pertuzumab + other chemo", 
      flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 & flag_tki_line == 1 ~ "trastuzumab + pertuzumab + tki",
      line_name == "pertuzumab, trastuzumab" ~ "trastuzumab + pertuzumab",
      line_name == "pertuzumab, trastuzumab, tukysa" ~ "pertuzumab + trastuzumab + tukysa",
      flag_trastuzumab_line == 1 & flag_pertuzumab_line == 1 ~ "trastuzumab + pertuzumab + other",
      flag_trastuzumab_line == 1 & flag_taxane_line == 1 ~ "trastuzumab + taxane",
      flag_trastuzumab_line == 1 & flag_chemotherapy_line == 1 & flag_taxane_line != 1 ~ "trastuzumab + other chemo",
      line_name == "trastuzumab, tukysa" ~ "trastuzumab + tukysa",
      line_name == "capecitabine, trastuzumab, tukysa" ~ "trastuzumab + tukysa + capecitabine",
      line_name == "trastuzumab" ~ "trastuzumab monotherapy",
      flag_trastuzumab_line == 1 ~ "trastuzumab + other",
      flag_chemotherapy_line == 1 ~ "Chemotherapy Only",
      line_name == "kadcyla" ~ "kadcyla", 
      line_name == "enhertu" ~ "enhertu",
      T ~ "Others"
    )
  )

table(trt_pattern4$trt_of_interest)