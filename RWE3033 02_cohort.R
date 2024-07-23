
#step 1. patients diagnosed with mBC.
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/01_mBC.sql")))
step1 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_01')

#step 2. patients 18 years and older at mBC diagnosis date
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/02_18 years or older.sql")))
step2 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_02')


# copy Shu's code below to build LOT
############################################################################################

## patients ---------------------------------------------------------------------------------------
raw_patients <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/patients.sql')))
raw_patients <- raw_patients %>% rename_all(tolower)


patients <- raw_patients %>%
  mutate(
    diagnosis_date = ymd(diagnosis_date),
    met_diagnosis_date = ymd(met_diagnosis_date),
    endpoint_date = case_when(
      !is.na(death_date) ~ death_date,
      TRUE ~ last_activity_dt
    )
  ) %>% 
  generate_region(state) %>%
  mutate(
    index_date = met_diagnosis_date,
    # index_year = year(index_date),
    # age_at_index = year(index_date) - birth_year,
    # age_at_index_cat = case_when(
    #   age_at_index >= 18 & age_at_index <= 24 ~ '18-24',
    #   age_at_index >= 25 & age_at_index <= 44 ~ '25-44',
    #   age_at_index >= 45 & age_at_index <= 64 ~ '45-64',
    #   age_at_index >= 65 & age_at_index <= 74 ~ '65-74',
    #   age_at_index >= 75 & age_at_index <= 84 ~ '75-84',
    #   age_at_index >= 85 ~ '85+',
    # ),
    gender = case_when(
      gender == 'F' ~ 'Female',
      gender == 'M' ~ 'Male'
    ),
    race_ethnicity = coalesce(race, ethnicity),
    race_ethnicity = factor(case_when(
      !is.na(race_ethnicity) ~ race_ethnicity,
      TRUE ~ 'Unknown/Missing'
    ),
    levels = c('White', 'Black or African American', 'Asian',
               'Not Hispanic or Latino', 'Hispanic or Latino',
               'Other Race', 'Unknown/Missing')),
    group_stage = case_when(
      group_stage %in% c('0', 'I', 'II', 'III', 'IV') ~ paste0('Stage ', group_stage),
      TRUE ~ 'Unknown'
    ),
    mbc_stage = case_when(
      group_stage == 'Stage IV' |
        met_diagnosis_date == diagnosis_date |
        (group_stage == 'Unknown' & met_diagnosis_date - diagnosis_date >= 0 & met_diagnosis_date - diagnosis_date <= 90)
      ~ 'De Novo',
      TRUE ~ 'Recurrent'
    ),
    practice_type = factor(
      practice_type,
      levels = c('Academic', 'Community', 'Both')
    ),
    followup_time = round(as.numeric((endpoint_date - index_date)/30.4375), 2)
  )









raw_orals <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/orals.sql')))
raw_admin <- dbGetQuery(con, glue::glue(read_file('~/RWE3033/query/administration.sql')))


## administrations ---------------------------------------------------------------------------------

raw_admin <- raw_admin %>% rename_all(toupper)
varsToDrop <- names(raw_admin)

admin <- raw_admin %>%
  mutate(
    patient_id = PATIENT_ID,
    order_id = ORDER_ID,
    administered_date = ymd(ADMINISTERED_DATE),
    drug_name = DRUG_NAME,
    common_drug_name = COMMON_DRUG_NAME,
    route = ROUTE,
    drug_category = DRUG_CATEGORY,
    detailed_drug_category = DETAILED_DRUG_CATEGORY,
    administered_amount = ADMINISTERED_AMOUNT,
    administered_units = ADMINISTERED_UNITS
  ) %>% 
  select(-all_of(varsToDrop))





## orals ------------------------------------------------------------------------------------------

raw_orals <- raw_orals %>% rename_all(toupper)
varsToDrop <- names(raw_orals)

orals <- raw_orals %>%
  mutate(
    patient_id = PATIENT_ID,
    drug_name = DRUG_NAME,
    start_date = ymd(START_DATE),
    end_date = ymd(END_DATE),
    date_granularity = DATE_GRANULARITY
  ) %>% 
  select(-all_of(varsToDrop))




## treatment records ------------------------------------------------------------------------------
#  all antineoplastic therapies, but removed hormone therapies
#  drug name substitutions

treatments <- bind_rows(
  # admin
  admin %>%
    inner_join(patients %>% select(patient_id, index_date, endpoint_date), by = 'patient_id') %>%
    filter(administered_date >= index_date - 14 & administered_date <= endpoint_date) %>%
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
    inner_join(patients %>% select(patient_id, index_date, endpoint_date), by = 'patient_id') %>%
    filter(start_date >= index_date - 14 & start_date <= endpoint_date) %>%
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
    )
  ) %>%
  group_by(patient_id) %>%
  mutate(last_drug_date = max(start_date)) %>%
  ungroup()




# drop_snowflake('GENESIS_TEMP.PUBLIC.JZ_RWE3033_ORALS')
# drop_snowflake('GENESIS_TEMP.PUBLIC.JZ_RWE3033_TREATMENTS')
# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_ORALS"), orals %>% rename_all(toupper), overwrite = T)
# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_TREATMENTS"), treatments %>% rename_all(toupper), overwrite = T)

## lot --------------------------------------------------------------------------------------------

#  create LOT table based on customized rules, 
raw_lot <- build_lot(
  dat = treatments %>% select(-drug_source, -last_drug_date),
  patient_id = patient_id,
  drug_name = drug_name,
  drug_start_date = start_date,
  drug_end_date = end_date,
  time_window = 28,
  allowable_gap = 60,
  messages = FALSE, 
  extend_end_date = FALSE
) %>%
  rename(
    `patient_id` = PatientID,
    `line_number` = line,
    `line_start_date` = start_date,
    `line_end_date` = stop_date,
    `line_name` = regimen
  ) %>%
  arrange(patient_id, line_number)



#  modify line end date based on gap/switch criteria
#  priority: gap > switch
lot <- raw_lot %>%
  mutate(
    line_number_cat = case_when(
      line_number <= 3 ~ paste0(line_number, 'L'),
      line_number > 3 ~ '4+L'
    )
  ) %>%
  group_by(patient_id) %>%
  mutate(
    index_date_line = line_start_date,
    next_line_number = lead(line_number),
    next_line_start_date = lead(line_start_date),
    next_line_criteria = case_when(
      next_line_start_date - line_end_date > 60 ~ 'gap',
      next_line_start_date - line_end_date <= 60 
      & !is.na(next_line_start_date) ~ 'switch'
    ),
    line_end_date = case_when(
      next_line_criteria == 'gap' ~ line_end_date,
      next_line_criteria == 'switch' ~ next_line_start_date -1,
      TRUE ~ line_end_date
    )
  ) %>%
  ungroup() %>%
  mutate(
    # flag treatments use by LOT
    flag_tukysa_line = as.numeric(grepl('tukysa', line_name)),
    flag_enhertu_line = as.numeric(grepl('enhertu', line_name)),
    flag_kadcyla_line = as.numeric(grepl('kadcyla', line_name)),
    flag_margetuximab_line = as.numeric(grepl('margetuximab', line_name)),
    flag_tucatinib_line = as.numeric(grepl('tucatinib', line_name)),
    flag_neratinib_line = as.numeric(grepl('neratinib', line_name)),
    flag_lapatinib_line = as.numeric(grepl('lapatinib', line_name)),
    flag_ttc_line = as.numeric(grepl('capecitabine', line_name) &
                                 grepl('trastuzumab', line_name) &
                                 grepl('tukysa', line_name)),
    
    flag_trastuzumab_line = as.numeric(grepl('trastuzumab', line_name)),
    flag_pertuzumab_line = as.numeric(grepl('pertuzumab', line_name)),
    flag_taxane_line = as.numeric(grepl('docetaxel|paclitaxel', line_name)),
    flag_lapatinib_neratinib_line = as.numeric(str_detect(line_name, "lapatinib|neratinib")),
    flag_chemotherapy_line = as.numeric(str_detect(line_name, "capecitabine|cyclophosphamide|carboplatin|docetaxel|paclitaxel|eribulin|gemcitabine|doxorubicin|vinorelbine|ixabepilone|cisplatin|oxaliplatin|topotecan|irinotecan|etoposide|pemetrexed|azacitidine|cladribine|cytarabine|decitabine|methotrexate|trifluridine/tipiracil|fluorouracil|vincristine|epirubicin|mitoxantrone|mitomycin|temozolomide|lurbinectedin|tazemetostat|thiotepa|ifosfamide")),
    flag_tki_line = as.numeric(str_detect(line_name,"adagrasib|afatinib|alectinib|amivantamab|brigatinib|cabozantinib|capmatinib|ceritinib|crizotinib|dabrafenib|dacomitinib|entrectinib|erlotinib|gefitinib|larotrectinib|lorlatinib|mobocertinib|osimertinib|pralsetinib|selpercatinib|sotorasib|tepotinib|trametinib|trastuzumab|vandetanib|vemurafenib"))
  ) %>%
  group_by(patient_id) %>%
  mutate(
    # flag treatments use by LOT
    flag_tukysa_pt = max(flag_tukysa_line),
    flag_enhertu_pt = max(flag_enhertu_line),
    flag_kadcyla_pt = max(flag_kadcyla_line),
    flag_ttc_pt = max(flag_ttc_line)
  ) %>%
  ungroup() %>% rename_all(toupper)




# drop_snowflake('GENESIS_TEMP.PUBLIC.JZ_RWE3033_LOT')
# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_LOT"), lot, overwrite = T)

#################################################################################








#step 3. patients who were treated with systemic anticancer treatments in the metastatic setting
#        for at least one line of therapy. The date of 1L initiation will be index date
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/03_with LOT.sql")))
step3 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_03')


#step 4. patients whose last HER2 assessment prior to 1L initiation and up to 28 days is positive
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/04_HER2 positive.sql")))
step4 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_04')

#step 5. patients with last activity or death date (set to last day of the month) occurrence after 1L initiation date
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/05_death or lastvist after index.sql")))
step5 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_05')

#step 6. patients with no evidence of other cancer in the six months prior to the index date
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/06_no other cancer.sql")))
step6 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_06')

#step 7. patients who are female
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/07_female.sql")))
step7 <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_07')

HR <- snowflake_count('GENESIS_TEMP.PUBLIC.JZ_RWE3033_HR')



## cohort attrition table
tbl_cohort <- bind_rows(
  as_tibble(step1), as_tibble(step2), as_tibble(step3), 
  as_tibble(step4), as_tibble(step5), as_tibble(step6), as_tibble(step7)
)

tbl_cohort <-tbl_cohort %>% 
  mutate(pct_remained = case_when(
    row_number() < n() - 1 ~ PTS/lag(PTS),
    row_number() < n() ~ PTS/lag(PTS,2),
    row_number() == n() ~ PTS/lag(PTS,3)
  ),
  PTS = prettyNum(PTS, big.mark = ",", scientific = FALSE),
  pct_remained = percent(pct_remained))  


#subgroups
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/HR.sql")))
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/Brain mets.sql")))
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/endocrine.sql")))

# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/chemo.sql"))) 
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/HER2 drug.sql")))

lot <- dbGetQuery(con, 'select * from GENESIS_TEMP.PUBLIC.JZ_RWE3033_endocrine')

cohort <- dbGetQuery(con, 'select * from GENESIS_TEMP.PUBLIC.JZ_RWE3033_HER2DRUG') %>% 
  rename_all(tolower) %>% 
  left_join(patients %>% select(-index_date, -met_diagnosis_date, -last_activity_dt), by='patient_id')

