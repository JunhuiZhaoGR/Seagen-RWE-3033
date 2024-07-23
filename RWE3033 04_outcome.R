
# dbGetQuery(con, glue::glue(read_file("~/RWE3033/query/last visit.sql")))
visit <- dbGetQuery(con, 'select * from GENESIS_TEMP.PUBLIC.JZ_RWE3033_visit') %>% rename_all(tolower)


outcome <- obj2 %>% 
  left_join(visit, by='patient_id') %>% 
  mutate(
    # OS
    event_ind_death = as.numeric(!is.na(death_date)),
    event_time_death = case_when(
      event_ind_death == 1 ~ round(as.numeric((death_date - line_start_date + 1)/30.4375), 2),
      event_ind_death == 0 ~ round(as.numeric((endpoint_date - line_start_date + 1)/30.4375), 2)
    ),
    
    # TTNT
    event_date_ttnt = ymd(pmin(next_line_start_date, death_date, na.rm = T)),
    event_ind_ttnt = as.numeric(!is.na(event_date_ttnt)),
    event_time_ttnt = case_when(
      event_ind_ttnt == 1 ~ round(as.numeric((event_date_ttnt - line_start_date + 1)/30.4375), 2),
      event_ind_ttnt == 0 ~ round(as.numeric((endpoint_date - line_start_date + 1)/30.4375), 2)
    ),  
    
    # TTD
    event_ind_ttd = case_when(
      !is.na(next_line_start_date) ~ 1,
      !is.na(death_date) ~ 1,
      last_visitdt - line_end_date >= 120 ~ 1,
      TRUE ~ 0
    ),
    event_time_ttd = case_when(
      !is.na(next_line_start_date) ~ 
        round(as.numeric((next_line_start_date - 1 - line_start_date + 1)/30.4375), 2),
      !is.na(death_date) ~ 
        round(as.numeric((death_date - line_start_date + 1)/30.4375), 2),
      TRUE ~ round(as.numeric((line_end_date - line_start_date + 1)/30.4375), 2)
    )
  )

# table(outcome$event_ind_death)
# 
# dbWriteTable(con, SQL("GENESIS_TEMP.PUBLIC.JZ_RWE3033_OUTCOME"), outcome %>% rename_all(toupper), overwrite = T)
# 
# os_output <- get_km_stats(
#   dat=outcome,
#   time_var = event_time_death,
#   event_var = event_ind_death, 
#   strata_var = brain_mets,
#   title = "real world overall survival",
#   ylab = "survival probability"
# )
# 
# ttd_output <- get_km_stats(
#   dat=outcome,
#   time_var = event_time_ttd,
#   event_var = event_ind_ttd, 
#   strata_var = brain_mets,
#   title = "real world time to discontinuation",
#   ylab = "probability"  
# )
# 
# ttnt_output <- get_km_stats(
#   dat=outcome,
#   time_var = event_time_ttnt,
#   event_var = event_ind_ttnt, 
#   strata_var = brain_mets,
#   title = "real world time to next treatment",
#   ylab = "probability"  
# )
# 
# 
# 
# 
# 
# table1_outcome <- outcome %>% 
#   select(event_time_death, event_ind_death, event_time_ttd, event_ind_ttd, event_time_ttnt, event_ind_ttnt, brain_mets) %>% 
#   tbl_summary(
#     by = brain_mets,
#     type = list(all_continuous() ~ "continuous2"),
#     statistic = list(
#       all_continuous() ~ c("{mean} ({sd})",
#                            "{median} ({p25}, {p75})",
#                            "{min}, {max}")
#     ),
#     missing = "no",
#     digits = list(
#       all_continuous() ~ 2,
#       all_categorical() ~ c(0,2)
#     ),
#   ) %>% 
#   add_overall() %>% 
#   bold_labels() %>% 
#   add_stat_label()
# 
# 
# 
# 
# 
# OS <- surv_fit(Surv(time = event_time_death, event = event_ind_death) ~ 1, data=outcome)
# OS <- surv_fit(Surv(time = event_time_death, event = event_ind_death) ~ brain_mets, data=outcome)
# OS_model <- eval(OS)
# OS_summary <- summary(OS_model)$table
# 
# survminer::ggsurvplot(
#   fit=OS, 
#   risk.table = TRUE, 
#   surv.median.line = 'hv',
#   break.x.by = 12,
#   legend.labs = c('No brian mets', 'brain mets'),
#   xlab = 'Months', 
#   ylab = 'Overall survival probability')
# 
# generate_os_table (outcome, tt_event, event_ind, 1, 'overall', 'os')
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 12)
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 24)
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 36)
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 48)
# 
# 
# 
# generate_os_table (outcome, tt_event, event_ind, brain_mets, 'brain_mets', 'os')  %>% 
#   filter(Category != 'brain_mets')
# 
# table(outcome$brain_mets)
# 
# 
# generate_os_timepoint (outcome, tt_event, event_ind, brain_mets, 'brain_mets', 'os', 12)
# 
# 
# 
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 24)
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 36)
# generate_os_timepoint (outcome, tt_event, event_ind, 1, 'overall', 'os', 48)
# 
# generate_km_curve(outcome, tt_event, event_ind, brain_mets, 'os','mBC', 'line start')
# 
# 
# ggsurvplot(
#   surv_fit(Surv(time = event_time_death, event = event_ind_death) ~ brain_mets, data=outcome),
#   data = outcome,
#   surv.median.line = 'hv',
#   risk.table = T,
#   # ggtheme = theme_bw(), 
#   legend = 'top',
#   legend.labs = c('No brian mets', 'brain mets'),
#   break.x.by = 12,
#   xlab = 'Time [Months]',
#   ylab = 'Overall survival probability',
#   pval = T
# )
# 
# 
