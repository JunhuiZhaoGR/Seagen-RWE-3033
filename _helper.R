
# converts US states to a set of regions

generate_region <- function(data, state_var) {
  state_var <- enquo(state_var)
  
  # These are the standard categorizations of States -> Regions
  data %>%
    mutate(
      state = case_when(
        !!state_var %in% c("CT", "ME", "MA", "NH", "RI","VT", "NJ", "NY", "PA") ~ "Northeast",
        !!state_var %in% c("IN", "IL", "MI", "OH", "WI", "IA", "KS", "MN", "MO",
                           "NE", "ND", "SD") ~ "Midwest",
        !!state_var %in% c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
                           "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX") ~ "South",
        !!state_var %in% c("AZ", "CO", "ID", "NM", "MT", "UT", "NV", "WY", "AK",
                           "CA", "HI", "OR", "WA") ~ "West",
        !!state_var == "PR" ~ 'Puerto Rico',
        
        TRUE ~ 'Unknown'
      ),
      
      state = factor(
        state,
        levels = c('Midwest', 'West', 'Northeast', 'South', 'Puerto Rico', 'Unknown')
      )
    )
}





# set variable label

set_attr_labels <- function(dat, labels) {
  
  for(i in seq_along(labels)) {
    
    attr(dat[[names(labels[i])]], "label") <- labels[[i]]
    
  }
  
  return(dat)
}





# generate treatment use frequeny table by line

generate_treament_use_line <- function (dat) {
  
  dat %>%
    mutate(
      n_total = length(unique(patient_id))
    ) %>%
    group_by(line_number) %>%
    mutate(n_line = n()) %>%
    select(line_number, n_line, n_total) %>%
    distinct() %>%
    mutate(
      percent_line = paste0(round(n_line/n_total*100, 2), '%')
    ) %>%
    arrange(line_number) %>%
    mutate(line_number = paste0(line_number, 'L')) %>%
    rename(
      `line number` = line_number,
      `pts in line, N` = n_line,
      `pts in total, N` = n_total,
      `percent` = percent_line
    )
  
}





# generate regimen group frequency table for treatment pattern

generate_regimen_freq <- function (dat) {
  
  dat %>%
    group_by(line_number) %>%
    mutate(n_total_line = n()) %>%
    group_by(line_number, line_name_cat) %>%
    mutate(n_line_name = n()) %>%
    select(line_number, line_name_cat, n_line_name, n_total_line) %>%
    distinct() %>%
    mutate(
      percent_line_name = paste0(round(n_line_name/n_total_line*100, 2), '%')
    ) %>%
    arrange(line_number, desc(n_line_name)) %>%
    rename(
      `line number` = line_number,
      `regimen group` = line_name_cat,
      `pts in regimen, N` = n_line_name,
      `pts in line, N` = n_total_line,
      `percent in line, %` = percent_line_name
    )
  
}





# generate regimen group frequency table for treatment pattern

generate_prior_regimen_freq <- function (dat) {
  
  dat %>%
    group_by(line_number) %>%
    mutate(n_total_line = n()) %>%
    group_by(line_number, line_name_cat2) %>%
    mutate(n_line_name = n()) %>%
    select(line_number, line_name_cat2, n_line_name, n_total_line) %>%
    distinct() %>%
    mutate(
      percent_line_name = paste0(round(n_line_name/n_total_line*100, 2), '%')
    ) %>%
    arrange(line_number, desc(n_line_name)) %>%
    rename(
      `line number` = line_number,
      `regimen group` = line_name_cat2,
      `pts in regimen, N` = n_line_name,
      `pts in line, N` = n_total_line,
      `percent in line, %` = percent_line_name
    )
  
}





# generate regimen group frequency table for treatment pattern

generate_line_name_freq <- function (dat) {
  
  bind_rows(
    dat %>%
      mutate(n_total = n()) %>%
      group_by(line_name) %>%
      mutate(n_line_name = n()) %>%
      select(line_name, n_line_name, n_total) %>%
      distinct() %>%
      mutate(
        percent_line_name = paste0(round(n_line_name/n_total*100, 2), '%')
      ) %>%
      arrange(desc(n_line_name)) %>%
      mutate(group = 'Overall'),
    
    dat %>%
      filter(bm_status_line == 'BM prior to Line start') %>%
      mutate(n_total = n()) %>%
      group_by(line_name) %>%
      mutate(n_line_name = n()) %>%
      select(line_name, n_line_name, n_total) %>%
      distinct() %>%
      mutate(
        percent_line_name = paste0(round(n_line_name/n_total*100, 2), '%')
      ) %>%
      arrange(desc(n_line_name)) %>%
      mutate(group = 'BM prior to Line start'),
    
    dat %>%
      filter(bm_status_line == 'No BM prior to Line start') %>%
      mutate(n_total = n()) %>%
      group_by(line_name) %>%
      mutate(n_line_name = n()) %>%
      select(line_name, n_line_name, n_total) %>%
      distinct() %>%
      mutate(
        percent_line_name = paste0(round(n_line_name/n_total*100, 2), '%')
      ) %>%
      arrange(desc(n_line_name)) %>%
      mutate(group = 'No BM prior to Line start')
  ) %>%
    select(
      group,
      `regimen` = line_name,
      `pts in regimen, N` = n_line_name,
      `pts in total, N` = n_total,
      `percent in line, %` = percent_line_name
    )
  
}




# patient count for landmark analysis in OS/TTNT

get_patient_count <- function (dat, event_type) {
  
  if (event_type == 'os') {
    dat <- dat %>%
      rename(
        `event_ind` = event_ind_death,
        `tt_event` = event_time_death
      ) 
  } else if (event_type == 'ttnt'){
    dat <- dat %>%
      rename(
        `event_ind` = event_ind_ttnt,
        `tt_event` = event_time_ttnt
      ) 
  }
  
  
  tbl_pt_count <- dat %>%
    mutate(n_total = n()) %>%
    filter(tt_event >= 1) %>%
    mutate(n_1m = n()) %>%
    filter(tt_event >= 3) %>%
    mutate(n_3m = n()) %>%
    filter(tt_event >= 6) %>%
    mutate(n_6m = n()) %>%
    filter(tt_event >= 9) %>%
    mutate(n_9m = n()) %>%
    filter(tt_event >= 12) %>%
    mutate(n_12m = n()) %>%
    select(starts_with('n_')) %>%
    distinct() %>%
    gather() %>%
    mutate(
      key = case_when(
        key == 'n_total' ~ 'Total patients',
        key == 'n_1m' ~ '1 month',
        key == 'n_3m' ~ '3 month',
        key == 'n_6m' ~ '6 month',
        key == 'n_9m' ~ '9 month',
        key == 'n_12m' ~ '12 month'
      )
    ) %>%
    rename(
      `Landmark time` = key,
      `pt count, N` = value
    )
  
  return(tbl_pt_count)
  
}





# rename column name for DOT and TTNT flag based on followup time (to generate tables)

persistence_column <- function(dat, trt_type, followup) {
  
  if (trt_type %in% c('tukysa', 'enhertu', 'kadcyla')) {
    
    if (followup == '1m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_1m_dot,
          `followup_dot` = followup_1m_dot,
          `flag_followup_ttnt` = flag_followup_1m_ttnt,
          `followup_ttnt` = followup_1m_ttnt
        )
    } else if (followup == '2m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_2m_dot,
          `followup_dot` = followup_2m_dot,
          `flag_followup_ttnt` = flag_followup_2m_ttnt,
          `followup_ttnt` = followup_2m_ttnt
        )
    } else if (followup == '3m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_3m_dot,
          `followup_dot` = followup_3m_dot,
          `flag_followup_ttnt` = flag_followup_3m_ttnt,
          `followup_ttnt` = followup_3m_ttnt
        )
    } else if (followup == '4m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_4m_dot,
          `followup_dot` = followup_4m_dot,
          `flag_followup_ttnt` = flag_followup_4m_ttnt,
          `followup_ttnt` = followup_4m_ttnt
        )
    } else if (followup == '5m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_5m_dot,
          `followup_dot` = followup_5m_dot,
          `flag_followup_ttnt` = flag_followup_5m_ttnt,
          `followup_ttnt` = followup_5m_ttnt
        )
    } else if (followup == '6m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_6m_dot,
          `followup_dot` = followup_6m_dot,
          `flag_followup_ttnt` = flag_followup_6m_ttnt,
          `followup_ttnt` = followup_6m_ttnt
        )
    } else if (followup == '7m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_7m_dot,
          `followup_dot` = followup_7m_dot,
          `flag_followup_ttnt` = flag_followup_7m_ttnt,
          `followup_ttnt` = followup_7m_ttnt
        )
    } else if (followup == '8m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_8m_dot,
          `followup_dot` = followup_8m_dot,
          `flag_followup_ttnt` = flag_followup_8m_ttnt,
          `followup_ttnt` = followup_8m_ttnt
        )
    } else if (followup == '9m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_9m_dot,
          `followup_dot` = followup_9m_dot,
          `flag_followup_ttnt` = flag_followup_9m_ttnt,
          `followup_ttnt` = followup_9m_ttnt
        )
    } else if (followup == '10m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_10m_dot,
          `followup_dot` = followup_10m_dot,
          `flag_followup_ttnt` = flag_followup_10m_ttnt,
          `followup_ttnt` = followup_10m_ttnt
        )
    } else if (followup == '11m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_11m_dot,
          `followup_dot` = followup_11m_dot,
          `flag_followup_ttnt` = flag_followup_11m_ttnt,
          `followup_ttnt` = followup_11m_ttnt
        )
    } else if (followup == '12m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_12m_dot,
          `followup_dot` = followup_12m_dot,
          `flag_followup_ttnt` = flag_followup_12m_ttnt,
          `followup_ttnt` = followup_12m_ttnt
        )
    } else if (followup == '13m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_13m_dot,
          `followup_dot` = followup_13m_dot,
          `flag_followup_ttnt` = flag_followup_13m_ttnt,
          `followup_ttnt` = followup_13m_ttnt
        )
    } else if (followup == '14m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_14m_dot,
          `followup_dot` = followup_14m_dot,
          `flag_followup_ttnt` = flag_followup_14m_ttnt,
          `followup_ttnt` = followup_14m_ttnt
        )
    } else if (followup == '15m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_15m_dot,
          `followup_dot` = followup_15m_dot,
          `flag_followup_ttnt` = flag_followup_15m_ttnt,
          `followup_ttnt` = followup_15m_ttnt
        )
    } else if (followup == '16m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_16m_dot,
          `followup_dot` = followup_16m_dot,
          `flag_followup_ttnt` = flag_followup_16m_ttnt,
          `followup_ttnt` = followup_16m_ttnt
        )
    } else if (followup == '17m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_17m_dot,
          `followup_dot` = followup_17m_dot,
          `flag_followup_ttnt` = flag_followup_17m_ttnt,
          `followup_ttnt` = followup_17m_ttnt
        )
    } else if (followup == '18m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_18m_dot,
          `followup_dot` = followup_18m_dot,
          `flag_followup_ttnt` = flag_followup_18m_ttnt,
          `followup_ttnt` = followup_18m_ttnt
        )
    } else if (followup == '19m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_19m_dot,
          `followup_dot` = followup_19m_dot,
          `flag_followup_ttnt` = flag_followup_19m_ttnt,
          `followup_ttnt` = followup_19m_ttnt
        )
    } else if (followup == '20m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_20m_dot,
          `followup_dot` = followup_20m_dot,
          `flag_followup_ttnt` = flag_followup_20m_ttnt,
          `followup_ttnt` = followup_20m_ttnt
        )
    } else if (followup == '21m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_21m_dot,
          `followup_dot` = followup_21m_dot,
          `flag_followup_ttnt` = flag_followup_21m_ttnt,
          `followup_ttnt` = followup_21m_ttnt
        )
    } else if (followup == '22m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_22m_dot,
          `followup_dot` = followup_22m_dot,
          `flag_followup_ttnt` = flag_followup_22m_ttnt,
          `followup_ttnt` = followup_22m_ttnt
        )
    } else if (followup == '23m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_23m_dot,
          `followup_dot` = followup_23m_dot,
          `flag_followup_ttnt` = flag_followup_23m_ttnt,
          `followup_ttnt` = followup_23m_ttnt
        )
    } else if (followup == '24m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_24m_dot,
          `followup_dot` = followup_24m_dot,
          `flag_followup_ttnt` = flag_followup_24m_ttnt,
          `followup_ttnt` = followup_24m_ttnt
        )
    } else if (followup == '25m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_25m_dot,
          `followup_dot` = followup_25m_dot,
          `flag_followup_ttnt` = flag_followup_25m_ttnt,
          `followup_ttnt` = followup_25m_ttnt
        )
    } 

  } else if (trt_type %in% c('ttc', 'ttc uk')) {
    
    if (followup == '1m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_1m_dot,
          `followup_dot` = followup_1m_dot,
          `flag_followup_dot_tukysa` = flag_followup_1m_dot_tukysa,
          `followup_dot_tukysa` = followup_1m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_1m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_1m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_1m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_1m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_1m_ttnt,
          `followup_ttnt` = followup_1m_ttnt
        ) 
    } else if (followup == '3m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_3m_dot,
          `followup_dot` = followup_3m_dot,
          `flag_followup_dot_tukysa` = flag_followup_3m_dot_tukysa,
          `followup_dot_tukysa` = followup_3m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_3m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_3m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_3m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_3m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_3m_ttnt,
          `followup_ttnt` = followup_3m_ttnt
        ) 
    } else if (followup == '6m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_6m_dot,
          `followup_dot` = followup_6m_dot,
          `flag_followup_dot_tukysa` = flag_followup_6m_dot_tukysa,
          `followup_dot_tukysa` = followup_6m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_6m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_6m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_6m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_6m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_6m_ttnt,
          `followup_ttnt` = followup_6m_ttnt
        ) 
    } else if (followup == '9m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_9m_dot,
          `followup_dot` = followup_9m_dot,
          `flag_followup_dot_tukysa` = flag_followup_9m_dot_tukysa,
          `followup_dot_tukysa` = followup_9m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_9m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_9m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_9m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_9m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_9m_ttnt,
          `followup_ttnt` = followup_9m_ttnt
        ) 
    } else if (followup == '12m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_12m_dot,
          `followup_dot` = followup_12m_dot,
          `flag_followup_dot_tukysa` = flag_followup_12m_dot_tukysa,
          `followup_dot_tukysa` = followup_12m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_12m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_12m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_12m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_12m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_12m_ttnt,
          `followup_ttnt` = followup_12m_ttnt
        ) 
    } else if (followup == '18m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_18m_dot,
          `followup_dot` = followup_18m_dot,
          `flag_followup_dot_tukysa` = flag_followup_18m_dot_tukysa,
          `followup_dot_tukysa` = followup_18m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_18m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_18m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_18m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_18m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_18m_ttnt,
          `followup_ttnt` = followup_18m_ttnt
        ) 
    } else if (followup == '24m') {
      dat %>%
        rename(
          `flag_followup_dot` = flag_followup_24m_dot,
          `followup_dot` = followup_24m_dot,
          `flag_followup_dot_tukysa` = flag_followup_24m_dot_tukysa,
          `followup_dot_tukysa` = followup_24m_dot_tukysa,
          `flag_followup_dot_trastuzumab` = flag_followup_24m_dot_trastuzumab,
          `followup_dot_trastuzumab` = followup_24m_dot_trastuzumab,
          `flag_followup_dot_capecitabine` = flag_followup_24m_dot_capecitabine,
          `followup_dot_capecitabine` = followup_24m_dot_capecitabine,
          `flag_followup_ttnt` = flag_followup_24m_ttnt,
          `followup_ttnt` = followup_24m_ttnt
        ) 
    }

  }
  
}





# patient count for persistence analysis

get_patient_count_persistence <- function (dat, trt_type) {
  
  if (trt_type %in% c('tukysa', 'enhertu', 'kadcyla', 'ttc', 'ttc uk')) {
    
    tbl_pt_count <- dat %>%
      mutate(n_total = n()) %>%
      filter(followup_time >= 1) %>%
      mutate(n_1m = n()) %>%
      filter(followup_time >= 2) %>%
      mutate(n_2m = n()) %>%
      filter(followup_time >= 3) %>%
      mutate(n_3m = n()) %>%
      filter(followup_time >= 4) %>%
      mutate(n_4m = n()) %>%
      filter(followup_time >= 5) %>%
      mutate(n_5m = n()) %>%
      filter(followup_time >= 6) %>%
      mutate(n_6m = n()) %>%
      filter(followup_time >= 7) %>%
      mutate(n_7m = n()) %>%
      filter(followup_time >= 8) %>%
      mutate(n_8m = n()) %>%
      filter(followup_time >= 9) %>%
      mutate(n_9m = n()) %>%
      filter(followup_time >= 10) %>%
      mutate(n_10m = n()) %>%
      filter(followup_time >= 11) %>%
      mutate(n_11m = n()) %>%
      filter(followup_time >= 12) %>%
      mutate(n_12m = n()) %>%
      filter(followup_time >= 13) %>%
      mutate(n_13m = n()) %>%
      filter(followup_time >= 14) %>%
      mutate(n_14m = n()) %>%
      filter(followup_time >= 15) %>%
      mutate(n_15m = n()) %>%
      filter(followup_time >= 16) %>%
      mutate(n_16m = n()) %>%
      filter(followup_time >= 17) %>%
      mutate(n_17m = n()) %>%
      filter(followup_time >= 18) %>%
      mutate(n_18m = n()) %>%
      filter(followup_time >= 19) %>%
      mutate(n_19m = n()) %>%
      filter(followup_time >= 20) %>%
      mutate(n_20m = n()) %>%
      filter(followup_time >= 21) %>%
      mutate(n_21m = n()) %>%
      filter(followup_time >= 22) %>%
      mutate(n_22m = n()) %>%
      filter(followup_time >= 23) %>%
      mutate(n_23m = n()) %>%
      filter(followup_time >= 24) %>%
      mutate(n_24m = n()) %>%
      filter(followup_time >= 25) %>%
      mutate(n_25m = n()) %>%
      select(starts_with('n_')) %>%
      distinct() %>%
      gather() %>%
      mutate(
        key = case_when(
          key == 'n_total' ~ 'Total patients',
          key == 'n_1m' ~ 'Followup time >= 1month',
          key == 'n_2m' ~ 'Followup time >= 2month',
          key == 'n_3m' ~ 'Followup time >= 3month',
          key == 'n_4m' ~ 'Followup time >= 4month',
          key == 'n_5m' ~ 'Followup time >= 5month',
          key == 'n_6m' ~ 'Followup time >= 6month',
          key == 'n_7m' ~ 'Followup time >= 7month',
          key == 'n_8m' ~ 'Followup time >= 8month',
          key == 'n_9m' ~ 'Followup time >= 9month',
          key == 'n_10m' ~ 'Followup time >= 10month',
          key == 'n_11m' ~ 'Followup time >= 11month',
          key == 'n_12m' ~ 'Followup time >= 12month',
          key == 'n_13m' ~ 'Followup time >= 13month',
          key == 'n_14m' ~ 'Followup time >= 14month',
          key == 'n_15m' ~ 'Followup time >= 15month',
          key == 'n_16m' ~ 'Followup time >= 16month',
          key == 'n_17m' ~ 'Followup time >= 17month',
          key == 'n_18m' ~ 'Followup time >= 18month',
          key == 'n_19m' ~ 'Followup time >= 19month',
          key == 'n_20m' ~ 'Followup time >= 20month',
          key == 'n_21m' ~ 'Followup time >= 21month',
          key == 'n_22m' ~ 'Followup time >= 22month',
          key == 'n_23m' ~ 'Followup time >= 23month',
          key == 'n_24m' ~ 'Followup time >= 24month',
          key == 'n_25m' ~ 'Followup time >= 25month'
        )
      ) %>%
      rename(
        `Followup time` = key,
        `pt count, N` = value
      )
    
  } else {
    
    tbl_pt_count <- dat %>%
      mutate(n_total = n()) %>%
      filter(followup_time >= 1) %>%
      mutate(n_1m = n()) %>%
      filter(followup_time >= 2) %>%
      mutate(n_2m = n()) %>%
      filter(followup_time >= 3) %>%
      mutate(n_3m = n()) %>%
      filter(followup_time >= 4) %>%
      mutate(n_4m = n()) %>%
      filter(followup_time >= 5) %>%
      mutate(n_5m = n()) %>%
      filter(followup_time >= 6) %>%
      mutate(n_6m = n()) %>%
      filter(followup_time >= 7) %>%
      mutate(n_7m = n()) %>%
      filter(followup_time >= 8) %>%
      mutate(n_8m = n()) %>%
      filter(followup_time >= 9) %>%
      mutate(n_9m = n()) %>%
      filter(followup_time >= 10) %>%
      mutate(n_10m = n()) %>%
      filter(followup_time >= 11) %>%
      mutate(n_11m = n()) %>%
      filter(followup_time >= 12) %>%
      mutate(n_12m = n()) %>%
      select(starts_with('n_')) %>%
      distinct() %>%
      gather() %>%
      mutate(
        key = case_when(
          key == 'n_total' ~ 'Total patients',
          key == 'n_1m' ~ 'Followup time >= 1month',
          key == 'n_2m' ~ 'Followup time >= 2month',
          key == 'n_3m' ~ 'Followup time >= 3month',
          key == 'n_4m' ~ 'Followup time >= 4month',
          key == 'n_5m' ~ 'Followup time >= 5month',
          key == 'n_6m' ~ 'Followup time >= 6month',
          key == 'n_7m' ~ 'Followup time >= 7month',
          key == 'n_8m' ~ 'Followup time >= 8month',
          key == 'n_9m' ~ 'Followup time >= 9month',
          key == 'n_10m' ~ 'Followup time >= 10month',
          key == 'n_11m' ~ 'Followup time >= 11month',
          key == 'n_12m' ~ 'Followup time >= 12month'
        )
      ) %>%
      rename(
        `Followup time` = key,
        `pt count, N` = value
      )
    
  }
  
  return(tbl_pt_count)
  
}







