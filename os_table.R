

# generate median OS and 95% CI

generate_os_table <- function (dat, time, event, strata, var, event_type) {
  
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
  } else if (event_type == 'ttd') {
    dat <- dat %>%
      rename(
        `event_ind` = event_ind_ttd,
        `tt_event` = event_time_ttd
      ) 
  }
  
  
  if (var == 'overall') {
    
    params <- list(
      time = substitute(time),
      event = substitute(event),
      strata = substitute(strata),
      dat = substitute(dat)
    )
    
    expr <- substitute(
      survfit(Surv(time = time, event = event) ~ strata, data = dat))
    model <- eval(expr)
    sum <- summary(model)$table
    
    tbl_output <- summary(model)$table %>%
      data.frame() %>%
      transmute(
        Category = var,
        N = sum[[1]],
        Events = sum[[4]],
        `Median [Months]` = round(sum[[7]],2),
        `Lower 95% CI` = round(sum[[8]],2),
        `Upper 95% CI` = round(sum[[9]],2)
      ) %>%
      unique() %>%
      mutate(
        variable = var
      ) %>%
      select(
        variable, Category, N, Events, 
        `Median [Months]`, `Lower 95% CI`, `Upper 95% CI`
      )
    
    rownames(tbl_output) <- NULL
    
  } else {
    
    if (var %in% colnames(dat)) {
      
      params <- list(
        time = substitute(time),
        event = substitute(event),
        strata = substitute(strata),
        dat = substitute(dat)
      )
      
      expr <- substitute(
        survfit(Surv(time = time, event = event) ~ strata, data = dat))
      
      model <- eval(expr)
      
      sum <- summary(model)$table %>%
        data.frame()
      
      result <- summary(model)$table %>%
        data.frame() %>%
        transmute(
          Category = rownames(sum),
          N = sum$records,
          Events = sum$events,
          `Median [Months]` = round(sum$median,2),
          `Lower 95% CI` = round(sum$X0.95LCL,2),
          `Upper 95% CI` = round(sum$X0.95UCL,2)
        ) %>%
        mutate(
          variable = var,
          Category = str_split(Category, paste0(var, '='), simplify = T)[ , 2]
        )
      
      rownames(result) <- NULL
      
      tbl_output <- data.frame(
        variable = var,
        Category = var,
        N = NA,
        Events = NA,
        Median = NA,
        LowerCI = NA,
        UpperCI = NA
      ) %>%
        rename(
          `Median [Months]` = Median,
          `Lower 95% CI` = LowerCI,
          `Upper 95% CI` = UpperCI
        ) %>%
        bind_rows(result) 
      
    } else {
      
      tbl_output <- data.frame(
        variable = var,
        Category = var,
        N = NA,
        Events = NA,
        Median = NA,
        LowerCI = NA,
        UpperCI = NA
      ) %>%
        rename(
          `Median [Months]` = Median,
          `Lower 95% CI` = LowerCI,
          `Upper 95% CI` = UpperCI
        ) 
      
    }
    
  }
  
  return(tbl_output)
  
}





# generate survival probability on specific timepoint 

generate_os_timepoint <- function (dat, time, event, strata, var, event_type, time_point) {
  
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
  } else if (event_type == 'ttd') {
    dat <- dat %>%
      rename(
        `event_ind` = event_ind_ttd,
        `tt_event` = event_time_ttd
      ) 
  }
  
  
  if (var == 'overall') {
    
    params <- list(
      time = substitute(time),
      event = substitute(event),
      strata = substitute(strata),
      dat = substitute(dat)
    )
    
    expr <- substitute(
      survfit(Surv(time, event) ~ strata, data = dat)
    )
    model <- eval(expr)
    
    surv_sum <- summary(
      model, 
      times = time_point
    )
    
    result <- data.frame(
      variable = var, 
      category = var,
      timepoint = paste0(time_point, ' month'),
      n_risk = surv_sum$n.risk,
      n_event = surv_sum$n.event,
      surv_prob = round(surv_sum$surv, 4),
      surv_prob_lower = round(surv_sum$lower, 4),
      surv_prob_uppper = round(surv_sum$upper, 4)
    ) %>%
      rename(
        `Category` = category,
        `Time point` = timepoint,
        `At risk` = n_risk,
        `Events` = n_event,
        `Survival probability` = surv_prob,
        `Lower 95% CI` = surv_prob_lower,
        `Upper 95% CI` = surv_prob_uppper
      )
    
  } else {
    
    params <- list(
      time = substitute(time),
      event = substitute(event),
      strata = substitute(strata),
      dat = substitute(dat)
    )
    
    expr <- substitute(
      survfit(Surv(time, event) ~ strata, data = dat)
    )
    model <- eval(expr)
    
    surv_sum <- summary(
      model, 
      times = time_point
    )
    
    result <- data.frame(
      variable = var,
      category = var,
      timepoint = NA,
      n_risk = NA,
      n_event = NA,
      surv_prob = NA,
      surv_prob_lower = NA,
      surv_prob_uppper = NA
    ) %>%
      bind_rows(
        data.frame(
          variable = var, 
          category = rownames(surv_sum$table),
          timepoint = paste0(time_point, ' month'),
          n_risk = surv_sum$n.risk,
          n_event = surv_sum$n.event,
          surv_prob = round(surv_sum$surv, 4),
          surv_prob_lower = round(surv_sum$lower, 4),
          surv_prob_uppper = round(surv_sum$upper, 4)
        ) %>%
          mutate(
            category = str_split(category, paste0(var, '='), simplify = T)[ , 2]
          )
      ) %>%
      rename(
        `Category` = category,
        `Time point` = timepoint,
        `At risk` = n_risk,
        `Events` = n_event,
        `Survival probability` = surv_prob,
        `Lower 95% CI` = surv_prob_lower,
        `Upper 95% CI` = surv_prob_uppper
      )
    
  }
  
  return(result)
  
}





# generate median OS and 95% CI (landmark analysis)

generate_os_table_landmark <- function (dat, time, event, strata, var, event_type, landmark) {
  
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
  } else if (event_type == 'ttd') {
    dat <- dat %>%
      rename(
        `event_ind` = event_ind_ttd,
        `tt_event` = event_time_ttd
      ) 
  }
  
  # subset population and re-calculate time to event
  dat <- dat %>%
    filter(tt_event >= landmark) %>%
    mutate(tt_event = tt_event - landmark)
  
  
  if (var == 'overall') {
    
    params <- list(
      time = substitute(time),
      event = substitute(event),
      strata = substitute(strata),
      dat = substitute(dat)
    )
    
    expr <- substitute(
      survfit(Surv(time = time, event = event) ~ strata, data = dat))
    model <- eval(expr)
    sum <- summary(model)$table
    
    tbl_output <- summary(model)$table %>%
      data.frame() %>%
      transmute(
        Category = var,
        N = sum[[1]],
        Events = sum[[4]],
        `Median [Months]` = round(sum[[7]],2),
        `Lower 95% CI` = round(sum[[8]],2),
        `Upper 95% CI` = round(sum[[9]],2)
      ) %>%
      unique() %>%
      mutate(
        variable = var
      ) %>%
      select(
        variable, Category, N, Events, 
        `Median [Months]`, `Lower 95% CI`, `Upper 95% CI`
      )
    
    rownames(tbl_output) <- NULL
    
  } else {
    
    if (var %in% colnames(dat)) {
      
      params <- list(
        time = substitute(time),
        event = substitute(event),
        strata = substitute(strata),
        dat = substitute(dat)
      )
      
      expr <- substitute(
        survfit(Surv(time = time, event = event) ~ strata, data = dat))
      
      model <- eval(expr)
      
      sum <- summary(model)$table %>%
        data.frame()
      
      result <- summary(model)$table %>%
        data.frame() %>%
        transmute(
          Category = rownames(sum),
          N = sum$records,
          Events = sum$events,
          `Median [Months]` = round(sum$median,2),
          `Lower 95% CI` = round(sum$X0.95LCL,2),
          `Upper 95% CI` = round(sum$X0.95UCL,2)
        ) %>%
        mutate(
          variable = var,
          Category = str_split(Category, paste0(var, '='), simplify = T)[ , 2]
        )
      
      rownames(result) <- NULL
      
      tbl_output <- data.frame(
        variable = var,
        Category = var,
        N = NA,
        Events = NA,
        Median = NA,
        LowerCI = NA,
        UpperCI = NA
      ) %>%
        rename(
          `Median [Months]` = Median,
          `Lower 95% CI` = LowerCI,
          `Upper 95% CI` = UpperCI
        ) %>%
        bind_rows(result) 
      
    } else {
      
      tbl_output <- data.frame(
        variable = var,
        Category = var,
        N = NA,
        Events = NA,
        Median = NA,
        LowerCI = NA,
        UpperCI = NA
      ) %>%
        rename(
          `Median [Months]` = Median,
          `Lower 95% CI` = LowerCI,
          `Upper 95% CI` = UpperCI
        ) 
      
    }
    
  }
  
  return(tbl_output)
  
}













