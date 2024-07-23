

# generate KM curve

generate_km_curve <- function (dat, time, event, strata_var, event_type, cap_label, legend_label) {
  
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
  
  
  params <- list(
    time = substitute(time),
    event = substitute(event),
    strata_var = substitute(strata_var),
    dat = substitute(dat)
  )
  
  expr <- substitute(
    survfit(Surv(time, event) ~ strata_var, data = dat),
    params
  )
  
  surv_obj <- eval(expr)
  
  if (legend_label == 'mBC') {
    ggsurvplot(
      surv_obj, 
      data = dat,
      surv.median.line = 'hv',
      risk.table = T,
      ggtheme = theme_bw(), 
      legend = 'bottom',
      legend.labs = c('Overall', 'BM prior to mBC date', 'No BM prior to mBC date'),
      break.x.by = 6,
      xlab = 'Time [Months]',
      pval = T,
      title = paste0('KM curve for ', cap_label)
    )
  } else if (legend_label == 'treatment start') {
    ggsurvplot(
      surv_obj, 
      data = dat,
      surv.median.line = 'hv',
      risk.table = T,
      ggtheme = theme_bw(), 
      legend = 'bottom',
      legend.labs = c('Overall', 'BM prior to Treatment initiation', 'No BM prior to Treatment initiation'),
      break.x.by = 6,
      xlab = 'Time [Months]',
      pval = T,
      title = paste0('KM curve for ', cap_label)
    )
  } else if (legend_label == 'line start') {
    ggsurvplot(
      surv_obj, 
      data = dat,
      surv.median.line = 'hv',
      risk.table = T,
      ggtheme = theme_bw(), 
      legend = 'bottom',
      legend.labs = c('Overall', 'BM prior to Line start', 'No BM prior to Line start'),
      break.x.by = 6,
      xlab = 'Time [Months]',
      pval = T,
      title = paste0('KM curve for ', cap_label)
    )
  }
  
}



# generate KM curve -- no BM subgroup only

generate_km_curve_single <- function (dat, time, event, strata_var, event_type, cap_label, legend_label) {
  
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
  
  
  params <- list(
    time = substitute(time),
    event = substitute(event),
    strata_var = substitute(strata_var),
    dat = substitute(dat)
  )
  
  expr <- substitute(
    survfit(Surv(time, event) ~ strata_var, data = dat),
    params
  )
  
  surv_obj <- eval(expr)
  
  if (legend_label == 'mBC') {
    ggsurvplot(
      surv_obj, 
      data = dat,
      surv.median.line = 'hv',
      risk.table = T,
      ggtheme = theme_bw(), 
      legend = 'bottom',
      legend.labs = c('Overall', 'No BM prior to mBC date'),
      break.x.by = 6,
      xlab = 'Time [Months]',
      pval = T,
      title = paste0('KM curve for ', cap_label)
    )
  } else if (legend_label == 'treatment start') {
    ggsurvplot(
      surv_obj, 
      data = dat,
      surv.median.line = 'hv',
      risk.table = T,
      ggtheme = theme_bw(), 
      legend = 'bottom',
      legend.labs = c('Overall', 'BM prior to Treatment initiation', 'No BM prior to Treatment initiation'),
      break.x.by = 6,
      xlab = 'Time [Months]',
      pval = T,
      title = paste0('KM curve for ', cap_label)
    )
  } else if (legend_label == 'line start') {
    ggsurvplot(
      surv_obj, 
      data = dat,
      surv.median.line = 'hv',
      risk.table = T,
      ggtheme = theme_bw(), 
      legend = 'bottom',
      legend.labs = c('Overall', 'BM prior to Line start', 'No BM prior to Line start'),
      break.x.by = 6,
      xlab = 'Time [Months]',
      pval = T,
      title = paste0('KM curve for ', cap_label)
    )
  }
  
}






# generate KM curve (landmark analysis)

generate_km_curve_landmark <- function (dat, time, event, strata_var, event_type, landmark, 
                                        cap_label, legend_label = NULL) {
  
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
  
  
  params <- list(
    time = substitute(time),
    event = substitute(event),
    strata_var = substitute(strata_var),
    dat = substitute(dat)
  )
  
  expr <- substitute(
    survfit(Surv(time, event) ~ strata_var, data = dat),
    params
  )
  
  surv_obj <- eval(expr)

  ggsurvplot(
    surv_obj, 
    data = dat,
    surv.median.line = 'hv',
    risk.table = T,
    ggtheme = theme_bw(), 
    legend = 'bottom',
    legend.labs = legend_label,
    #break.x.by = 5,
    xlab = 'Time [Months]',
    pval = T,
    title = paste0('KM curve for ', cap_label)
  )
  
}





# get legend labels

get_legend_label <- function (dat, event_type, landmark, bm_status) {
  
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
  
  
  if (bm_status == 'treatment start') {
    sort(unique(dat$km_strata_pt))
  } else if (bm_status == 'line start') {
    sort(unique(dat$km_strata_line))
  }
  
}





# generate KM curve

generate_km_curve_mbc <- function (dat, time, event, strata_var, event_type) {
  
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
  
  
  params <- list(
    time = substitute(time),
    event = substitute(event),
    strata_var = substitute(strata_var),
    dat = substitute(dat)
  )
  
  expr <- substitute(
    survfit(Surv(time, event) ~ strata_var, data = dat),
    params
  )
  
  surv_obj <- eval(expr)
  
  ggsurvplot(
    surv_obj, 
    data = dat,
    surv.median.line = 'hv',
    risk.table = T,
    ggtheme = theme_bw(), 
    legend = 'bottom',
    #legend.labs = c('Overall', 'BM prior to mBC date', 'No BM prior to mBC date'),
    break.x.by = 6,
    xlab = 'Time [Months]',
    pval = T
    #title = paste0('KM curve for ', cap_label)
  )

}





# generate KM curve overall only

generate_km_curve_overall <- function (dat, time, event, strata_var, event_type, cap_label) {
  
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
  
  
  params <- list(
    time = substitute(time),
    event = substitute(event),
    strata_var = substitute(strata_var),
    dat = substitute(dat)
  )
  
  expr <- substitute(
    survfit(Surv(time, event) ~ strata_var, data = dat),
    params
  )
  
  surv_obj <- eval(expr)
  
  ggsurvplot(
    surv_obj, 
    data = dat,
    surv.median.line = 'hv',
    risk.table = T,
    ggtheme = theme_bw(), 
    legend = 'bottom',
    legend.labs = "Overall",
    break.x.by = 6,
    xlab = 'Time [Months]',
    pval = T,
    title = paste0('KM curve for ', cap_label),
    conf.int = F
  )
  
}



