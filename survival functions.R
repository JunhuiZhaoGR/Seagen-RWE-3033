

#function to get time-to event stats
get_km_stats <- function(dat, time_var, event_var, strata_var, title, ylab) {
  
  time_var <- enquo(time_var)
  event_var <- enquo(event_var)
  strata_var <- enquo(strata_var)
  
  dat <- dat %>%
    mutate(
      Time_Var = !!time_var,
      Event_Var = !!event_var,
      Strata_Var = !!strata_var
    )
  
  #model
  model <- apply_survfit(dat, Time_Var,Event_Var, Strata_Var)
  
  # create summary table
  summary_tbl <- knitr::kable(
    summarise_survfit(model)
  ) %>%
    kableExtra::kable_styling(full_width = FALSE)
  
  
  # create plot
  km_plot <- create_ggsurvplot(
    dat,
    model,
    # legend_labels = final_cohort %>% select(trt_group) %>% unique() %>% pull(),
    risk_tbl_height = 0.35,
    x_axis_limit = 60,
    break_axis_by = 12,
    title = title,
    ylab = ylab)
  
  
  km_tbl <- survfit(Surv(Time_Var,Event_Var) ~ Strata_Var, dat) %>%
    tbl_survfit(times = seq.int(12,60,by=12), label_header="{time} Months",reverse=TRUE)
  
  output <- list(
    model = model,
    summary_tbl = summary_tbl,
    km_plot = km_plot,
    km_tbl= km_tbl
  )
  
  return(output)
  
}

  
#' Applies a survfit model to the data and parameters provided
#'
#' This function helps apply a survival model in a loop with multiple
#' data sets or with multiple varying parameters.
#' @import survival
#' @param dat The data frame to use
#' @param time The time variable in the specified data frame
#' @param event The event variable in the specified data frame
#' @param strata A variable to stratify the model by. Defaults to no stratification.
#' @return A survfit model object
#' @export
#' @examples
#' library(survival)
#'
#' data(ovarian) # this comes from the survival library
#'
#' apply_survfit(ovarian, time = futime, event = fustat)
#'
#' apply_survfit(ovarian, time = futime, event = fustat, strata = rx)

apply_survfit <- function(dat, time, event, strata = 1) {
  
  if (!"package:survival" %in% search()) {
    stop("Please load the survival library with library(survival)")
  }
  
  # create a list of parameter names
  params <- list(
    time = substitute(time),
    event = substitute(event),
    strata = substitute(strata)
  )
  
  # create the survival expression
  expr <- substitute(
    survfit(Surv(time = time, event = event) ~ strata, data = dat),
    params
  )
  
  # evaluate the expression to generate the model
  model <- eval(expr)
  
  return(model)
}

#' Generates tidy survival model output
#'
#' This function helps generate clean summary statistics from a survival model.
#' @import dplyr
#' @param survfit_model A survival model object created by survfit()
#' @return A data frame with: Strata, N, Events, and Median (95% CI)
#' @export
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data(ovarian) # this comes from the survival library
#'
#' model <- survfit(Surv(time = futime, event = fustat) ~ 1, data = ovarian)
#' summarise_survfit(model)

summarise_survfit <- function(survfit_model) {
  
  if (!"package:dplyr" %in% search()) {
    stop("Please load the dplyr library with library(dplyr)")
  }
  
  if (class(survfit_model) != "survfit") {
    stop("Model most be of class 'survfit'")
  }
  
  model_summary <- summary(survfit_model)$table
  
  # if the model is stratified, handle the extra rows
  if (length(dim(model_summary)) > 0) {
    model_summary <- tibble::rownames_to_column(
      as.data.frame(model_summary)
    )
  } else {
    model_summary <- tibble::enframe(model_summary) %>%
      tidyr::spread(key = name,
                    value = value) %>%
      mutate(rowname = '')
  }
  
  # create final output
  model_summary <- model_summary %>%
    mutate(`Median (95% CI)` = glue::glue("{median} ({LC}, {UC})",
                                          median = round(median, 2),
                                          LC = round(`0.95LCL`, 2),
                                          UC = round(`0.95UCL`, 2)),
           Strata = stringr::str_replace_all(rowname, ".*=", ""),
           `Events (%)` = glue::glue("{events_} ({percent}%)",
                                     events = events,
                                     events_ = format(as.numeric(events), nsmall=0, big.mark = ","),
                                     percent = round(events/records*100,2)),
           `Censored (%)` = glue::glue("{censored_} ({percent}%)",
                                       censored = records - events,
                                       censored_ = format(as.numeric(censored), nsmall=0, big.mark = ","),
                                       percent = round(censored/records*100,2))) %>%
    select(
      Strata,
      N = records,
      Events = events,
      `Events (%)`,
      `Censored (%)`,
      `Median (95% CI)`
    ) %>%
    mutate(N = format(as.numeric(N), nsmall=0, big.mark=","),
           Events = format(as.numeric(Events), nsmall=0, big.mark=","))
  
  return(model_summary)
}

#' A simple wrapper around the ggsurvplot function to standardize plots
#'
#' This function helps generate clean milestone statistics from a survival model.
#' @import survminer
#' @param dat The dataframe used to create the survival model
#' @param survfit_model A survival model object created by survfit()
#' @param time_units The units to be placed on the x-axis label. Default
#' is months so label would be "Time (Months)"
#' @param legend_labels A vector of labels to replace the default
#' ggsurvplot labels. This should be a vector of the same length of the legend
#' @return A KM plot
#' @export
#' @examples
#' library(survival)
#'
#' data(ovarian) # this comes from the survival library
#'
#' my_model <- apply_survfit(ovarian, time = futime, event = fustat, strata = rx)
#' create_ggsurvplot(ovarian, my_model)

create_ggsurvplot <- function(dat, survfit_model, time_units = "Months",
                              legend_labels = NULL, risk_tbl_height = 0.45, x_axis_limit, break_axis_by, title, ylab) {
  
  if (is.null(legend_labels)) {
    legend_labels <- names(survfit_model$strata)
  }
  
  km_plot <- ggsurvplot(
    fit = survfit_model,
    data = dat,
    risk.table = TRUE,
    surv.scale = "percent",
    xlab = paste0("Time (", time_units,")"),
    legend.labs = legend_labels,
    risk.table.height = risk_tbl_height,
    xlim = c(0,x_axis_limit),
    break.time.by = break_axis_by,
    pval = TRUE,
    title = title,
    ylab = ylab
  )
  
  return(km_plot)
}
