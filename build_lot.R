# helpers
source("./RWE3033/lot_mutaters.R")

#' Build line therapies from a few input variables.
#'
#' This function will build line of therapies for patients based on a few
#' parameters to determine how lines will end and the next line starts.
#'  
#' @import dplyr
#' @param dat Data frame with drug / therapies of interest.
#' @param patient_id Variable name that corresponds to the patient ID.
#' @param drug_name Variable name that corresponds to the drugs.
#' @param drug_start_date Variable name that corresponds to the drugs start date.
#' @param drug_end_date Variable name that corresponds to the drugs end date.
#' @param time_window Number of days from the start of a line to consider drugs
#' part of that line. Typically this is something like 28 or 60.
#' @param allowable_gap Number of days from the end of a line to consider drugs
#' part of that line if they occur before the start of a new drug.
#' Typically this is something like 180.
#' @param messages Print out a message for the line number that was processed.
#' @param extend_end_date If TRUE, all line end dates will be extended to
#' one day prior to the start of the next line.
#' @return Returns a dataframe that consists of the patient ID, the line number
#' the line start and end dates, and the regimen for that line.
#' @export

build_lot <- function(dat, patient_id, drug_name,
                      drug_start_date, drug_end_date,
                      time_window = 28, allowable_gap = 180,
                      messages = FALSE, extend_end_date = FALSE) {
  
  
  # Check that variables exist in the data frame
  if (!deparse(substitute(patient_id)) %in% names(dat)) {
    stop("patient_id variable does not exist in the supplied data frame")
  }
  if (!deparse(substitute(drug_name)) %in% names(dat)) {
    stop("drug_name variable does not exist in the supplied data frame")
  }
  if (!deparse(substitute(drug_start_date)) %in% names(dat)) {
    stop("drug_start_date variable does not exist in the supplied data frame")
  }
  if (!deparse(substitute(drug_end_date)) %in% names(dat)) {
    stop("drug_end_date variable does not exist in the supplied data frame")
  }
  
  # quote objects to rename variables that will be used in mutate functions
  patient_id <- enquo(patient_id)
  drug_name <- enquo(drug_name)
  drug_start_date <- enquo(drug_start_date)
  drug_end_date <- enquo(drug_end_date)
  
  dat <- dat %>%
    # force the passed in data frame to have all lines initially set to NA
    mutate(line = NA) %>%
    rename(
      PatientID = !!patient_id,
      DrugName = !!drug_name,
      DrugDate = !!drug_start_date,
      EndDate = !!drug_end_date
    )
  
  # check that dates contain no missing values
  if (any(is.na(dat$DrugDate))) {
    stop("drug_start_date contains missing values.")
  }
  
  if (any(is.na(dat$EndDate))) {
    stop("drug_end_date contains missing values.")
  }
  
  # create a list to hold the results
  # this is created to avoid rbind-ing results at each iteration
  MAX_LINES <- 100
  treatment_lines <- vector("list", length = MAX_LINES)
  
  CURRENT_LINE <- 1
  
  while(any(is.na(dat$line))) {
    
    dat <- dat %>%
      mutate_line_start_date(.) %>%
      mutate_line_number(., time_window = time_window, CURRENT_LINE) %>%
      mutate_line_end_date(., CURRENT_LINE) %>%
      mutate_line_regimen(., CURRENT_LINE) %>%
      mutate_next_line_start_date(., CURRENT_LINE) %>%
      mutate_line_extension_end_date(., CURRENT_LINE,
                                     allowable_gap = allowable_gap)
    
    # pull out current line
    current_line <- dat %>%
      filter(line == CURRENT_LINE) %>%
      select(
        PatientID, 
        line,
        start_date = line_start_date, 
        stop_date = line_end_date,
        regimen
      ) %>%
      distinct()
    
    # add the current line details to the list object
    treatment_lines[[CURRENT_LINE]] <- current_line
    
    # reset the data frame
    dat <- dat %>%
      select(-line_start_date, -line_end_date,
             -regimen, -next_line_start_date)
    
    if (messages) {
      message(paste0("Processed line: ", CURRENT_LINE))
    }
    
    # update the line number after each iteration
    CURRENT_LINE <- CURRENT_LINE + 1
    
    # quick check that the number of lines exceeded the max of 100
    if (CURRENT_LINE > MAX_LINES) {
      print("You have exceeded 100 lines of therapy. Stopping and returning.")
      break
    }
  }
  
  # combine all lists
  treatment_lines <- do.call("bind_rows", treatment_lines)
  
  # if the end of lines should be extended to the start of the next line
  if (extend_end_date) {
    treatment_lines <- treatment_lines %>%
      arrange(line) %>%
      group_by(PatientID) %>%
      mutate(
        stop_date = if_else(!is.na(lead(start_date)),
                            lead(start_date) - 1,
                            stop_date)
      ) %>%
      ungroup()
  }
  
  return(treatment_lines)
}
