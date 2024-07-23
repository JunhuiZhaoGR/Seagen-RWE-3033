#' Add the minimum date for patients records missing a line number
#'
#' This function adds the minimum date of drug / therapy records
#' that have missing values for line number. The line_start_date
#' variable will be mutated back to a data frame.
#' @import dplyr
#' @param dat Data frame to be mutated.
#' @return Returns the provided data frame with a new variable: line_start_date
#' @export

mutate_line_start_date <- function(dat) {
  
  line_start_dates <- dat %>%
    # only keep lines that are missing a line number
    filter(is.na(line)) %>%
    # get the first drug date to determine the start of the line
    arrange(DrugDate) %>%
    group_by(PatientID) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(PatientID, line_start_date = DrugDate) %>%
    distinct()
  
  # return the incoming data frame with the start dates
  dat %>% left_join(line_start_dates, by = "PatientID")
}

#' Updates the line number if records occur within a certain time window
#' of the line starting date.
#'
#' This function updates the line number to the current line value for 
#' all records that occur within a time window of the line starting date
#' @import dplyr
#' @param dat Data frame to be mutated.
#' @param  time_window The number of days after the start date that should
#' be used to determine if a record belongs in that line.
#' @param current_line The line number that should be used to update the line
#' @return Returns the provided data frame with the line variable updated.
#' @export

# TODO: Better naming for this function?
mutate_line_number <- function(dat, time_window, current_line) {
  
  dat %>%
    # set the line number for any records that do not already belong to a line
    # and occur within the time window provided 
    mutate(
      line = case_when(
        is.na(line) & DrugName %in% c('abemaciclib', 'palbociclib', 'ribociclib') &
          DrugDate <= line_start_date + 60 ~ current_line,
        is.na(line) & 
          DrugDate <= line_start_date + time_window ~ current_line,
        TRUE ~ as.numeric(line)
      )
    ) 
}

#' Add the maximum date for patients records during specific line
#'
#' This function adds the maximum date of drug / therapy records
#' that occur during a certain line number. The line_end_date
#' variable will be mutated back to a data frame.
#' @import dplyr
#' @param dat Data frame to be mutated.
#' @param current_line The line number of interest. If missing, records with a 
#' missing line will be used.
#' @return Returns the provided data frame with a new variable: line_end_date
#' @export

mutate_line_end_date <- function(dat, current_line = NA) {
  
  if (is.na(current_line)) {
    filter_criteria <- rlang::parse_expr("is.na(line)")
  } else {
    # only keep lines that part of the current line
    filter_criteria <- rlang::parse_expr(paste0("line == ", current_line))
  }
  
  line_end_dates <- dat %>%
    filter(!!filter_criteria) %>%
    # get the first drug date to determine the start of the line
    arrange(desc(EndDate)) %>%
    group_by(PatientID) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(PatientID, line_end_date = EndDate) %>%
    distinct()
  
  # return the incoming data frame with the end dates
  dat %>% left_join(line_end_dates, by = "PatientID")
}

#' Add the unique drugs that appear in a specific line
#'
#' This function adds the all drugs / therapies that occur
#' during a certain line number. The regimen variable, a comma separated string
#' of unique drug names, will be mutated back to a data frame.
#' @import dplyr
#' @param dat Data frame to be mutated.
#' @param current_line The line number of interest.
#' @return Returns the provided data frame with a new variable: regimen
#' @export

mutate_line_regimen <- function(dat, current_line) {
  
  line_regimens <- dat %>%
    filter(line == current_line) %>%
    select(PatientID, DrugName) %>%
    distinct() %>%
    arrange(DrugName) %>%
    group_by(PatientID) %>%
    summarise(regimen = paste0(DrugName, collapse = ", "))
  
  # return the incoming data frame with the drug regimen
  dat %>% left_join(line_regimens, by = "PatientID")
}

#' Add the minimum date for drugs that is not in the regimen of a specific line
#'
#' This function adds the all drugs / therapies that occur
#' during a certain line number. The regimen variable, a comma separated string
#' of unique drug names, will be mutated back to a data frame.
#' @import dplyr
#' @param dat Data frame to be mutated.
#' @param current_line The line number of interest.
#' @return Returns the provided data frame with a new variable:
#' next_line_start_date
#' @export

mutate_next_line_start_date <- function(dat, current_line) {
  
  next_line_start_dates <- dat %>%
    # only keep lines that are missing a line number
    filter(is.na(line)) %>%
    filter(stringr::str_detect(regimen, stringr::fixed(DrugName), negate = TRUE)) %>%
    select(PatientID, line, DrugDate) %>%
    mutate_line_start_date(.) %>%
    select(PatientID, next_line_start_date = line_start_date) %>%
    distinct()
  
  # return the incoming data frame with the next line start date
  dat %>% left_join(next_line_start_dates, by = "PatientID")
}

# perform line extension if similar drugs occur within 180 days
# of line end date and before next line start date

#' Extends the current line if drugs that occur in the regimen appear within
#' a certain time window from the current end date and are prior to the next
#' line starting date.
#'
#' This function determines if the line end date should be moved into the future
#' because of drugs that occured witihn a certain allowable gap after the 
#' current end date. If a drug from the current line occurs in the regimen
#' then the line end date might be moved back.
#' @import dplyr
#' @param dat Data frame to be mutated.
#' @param current_line The line number of interest.
#' @param  allowable_gap The number of days after the end date that should
#' be used to determine if a record belongs in that line.
#' @return Returns the provided data frame with the line number updated for
#' records that have been extended.
#' @export


# TODO: Better name for this function?
mutate_line_extension_end_date <- function(dat, current_line, allowable_gap) {
  
  get_extended_max_date <- function(drug_dates, end_dates,
                                    current_end_date, next_start_date,
                                    allowable_gap = allowable_gap) {
    
    # get the individual end dates and next start dates
    # this will be by patient when used with group by
    current_end_date <- unique(current_end_date)
    next_start_date <- unique(next_start_date)
    # new_end_date <- NA
    
    while(TRUE) {
      # if this is the last line, thatn is, both current end and next start
      # are missing, than break out of the loop
      if (is.na(current_end_date) & is.na(next_start_date)) break
      
      if (is.na(next_start_date)) {
        # if the patient is missing the next start date for a new drug
        # we only want to check if there are other drugs within the time window
        new_end_date <- max(
          # TODO: <= allowable gap?
          end_dates[drug_dates < current_end_date + allowable_gap]
        )
        
      } else {
        # otherwise we will check which drugs occur before the next start date
        new_end_date <- max(
          # TODO: <= allowable gap?
          end_dates[drug_dates < current_end_date + allowable_gap &
                      drug_dates < next_start_date]
        )
      }
      
      # if they do not match, update the current end date to be the new 
      # end date, and repeat
      if (current_end_date == new_end_date) {
        break
      } else {
        current_end_date <- new_end_date
      }
    }
    
    # return the end date
    return(current_end_date)
  }
  
  # apply the max function to each patient
  new_line_end <- dat %>%
    # only keep records in the current line or missing a line
    filter(line == current_line | is.na(line)) %>%
    # only keep records of drugs that occur in the regimen
    filter(stringr::str_detect(regimen, stringr::fixed(DrugName))) %>%
    group_by(PatientID) %>%
    summarise(
      new_line_end = get_extended_max_date(DrugDate, EndDate,
                                           line_end_date, next_line_start_date,
                                           allowable_gap = allowable_gap)
    )
  
  # if a patient does not have a new line end, then use the current line end
  dat <- dat %>%
    left_join(new_line_end, by = "PatientID") %>%
    # this replaces the end date for those with no more lines
    mutate(line_end_date = if_else(!is.na(new_line_end),
                                   new_line_end, line_end_date)) %>%
    select(-new_line_end)
  
  # update the line number if the record occurs prior to the new line end
  # and the drug is in the regimen
  dat %>%
    # if the line end date is the same as the next line start date or
    # it occurs after the next line start then
    # we will end the line one day prior to next start date
    mutate(line_end_date = if_else(!is.na(next_line_start_date) & 
                                     (line_end_date == next_line_start_date |
                                        line_end_date > next_line_start_date),
                                   next_line_start_date - 1, line_end_date)) %>%
    # update all drug records that occur prior to extended line end date
    mutate(
      line = case_when(
        stringr::str_detect(regimen, stringr::fixed(DrugName)) &
          DrugDate <= line_end_date ~ current_line,
        TRUE ~ line)
    )
  
}
