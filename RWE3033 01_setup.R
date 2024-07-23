
## load packages
library('RODBC')
library(DBI)
library(RODBCDBI)
library(tidyverse)
library(glue)
library(readxl)
library(lubridate)
library(stringr)
library(gtsummary)
library(survival)
library(broom)
library(survminer, quietly = T)
library(dplyr)
library(dbplyr)
library(odbc)
library(scales)
library(sqldf)
library(openxlsx)
library(data.table)

source('./RWE3033/build_lot.R')
source('./RWE3033/lot_mutaters.R')
# source('./RWE3033/survival functions.R')

# source('./RWE3033/_helper.R')
source('./RWE3033/os_table.R')
# source('./RWE3033/os_figure.R')
# source('./RWE3033/generate_sankey.R')



con <- DBI::dbConnect(odbc::odbc(), "Snowflake_SG", 
                      role = "SG_GENESIS",
                      warehouse = "DEFAULT_WH",
                      uid = "jzhao@seagen.com",
                      pwd = Sys.getenv("PASSWORD"))

snowflake_count <- function(db) {
  dbGetQuery(con,
             glue("SELECT count(distinct patient_id) as pts, count(patient_id) as obs FROM {db}"))
}

drop_snowflake <- function(tbl_name) {
  glue("DROP TABLE IF EXISTS {tbl_name}")
}


snowflake_str <- function(codelist){
  
  paste0("'", codelist, "'", collapse = ',')
  
}

year_month = "202403"

other_cancer_codes = read_xlsx("~/RWE3033/Exclusion Neoplasms SGN-3033 07MAY2024.xlsx", sheet = 2)

other_cancer_dx = snowflake_str(other_cancer_codes %>% select(code) %>% unlist())



generate_region <- function(data, state_var) {
  state_var <- enquo(state_var)
  
  # These are the standard categorizations of States -> Regions
  data %>%
    mutate(
      region = case_when(
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
      
      region = factor(
        region,
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

