#' codelist_load
#'
#' @param filepath
#' @param sheets
#' @param adtl_cols
#' @param no_dots
#' @param format_check
#' @param dupe_check
#' @param log_limit
#'
#' @return
#' @export
#'
#' @examples

codelist_load <- function(filepath, sheets, adtl_cols=NULL, no_dots=TRUE, format_check=TRUE, dupe_check=TRUE, log_limit=30) {
  
  if (missing(sheets)) {
    sheets = readxl::excel_sheets(filepath)
  }
  
  for (s in sheets) {
    if (s == sheets[1]) {
      df = readxl::read_excel(filepath, sheet=s)
    } else {
      df = dplyr::bind_rows(df, readxl::read_excel(filepath, sheet=s))
    }
  }
  
  cat(crayon::blue(nrow(df), "codes imported from", length(sheets), "sheet(s).\n"))
  
  if (format_check == TRUE) {
    format_check_df = df %>%
      # dplyr::rowwise() %>%
      dplyr::mutate(
        Code_nosym = stringr::str_replace(stringr::str_replace(Code,"-",""),"[.]","")
        # , loinc_chk1 = dplyr::case_when(Code_Type == 'LOINC' ~ stringr::str_split(Code, "-", simplify = TRUE)[1], TRUE ~ "00")
        # , loinc_chk2 = dplyr::case_when(
        #   Code_Type == 'LOINC' & nchar(loinc_chk1) %% 2 == 0 ~ paste0(substr(loinc_chk1,8,8), substr(loinc_chk1,6,6), substr(loinc_chk1,4,4), substr(loinc_chk1,2,2))
        #   , Code_Type == 'LOINC' & nchar(loinc_chk1) %% 2 == 1 ~ paste0(substr(loinc_chk1,7,7), substr(loinc_chk1,5,5), substr(loinc_chk1,3,3), substr(loinc_chk1,1,1))
        # )
        # , loinc_chk3 = dplyr::case_when(Code_Type == 'LOINC' ~ as.character(as.numeric(loinc_chk2)*2))
        # , loinc_chk4 = dplyr::case_when(
        #   Code_Type == 'LOINC' & nchar(loinc_chk1) %% 2 == 1 ~ paste0(substr(loinc_chk1,8,8), substr(loinc_chk1,6,6), substr(loinc_chk1,4,4), substr(loinc_chk1,2,2))
        #   , Code_Type == 'LOINC' & nchar(loinc_chk1) %% 2 == 0 ~ paste0(substr(loinc_chk1,7,7), substr(loinc_chk1,5,5), substr(loinc_chk1,3,3), substr(loinc_chk1,1,1))
        # )
        # , loinc_chk5 = dplyr::case_when(Code_Type == 'LOINC' ~ paste0(loinc_chk4, loinc_chk3))
        # , loinc_chk6 = dplyr::case_when(Code_Type == 'LOINC' ~ sum(as.numeric(stringr::str_split(loinc_chk5, "", simplify = TRUE))))
        # , loinc_chk7 = dplyr::case_when(Code_Type == 'LOINC' ~ ceiling(loinc_chk6/10)*10)
        # , loinc_chk_digit = dplyr::case_when(Code_Type == 'LOINC' ~ loinc_chk7-loinc_chk6)
      ) %>%
      dplyr::filter(
        Code_Type == "ICD-9-CM" & (
          nchar(Code_nosym) > 5
          | grepl("[^0-9^E^V]", substr(Code,1,1))
          | grepl("[^0-9^E^V]",substr(Code,1,1))
          | grepl("[^0-9^.]",substr(Code,2,100))
          | (substr(Code,1,1) != 'E' & grepl("[.]",Code) & stringr::str_locate(Code, "[.]")[1] != 4)
          | (substr(Code,1,1) == 'E' & grepl("[.]",Code) & stringr::str_locate(Code, "[.]")[1] != 5)
          | stringr::str_count(Code, "[.]") > 1
        )
        | Code_Type == "ICD-10-CM" & (
          nchar(Code_nosym) > 7
          | grepl("[^A-Z]",substr(Code,1,1))
          | grepl("[^A-Z^0-9^.]",Code)
          | (grepl("[.]",Code) & stringr::str_locate(Code, "[.]")[1] != 4)
          | stringr::str_count(Code, "[.]") > 1
        )
        | Code_Type %in% c('CPT','HCPCS','CPT/HCPCS') & (
          nchar(Code) != 5
          | grepl("[^0-9^A-Z]",Code)
        )
        | Code_Type == "ICD-9-PCS" & (
          nchar(Code_nosym) < 2
          | nchar(Code_nosym) > 4
          | grepl("[^0-9^.]",Code)
          | (grepl("[.]",Code) & stringr::str_locate(Code, "[.]")[1] != 3)
          | stringr::str_count(Code, "[.]") > 1
        )
        | Code_Type == "ICD-10-PCS" & (
          nchar(Code) != 7
          | grepl("[^0-9^A-Z]",Code)
          
        )
        | Code_Type == "NDC" & (
          nchar(Code_nosym) != 11
          | grepl("[^0-9^-]",Code)
          | (grepl("-",Code) & (substr(Code,6,6) != '-' | substr(Code,11,11) != '-'))
          
        )
        | Code_Type == "DRG" & (
          nchar(Code) > 3
          | grepl("[^0-9]",Code)
          
        )
        | Code_Type == "REVCODE" & (
          nchar(Code) != 4
          | grepl("[^0-9]",Code)
          
        )
        # | Code_Type == "LOINC" & (
        #   nchar(Code_nosym) < 3
        #   | nchar(Code_nosym) > 8
        #   | grepl("[^0-9^-]",Code)
        #   | stringr::str_count(Code, "-") != 1
        #   | nchar(stringr::str_split(Code, "-", simplify = TRUE)[2]) != 1
        #   | stringr::str_split(Code, "-", simplify = TRUE)[2] != loinc_chk_digit
        # )
      ) %>%
      dplyr::select(Code_Type, Code)
    
    if (nrow(format_check_df) > 0) {
      cat(crayon::yellow(paste0('WARNING: ', nrow(format_check_df), ' imported codes do not have expected format of coding system\n')))
      print(format_check_df, n=log_limit)
    } else {
      cat(crayon::blue("Format check completed and found no issues.\n"))
    }
  }
  
  if (no_dots == TRUE) {
    df = dplyr::mutate(df, Code=stringr::str_replace(Code,"[.]",""))
  }
  
  if (dupe_check == TRUE) {
    dupe_check_df = df %>%
      dplyr::filter(0==1) %>%
      dplyr::select(Code_Type, Code)
    for (t in colnames(df)) {
      if (startsWith(t, 'Label')) {
        dupe_check_df = dupe_check_df %>%
          dplyr::bind_rows(
            df %>%
              dplyr::select(all_of(t), Code_Type, Code) %>%
              dplyr::rename(Label = t) %>%
              dplyr::filter(Label != '-' & !is.null(Label))
          )
      }
    }
    
    dupe_check_df = dupe_check_df %>%
      dplyr::group_by(Label, Code_Type, Code) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "keep") %>%
      dplyr::filter(n > 1) %>%
      dplyr::ungroup()
    
    if (nrow(dupe_check_df) > 0) {
      cat(crayon::yellow(paste0('WARNING: ', nrow(dupe_check_df), ' duplicate codes found.\n')))
      print(dupe_check_df, n=log_limit)
    } else {
      cat(crayon::blue("Duplicate code check completed and found no issues.\n"))
    }
  }
  
  return(df %>% dplyr::select(starts_with("Label"), Code_Type, Code, all_of(adtl_cols)))
}

codelist_fetch <- function(var_labels, df, str_format="comma_quoted", code_types) {
  
  if (!(str_format %in% c("comma_quoted","comma_unquoted","space_quoted","space_unquoted","pipe_quoted","pipe_unquoted","likeany_startswith","likeany_contains","rlike_startswith","rlike_contains","vector"))) {
    cat(crayon::red(paste0("ERROR: Invalid value for str_format (", str_format, ")")))
  } 
  else {
    quotes = ifelse(grepl("_unquoted",str_format) | grepl("rlike_",str_format), "", "'")
    
    if (grepl("space_",str_format)) {
      dlm = " "
    } else if (grepl("pipe_",str_format) | grepl("rlike_",str_format)) {
      dlm = "|"
    } else {
      dlm=','
    }
    
    opensym = ifelse(str_format == "likeany_contains", "%", "")
    opensym = ifelse(str_format == "rlike_startswith", "^", opensym)
    closesym = ifelse(str_format %in% c("likeany_startswith","likeany_contains"), "%", "")
    
    if (length(var_labels) == 1) {
      if (var_labels == "_ALL_") {
        var_labels = list()
        for (t in colnames(df)) {
          if (startsWith(t, 'Label')) {
            var_labels = c(var_labels, (df %>% dplyr::select(all_of(t[1])) %>% dplyr::distinct())[[t]])
          }
        }
        var_labels = unique(var_labels[var_labels != "-"])
      }
    }
    
    codes = c()
    for (l in var_labels) {
      cat(crayon::blue("Importing",l,"\n"))
      df2 = dplyr::filter(df, if_any(starts_with("Label"), ~ tolower(.x) == tolower(l)))
      
      if (missing(code_types)) {
        code_types_f = (
          df2 %>% 
            dplyr::filter(if_any(starts_with("Label"), ~ tolower(.x) == tolower(l))) %>%
            dplyr::select(Code_Type) %>%
            dplyr::distinct()
        )$Code_Type
      }
      else {
        code_types_f = code_types
      }
      for (t in code_types_f) {
        if (str_format == "vector") {
          if (length(var_labels) > 1) {
            codes[[l]][[t]] = dplyr::filter(df2, Code_Type == t)$Code
          } else {
            codes[[t]] = dplyr::filter(df2, Code_Type == t)$Code
          }
        } else {
          if (length(var_labels) > 1) {
            codes[[l]][[t]] = paste0(quotes, opensym, dplyr::filter(df2, Code_Type == t)$Code, closesym, quotes, collapse=dlm)
          } else {
            codes[[t]] = paste0(quotes, opensym, dplyr::filter(df2, Code_Type == t)$Code, closesym, quotes, collapse=dlm)
          }
        }
      }
      if (length(var_labels) > 1) {
        print(codes[[l]])
      } else {
        print(codes)
      }
    }
    
    return(codes)
  }
}