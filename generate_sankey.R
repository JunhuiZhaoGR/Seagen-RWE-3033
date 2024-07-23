

## generate sankey chart for LOT table ------------------------------------------------------------

## non-interactive way ----------------------------------------------------------------------------
generate_sankey_freq <- function(
  dataframe,
  id = NULL,
  linename = NULL,
  linenumber = NULL,
  label_threshold = NULL
) {
  
  # messages ------------------------------------------------------------
  message(paste0(
    "Please manually validate the numbers - this function is unvalidated."
  ))
  
  # check input ------------------------------------------------------------
  # is in dataframe
  if (!id %in% names(dataframe)) stop("id not in dataframe")
  if (!linename %in% names(dataframe)) stop("linename not in dataframe")
  if (!linenumber %in% names(dataframe)) stop("linenumber not in dataframe")
  
  # start ------------------------------------------------------------
  
  # dataset to work with
  temp_data <- dataframe %>%
    # cut dataset
    select(id, linename, linenumber) %>%
    # rename to make programming simple!
    rename(
      "id" = id,
      "linename" = linename,
      "linenumber" = linenumber
    )
  
  # summary count N by line number and by line number, line name level
  temp_data_summary <- temp_data %>%
    group_by(linenumber) %>%
    mutate(n_total_line = length(unique(id))) %>%
    group_by(linenumber, linename) %>%
    mutate(n_line_name = length(unique(id))) %>%
    ungroup() 
  
  # add percent and labels
  temp_data_output <- temp_data_summary %>%
    mutate(
      freq = 1,
      percent = paste0(round(n_line_name / n_total_line, 4) *100, '%'),
      label_list = ifelse(n_line_name / n_total_line > label_threshold, 
                          paste0(linename, '\n', percent), 
                          percent)
    )
  
  # generate sankey plot
  ggplot(temp_data_output,
         aes(x = linenumber, 
             stratum = linename, 
             alluvium = id,
             y = freq,
             fill = linename, 
             label = label_list)
         ) +
    scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum", size = 3) +
    labs(
      x = 'Line Number',
      y = 'N Patients'
    ) +
    guides(fill = guide_legend(title = "Regimens")) +
    theme_classic() +
    theme(legend.position = "bottom")
  
}





## interactive way --------------------------------------------------------------------------------
generate_sankey_line <- function(
  dataframe,
  id = NULL,
  linename = NULL,
  linenumber = NULL,
  return_data = F,
  postfix = NULL
) {
  
  # define so that within function calls don't create error
  n <- NULL
  line <- NULL
  . <- NULL
  linename_derived <- NULL
  linenumber_derived <- NULL
  nextline <- NULL
  id_factor <- NULL
  id_num <- NULL
  
  # messages ------------------------------------------------------------
  message(paste0(
    "Please manually validate the numbers - this function is unvalidated."
  ))
  
  # check input ------------------------------------------------------------
  # is character
  if (!is.character(id)) stop("id must be a character value")
  if (!is.character(linename)) stop("linename must be character value")
  if (!is.character(linenumber)) stop("linenumber must be character value")
  if (!is.character(postfix)) stop("postfix must be character")
  
  # is in dataframe
  if (!id %in% names(dataframe)) stop("id not in dataframe")
  if (!linename %in% names(dataframe)) stop("linename not in dataframe")
  if (!linenumber %in% names(dataframe)) stop("linenumber not in dataframe")
  
  # start ------------------------------------------------------------
  
  # dataset to work with
  temp_data <- dataframe %>%
    # cut dataset
    select(id, linename, linenumber) %>%
    # rename to make programming simple!
    rename(
      "id" = id,
      "linename" = linename,
      "linenumber" = linenumber
    )
  
  # constants
  # max lines to go through
  minlines <- min(temp_data$linenumber, na.rm = T)
  maxlines <- max(temp_data$linenumber, na.rm = T)
  
  # total pts
  total_pts=temp_data %>%
    mutate(total_pts=n_distinct(id)) %>%
    distinct(total_pts)
  
  # find the n_common most common treatments in line 1 to n
  # placeholder
  temp_mostcommon <- NULL
  # fill it
  for (i in minlines:maxlines) {
    temp2 <- temp_data %>%
      dplyr::filter(linenumber == i) %>%
      dplyr::group_by(linename) %>%
      dplyr::summarise(
        n = n()
      ) %>%
      dplyr::mutate(line = i)
    # merge
    temp_mostcommon <- rbind(
      temp2,
      temp_mostcommon
    )
  }
  # scrub formatting and order
  temp_mostcommon <- temp_mostcommon %>%
    dplyr::arrange(line,n) %>%
    as.data.frame()
  
  # build a dataset of source, target and n!
  # recode into other if not of interest
  temp_data2 <- NULL
  for (i in minlines:maxlines) {
    # what is most common in this line?
    temp_line <- temp_mostcommon %>%
      dplyr::filter(line == i) %>%
      .$linename
    # recode
    temp2 <- temp_data %>%
      dplyr::filter(linenumber == i) %>%
      dplyr::mutate(
        linename = dplyr::if_else(
          linename %in% temp_line,
          linename,
          "Other Tx",
          missing = "No recorded"
        )
      )
    # left join onto original to find missing
    temp2 <- dplyr::left_join(
      unique(temp_data["id"]),
      temp2,
      by = "id")
    temp2$linename_derived <- dplyr::if_else(
      !is.na(temp2$linename),
      paste0(temp2$linename," ",i,"L"),
      paste0("No recorded ",i,"L")
    )
    # new to check the old is the same
    temp2$linenumber_derived <- i
    
    # merge
    temp_data2 <- rbind(
      temp2,
      temp_data2
    )
  }
  
  # in each line, what's the next line?
  temp_data2 <- temp_data2 %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      nextline = dplyr::lead(linename_derived, order_by = linenumber_derived)
    ) %>%
    arrange(id, linenumber_derived)
  
  node_n_df=temp_data2 %>%
    distinct(id,linename_derived) %>%
    group_by(linename_derived) %>%
    mutate(
      node_n=n()
    ) %>%
    distinct(linename_derived,node_n) %>%
    ungroup()
  
  # get count of each combo
  temp_data2 <- temp_data2 %>%
    dplyr::group_by(linename_derived,nextline) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::rename(
      source = linename_derived,
      target = nextline,
      value = n
    )
  
  # remove if line dead
  temp_data2 <- stats::na.omit(temp_data2)
  
  # add transition line labels
  temp_data2=temp_data2 %>%
    group_by(source) %>%
    mutate(
      node_sum=sum(value),
      transition_label=paste0(
        value,
        "/",
        sum(value),
        " (",
        round(value/sum(value) * 100),
        "%)"
      )
    ) %>%
    ungroup()
  
  # nodes (Tx)
  nodes <- data.frame(
    id_char = unique(c(temp_data2$source, temp_data2$target)),
    id_factor = as.factor(unique(c(temp_data2$source, temp_data2$target))),
    stringsAsFactors = F
  ) %>%
    dplyr::mutate(
      id_num = as.numeric(id_factor) - 1
    ) %>%
    # arrange, as order is used to match!
    dplyr::arrange(
      id_num
    ) %>% as.data.frame() %>%
    # change colours
    mutate(
      colour = dplyr::case_when(
        grepl("No recorded ",.$id_char) ~ "rgb(165,42,42)",
        grepl("Other Tx ",.$id_char) ~ "rgb(138,43,226)",
        TRUE ~ "rgb(30,144,255)"
      )
    ) %>%
    # add labels
    mutate(
      linenumber=case_when(
        grepl("[0-9]",id_char) ~ as.numeric(stringr::str_extract(id_char, "\\d+"))
      )
    ) %>% 
    left_join(
      temp_data %>%
        group_by(linenumber) %>%
        mutate(
          total_line_n=n_distinct(id)
        )  %>%
        distinct(total_line_n) %>%
        ungroup()
    ) %>%
    left_join(
      node_n_df %>%
        rename(id_char=linename_derived)
    ) %>%
    mutate(
      total_line_n=case_when(
        grepl("No recorded",id_char) ~total_pts$total_pts,
        TRUE ~ total_line_n
      )
    ) %>%
    mutate(
      node_label=paste0(
        node_n,
        "/",
        total_line_n,
        " (",
        round(node_n/total_line_n * 100),
        "%)"
      )
    )
  
  # give numeric edges
  # will be referenced against nodes, so order crucial
  edges <- dplyr::left_join(
    temp_data2,
    nodes[c("id_char","id_num")],
    by = c("source" = "id_char")
  ) %>% dplyr::rename(
    idnum_source = id_num
  )  %>% as.data.frame()
  
  edges <- dplyr::left_join(
    edges,
    nodes[c("id_char","id_num")],
    by = c("target" = "id_char")
  ) %>% dplyr::rename(
    idnum_target = id_num
  ) %>% as.data.frame()
  
  if (return_data == T) {
    return(list(edges = edges,nodes = nodes))
  }
  
  # make the plot
  plotly::plot_ly(
    type = "sankey",
    domain = list(
      x =  c(0,1),
      y =  c(0,1)
    ),
    orientation = "h",
    valueformat = ".0f",
    valuesuffix = postfix,
    
    node = list(
      label = paste0(
        nodes$id_char," [",
        # " \n", 
        # "Treatment Line, N (%): ",
        nodes$node_label,
        "]"
      ),
      color = nodes$colour,
      pad = 15,
      thickness = 10,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = edges$idnum_source,
      target = edges$idnum_target,
      value =  edges$value,
      label =  paste0('Transition, N (%): ',
                      edges$transition_label,
                      "\n",
                      "Total Population, N (%): ",
                      paste0(edges$value,"/",total_pts$total_pts," (",round(edges$value/total_pts$total_pts * 100),"%)")
      )
    )
  ) %>% plotly::config(displayModeBar = F)
  
}



