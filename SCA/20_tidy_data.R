
tidy_conditions <- function(.df) {
  # Format names of conditions
  data <- .df %>% 
    mutate(condition = toupper(condition)) %>% 
    mutate(condition = case_when(
      condition == "LOX1G1" ~ "LOXL1g1 KO",
      condition == "LOX1G2" ~ "LOXL1g2 KO",
      condition == "LOX3" ~ "LOXL3 KO",
      condition == "VHL" ~ "VHL KO",
      condition == "NOT" ~ "noTarget",
      condition == "WT" ~ "WT",
      condition == "LOXL2" ~ "LOXL2 KO",
      condition == "LOXL2-KO" ~ "LOXL2 KO",
      condition == "LOXL2-KI" ~ "LOXL2 plasmid",
      condition == "LOXL2-KI" ~ "LOXL2 plasmid",
      TRUE ~ "missing"
    ))
  
  if ("missing" %in% data$condition) {
    stop("ERROR: missing conditions")
  } else {
    return(data)
  }
}


reformat_signal <- function(.df, .bl, .fps) {
  # normalize signal and convert frame to time
  # .bl (double): duration of baseline [sec]
  # .fps (double): frames per second
  
  bl_end <- .bl*.fps
  
  bl_mean <- filter(.df, frame < bl_end) %>% 
    pluck("calcium_raw") %>% 
    mean()
  
  .df %>% 
    # group_by(cell_id) %>%
    # na.omit() %>% 
    mutate(calcium = calcium_raw/bl_mean) %>%
    ungroup() %>% 
    rename(time = frame) %>% 
    mutate(time = time/.fps) %>%  # [time] = seconds
    select(-calcium_raw)
}
