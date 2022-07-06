
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