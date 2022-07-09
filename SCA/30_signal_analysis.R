
classify_signals <- function(.dat, .thresh = 1.1) {
  # Classify the cells into active/inactive based on whether their signal
  # intensity surpasses the threshold set and find the time at which the
  # first datapoint surpassed the threshold
  # .dat (tbl): data nested by sample
  # .thresh (dbl): threshold for active/inactive classification
  
  # Find the position of the first datapoint over the threshold
  first_point <- .dat %>% 
    # Renest into single cells
    unnest(data) %>% 
    group_nest(across(1:11)) %>% 
    
    # Find the first datapoint that surpasses the threshold
    mutate(first_peak = map_dbl(data, ~min(which(.$calcium > .thresh)))) %>%
    suppressWarnings() %>% 
    mutate(active = if_else(first_peak == Inf, 0, 1))
  
  # Extract the time of the first peak
  extract_time <- function(.cell, .first_peak) {
    # Extract the time of the first time of the first peak or return
    # NA if there is no peak
    
    if (is.infinite(.first_peak)) return(NA)
    pluck(.cell, "time", .first_peak)
  }
  
  first_point$first_peak <- map2_dbl(first_point$data,
                                     first_point$first_peak,
                                     extract_time)
    
  # Renest data into samples
  first_point %>% 
    relocate(active, first_peak, .before = data) %>% 
    unnest(data) %>% 
    group_nest(across(1:10))
}


cum_activity <- function(.dat) {
  # Calculate the cumulative cell activity and count the cells
  # .dat (tbl): data nested by sample (output from cassify_signals())
  
  .dat %>% 
    # Count cells
    mutate(n_cells = map_int(data, 
                             ~pluck(., "cell_id") %>% 
                               n_distinct()
                             )
           ) %>% 
    unnest(data) %>% 
    
    # Remove inactive cells
    filter(first_peak != Inf) %>%
    
    # Retain only one row per cell; we only need the first_peak value
    group_by(dataset, condition, img_id, cell_id, n_cells) %>% 
    slice(1) %>% 
    
    # Count the number of active cells per timepoint (that has active cells)
    group_by(dataset, condition, img_id, first_peak, n_cells) %>% 
    summarise(n_active = n(), .groups = "drop") %>% 
    
    # Calculate the cumulative cell activity
    group_by(dataset, condition, img_id) %>% 
    mutate(cum_activity = cumsum(n_active),
           norm_cum_activity = cum_activity/max(cum_activity),
           n_cells_norm_cum_activity = cum_activity/max(n_cells))
}
