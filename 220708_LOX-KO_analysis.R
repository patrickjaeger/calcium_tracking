library(tidyverse)

load("data/220707_LOX_KO_1245.RData")

# TODO signal amplitude, duration, influx velocity



# Classify active/inactive cells ------------------------------------------
## How to set the activation threshold?

classify_signals <- function(.df, .thresh = 1.1) {
  # Extract the time when a cell first surpasses the threshold
  .df %>% 
    unnest(data) %>% 
    group_nest(across(1:11)) %>% 
    mutate(first_peak = map_dbl(data, ~min(which(.$calcium > .thresh)))) %>%
    suppressWarnings() %>%  # Every Inf value gives a warning
    # If the threshold is never reached (Inf), the cell is not active (0)
    # else it's active (1)
    mutate(peak_type = if_else(first_peak == Inf, 0, 1)) %>%
    unnest(data) %>% 
    group_nest(across(1:10))
}


dat <- classify_signals(dat1245)
rm(dat1245)
