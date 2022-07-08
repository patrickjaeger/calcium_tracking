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






cum_activity <- function(.dat) {
  # Add the cumulative cell activity to the data
  .dat %>% 
    mutate(n_cells = map_int(data, ~n_distinct(.$cell_id))) %>% 
    unnest(data) %>% 
    filter(first_peak != Inf) %>%
    group_by(dataset, condition, img_id, cell_id, n_cells) %>% 
    slice(1) %>% 
    # ungroup() %>% 
    group_by(dataset, condition, img_id, first_peak, n_cells) %>% 
    summarise(n_active = n()) %>% 
    group_by(dataset, condition, img_id) %>% 
    mutate(cum_activity = cumsum(n_active),
           norm_cum_activity = cum_activity/max(cum_activity),
           n_cells_norm_cum_activity = cum_activity/max(n_cells))
}

datc <- cum_activity(dat)

ggplot(datc, aes(first_peak, 
                 n_cells_norm_cum_activity, 
                 group = interaction(dataset, img_id), 
                 color = condition)) +
  geom_line() +
  geom_vline(xintercept = c(10, 210), lty = 3) +
  # scale_color_manual(values = c('black', 'skyblue')) +
  labs(x = 'Time [s]', y = 'Active cells [%]', color = '',
       title = 'Cumulative calcium signals at 0.5%/s to 50% max. strain') +
  theme_bw() + 
  theme(legend.position = 'top')


find_threshold <- function(.dat) {
  
}