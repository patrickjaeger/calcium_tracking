library(tidyverse)
source("SCA/30_signal_analysis.R")

load("data/220707_LOX_KO_1245.RData")

# TODO signal amplitude, duration, influx velocity


# Classify active/inactive cells ------------------------------------------
## How to set the activation threshold?

datc <- classify_signals(dat1245)
datca <- cum_activity(datc)

geom_stretch <- function() {
  dat_line <- tibble(x = 10:60, y )
}

ggplot(datca, aes(first_peak, 
                  n_cells_norm_cum_activity, 
                  group = interaction(dataset, img_id), 
                  color = condition)) +
  geom_line() +
  labs(x = 'Time [s]', y = 'Active cells [%]', color = '',
       title = 'Cumulative calcium signals at 0.5%/s to 50% max. strain') +
  theme_bw() + 
  geom_line(data = tibble(x = 10:60, y = rep(-0.02, 51)),
            aes(x, y, group = 1), color = "gray60", size = 2) +
  geom_text(data = tibble(x = 68, y = -0.015, t = "stretch"), 
            aes(x, y, label = t, group = 1), color = "gray50") +
  theme(legend.position = 'top')


find_threshold <- function(.dat, .strain_rate = 0.5) {
  
  .dat %>% 
    group_by(dataset, condition, img_id) %>% 
    filter(n_cells_norm_cum_activity >= 0.495) %>% 
    arrange(n_cells_norm_cum_activity) %>%  # what is this?
    slice(1) %>% 
    mutate(strain = (first_peak-10)*.strain_rate)
}
datc %>% slice(1) %>% find_threshold()
