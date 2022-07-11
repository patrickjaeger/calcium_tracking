library(tidyverse)
source("SCA/30_signal_analysis.R")

load("data/220707_LOX_KO_1245.RData")
load("data/220709_LOX_KO_5.1_6.RData")
dat_all <- bind_rows(dat1245, dat_5.1_6)
rm(dat1245, dat_5.1_6)

# TODO Check threshold setting: 1.1?
# TODO signal amplitude, duration, influx velocity


# Classify active/inactive cells ------------------------------------------
## How to set the activation threshold?

datc <- classify_signals(dat_all, .thresh = 1.05)


# Cumulative activity -----------------------------------------------------

datca <- cum_activity(datc)
datt <- find_threshold(datca)


target_dataset <- "ko6"
target_conditions <- c("noTarget", "WT", "LOXL2 KO", "LOXL2 plasmid")

datp1 <- filter(datca, dataset == target_dataset)
datp1 <- filter(datca, condition %in% target_conditions)

ggplot(datp1, aes(first_peak, 
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


# Threshold ---------------------------------------------------------------


datp2 <- filter(datt, dataset == target_dataset)
datp2 <- filter(datt, condition %in% target_conditions)

datp2$condition <- as_factor(datp2$condition) %>%
  fct_relevel("WT", "noTarget", "LOXL2 KO", "LOXL2 plasmid")
better_colors <- c("black", "grey50", "darkred", "dodgerblue")

ggplot(datt, aes(condition, strain, color = condition)) +
  stat_summary(geom = 'bar', cex = 0.9, alpha = 0.4, show.legend = F, 
               width = 0.7, fill = NA) +
  stat_summary(geom = 'errorbar', width = 0.5, cex = 0.9, show.legend = F) +
  geom_jitter(width = 0.07, show.legend = F, pch = 1, cex = 2) +
  # scale_color_manual(values = better_colors) +
  # scale_fill_manual(values = nice_colors) +
  theme_classic() +
  labs(x = NULL, y = 'Activation thresh. in strain [%]') +
  scale_y_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0), breaks = c(0, 50, 100), limits = c(0, 100)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1)) 
  # facet_wrap(~dataset)

datt %>% 
  group_by(condition, img_id) %>% 
  summarise(n = n(),
            mean_thresh = mean(strain),
            sd_thresh = sd(strain)) %>% print(n = nrow(.))


# Cell count --------------------------------------------------------------

ggplot(datp2, aes(condition, n_cells, color = condition)) +
  stat_summary(geom = 'bar', cex = 0.9, alpha = 0.4, show.legend = F, 
               width = 0.7, fill = NA) +
  stat_summary(geom = 'errorbar', width = 0.5, cex = 0.9, show.legend = F) +
  geom_jitter(width = 0.07, show.legend = F, pch = 1, cex = 2) +
  theme_classic() +
  scale_color_manual(values = better_colors) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = '# of cells [n]') +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~dataset)


# cor(threshold, n_cells)? ------------------------------------------------

# ggpubr::ggarrange(p1,p2)
# 
# dattwt <- filter(datt, condition == "noTarget")
# qplot(dattwt$n_cells, dattwt$strain)
# qplot(datt$n_cells, datt$strain, color = datt$condition)
# cor(dattwt$n_cells, dattwt$strain)
