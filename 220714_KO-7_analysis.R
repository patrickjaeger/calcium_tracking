library(tidyverse)
source("SCA/30_signal_analysis.R")
source("SCA/geom_hpline.R")

load("data/220714_LOX_KO_7.RData")
dat <- dat_7
rm(dat_7)


# Remove broken samples ---------------------------------------------------
# dat <- filter(dat, dataset != "ko5tc" & strain_rate != 1)


# Classify active/inactive cells ------------------------------------------
## How to set the activation threshold?

unique(dat$condition)
dat$condition <- as_factor(dat$condition) %>%
  fct_relevel(
    "noTarget 8k",
    "LOXL2 KO 8k",
    "noTarget 10k",
    "LOXL2 KO 10k",
    "noTarget 12k",
    "LOXL2 KO 12k"
  )
datc <- classify_signals(dat, .thresh = 1.05)



# Cumulative activity -----------------------------------------------------

datca <- cum_activity(datc)
datt <- find_threshold(datca)


# Total activity ----------------------------------------------------------

dat_act <- datca %>% 
  group_by(condition, img_id) %>% 
  filter(n_cells_norm_cum_activity == max(n_cells_norm_cum_activity)) %>% 
  print(n = nrow(.))

cc <- c("black", "gray40", "darkblue", "blue", "darkred", "red")

ggplot(dat_act, aes(condition, n_cells_norm_cum_activity*100, group = condition, color = condition)) +
  stat_summary(geom = "hpline", show.legend = FALSE, alpha = 0.7, width = 0.8) +
  geom_point(show.legend = FALSE) +
  geom_text(aes(label = sample), show.legend = FALSE, nudge_x = 0.2, cex = 3) +
  theme_classic() +
  scale_color_manual(values = cc) + 
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9)) +
  labs(x = element_blank(), y = "Max. cell activity with 50% strain [%]") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = c(0, 2))

# ggsave("results/220714_ko7_activity.svg", units = "cm", width = 8, height = 8.8)

  
ggplot(dat_act, aes(condition, n_cells, group = condition, color = condition)) +
  stat_summary(geom = "hpline", show.legend = FALSE, alpha = 0.7, width = 0.8) +
  geom_point(show.legend = FALSE) +
  geom_text(aes(label = sample), show.legend = FALSE, nudge_x = 0.1) +
  theme_classic() +
  scale_color_manual(values = cc) + 
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9)) +
  labs(x = element_blank(), y = "Cell count [n]")

ggsave("results/220714_ko7_cell_counts.svg", units = "cm", width = 8, height = 8.8)


ggplot(datca, aes(first_peak, 
                 n_cells_norm_cum_activity*100,
                 group = interaction(dataset, img_id), 
                 color = condition)) +
  geom_line() +
  labs(x = 'Time [s]', y = 'Active cells [%]', color = '',
       title = 'Cumulative calcium signals at 0.5%/s to 50% max. strain') +
  theme_bw() + 
  scale_color_manual(values = cc) + 
  geom_line(data = tibble(x = 10:110, y = rep(-0.02, 101)),
            aes(x, y, group = 1), color = "gray60", size = 2) +
  geom_text(data = tibble(x = 130, y = -0.015, t = "stretch"), 
            aes(x, y, label = t, group = 1), color = "gray50") +
  theme(legend.position = 'top') +
  facet_wrap(~condition)


# Threshold ---------------------------------------------------------------


datp2 <- filter(datt, dataset == target_dataset)
datp2 <- filter(datt, condition %in% target_conditions)

# datp2$condition <- as_factor(datp2$condition) %>%
#   fct_relevel("WT", "noTarget", "LOXL2 KO", "LOXL2 plasmid")
better_colors <- c("black", "grey50", "darkred", "dodgerblue")

p1 <- ggplot(datp2, aes(condition, strain, color = condition)) +
  stat_summary(geom = 'bar', cex = 0.9, alpha = 0.4, show.legend = F, 
               width = 0.7, fill = NA) +
  stat_summary(geom = 'errorbar', width = 0.5, cex = 0.9, show.legend = F) +
  geom_jitter(width = 0.07, show.legend = F, pch = 1, cex = 2) +
  scale_color_manual(values = better_colors) +
  theme_classic() +
  labs(x = NULL, y = 'Activation thresh. in strain [%]') +
  scale_y_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0), breaks = c(0, 50, 100), limits = c(0, 100)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1)) 
# facet_wrap(~dataset)

datt %>% 
  group_by(condition) %>% 
  summarise(n = n(),
            mean_thresh = mean(strain),
            sd_thresh = sd(strain)) %>% print(n = nrow(.))


# Cell count --------------------------------------------------------------

p2 <- ggplot(datp2, aes(condition, n_cells, color = condition)) +
  stat_summary(geom = 'bar', cex = 0.9, alpha = 0.4, show.legend = F, 
               width = 0.7, fill = NA) +
  stat_summary(geom = 'errorbar', width = 0.5, cex = 0.9, show.legend = F) +
  geom_jitter(width = 0.07, show.legend = F, pch = 1, cex = 2) +
  theme_classic() +
  scale_color_manual(values = better_colors) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = '# of cells [n]') +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1))
# facet_wrap(~dataset)


# cor(threshold, n_cells)? ------------------------------------------------

ggpubr::ggarrange(p1,p2)
# 
# dattwt <- filter(datt, condition == "noTarget")
# qplot(dattwt$n_cells, dattwt$strain)
# qplot(datt$n_cells, datt$strain, color = datt$condition)
# cor(dattwt$n_cells, dattwt$strain)


cor(datp2$n_cells, datp2$strain)
qplot(datp2$n_cells, datp2$strain, color = datp2$condition) +
  scale_color_manual(values = better_colors)

