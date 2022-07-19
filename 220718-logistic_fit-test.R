library(tidyverse)
load("data/ko7-img3.RData")


# Isolate example ---------------------------------------------------------

# dat7_3 <- datca %>% 
#   ungroup() %>% 
#   filter(img_id == 3) %>% 
#   select(first_peak, n_cells_norm_cum_activity)
# 
# save(dat7_3, file = "data/ko7-img3.RData")

dat7_4 <- datca %>%
  ungroup() %>%
  filter(img_id == 25) %>%
  select(first_peak, n_cells_norm_cum_activity)

# save(dat7_4, file = "data/ko7-img4.RData")

# Logistic function -------------------------------------------------------
## https://en.wikipedia.org/wiki/Logistic_function

logistic_f <- function(x, b, c, l = 1) {
  # b: slope
  # c: horizontal shift = value of sigmoid midpoint
  # l: the curve's max. value
  # (intercept=0 -> it cancels out)
  
  l/(1+exp(-b*(x-c)))
}


# Fit logistic function ---------------------------------------------------

plot(c(0,250), c(0, 1), "n",
     xlab = "Time [s]", 
     ylab = "Cumul. activity [%]")
points(dat7_4, col = "gray")

dat3 <- tibble(x = seq(0, 250, 1))
dat3$y <- logistic_f(dat3$x, 0.035, 140)

lines(dat3)

## Find threshold
t_thresh <- dat3[which.min(abs(dat3$y - 0.5)), ]
t_thresh
points(t_thresh, col = "black", pch = 16)
title("A  manual logistic fit predicts a \nthreshold at 65% strain")


# Automated fit -----------------------------------------------------------

## Isolate up to steepest point of the curve
dat_af <- rename(dat7_4, t = "first_peak", act = "n_cells_norm_cum_activity")

steepest <- dat_af[which.max(diff(dat_af$act, 12)), ]
dat_steep <- filter(dat_af, t < steepest$t)

## Fit model
mod <- nls(act ~ logistic_f(t, b, c),
           dat_steep,
           list(b = 0.05, c = 130))

thresh_pred <- round((coef(mod)["c"]-10)/2, 1)
paste0("Predicted threshold: ", thresh_pred, "%")

## Predict curve
dat_pre <- tibble(
  t = 0:250,
  act = predict(mod, newdata = data.frame(t = t))
)

## Plot 
plot(c(0,250), c(0, 1), "n",
     xlab = "Time [s]", 
     ylab = "Cumul. activity [%]")
points(dat_af, col = "gray")

points(dat_steep, col = "red")

lines(dat_pre, col = "blue")
points(dat_pre[which.min(abs(dat_pre$act - 0.5)), ], col = "blue", pch = 16)
title(paste0("A automatic logistic fit predicts a \nthreshold at ", thresh_pred, "% strain"))


# More robust endpoint identification -------------------------------------

# ## Attempt 1: Partial fit ----
# ### Logistic
# mod1 <- nls(act ~ logistic_f(t, b, c, l = max(act)),
#             dat_af,
#             list(b = 0.1, c = 130))
# # -> Problem: this is not actually a sigmoid curve - it's the beginning of it,
# #             then it ends abruptly (because the stretch stops), then there
# #             are some straggler signals
# 
# ### Quadratic
# mod1 <- lm(act~I(t^2), dat_af)
# # -> Problem: straggler signals (after the stretch is over) result in a skewed 
# #             fit towards the right

# ## Predict new data for partial fit
# dat_1 <- tibble(
#   t = 0:250,
#   act = predict(mod1, newdata = data.frame(t = t))
# )
# 
# ## Plot partial fit
# plot(c(0,250), c(0, 1), "n",
#      xlab = "Time [s]", 
#      ylab = "Cumul. activity [%]")
# points(dat_af, col = "gray")
# lines(dat_1, col = "blue")
# 
# ## Isolate up to the "steepest" point
# max_slope <- dat_1[which.max(diff(dat_1$act)), ]
# points(max_slope, col = "blue", pch = 16)
# dat_1_steep <- dat_1 %>% filter(t <= max_slope$t)


# ### Attemp 2: Straggler elimination ----
# #### Fit loess
# mod_l <- loess(act~t, dat_af, span = 0.5)
# dat_l <- tibble(
#   t = 0:250,
#   act = predict(mod_l, newdata = data.frame(t = t))
# )
# 
# #### Plot loess fit
# plot(c(0,250), c(0, 1), "n",
#      xlab = "Time [s]", 
#      ylab = "Cumul. activity [%]")
# points(dat_af, col = "gray")
# lines(dat_l, col = "blue")
# 
# #### Get slope 
# slope_lag <- 5
# dat_l_slope <- diff(dat_l$act, slope_lag)/diff(dat_l$t, slope_lag)
# lines(slope_lag:250, dat_l_slope*10)
# 
# #### Find timepoint at max. slope
# t_max <- as.integer(names(dat_l_slope[which.max(dat_l_slope)]))
# t_max
# 
# #### Remove stragglers
# dat_1_steep <- filter(dat_l, t <= t_max)


### Attempt 3: Just cut at 110sec ----

#### Remove stragglers by cutting at 110sec
dat_1_steep <- filter(dat_af, t <= 110)

nrow(dat_1_steep)
## Fit logistic curve on partial curve
mod11 <- nls(act ~ logistic_f(t, b, c),
             dat_1_steep,
             # weights = c(rep(50,3), rep(1,25)),
             list(b = 0.05, c = 130))

thresh_pred_dat1steep <- round((coef(mod11)["c"]-10)/2, 1)

dat_1_pre <- tibble(
  t = 0:250,
  act = predict(mod11, newdata = data.frame(t = t))
)

## Plot logistic fit
plot(c(0,250), c(0, 1), "n",
     xlab = "Time [s]", 
     ylab = "Cumul. activity [%]")
points(dat_af, col = "gray")
points(dat_1_steep, col = "pink")

lines(dat_1_pre, col = "blue")
points(dat_1_pre[which.min(abs(dat_1_pre$act - 0.5)), ], col = "blue", pch = 16)
title(paste0("A automatic logistic fit predicts a \nthreshold at ", 
             thresh_pred_dat1steep, "% strain"))



# Full automation ---------------------------------------------------------

dat_auto <- datca %>% ungroup() %>% group_nest(condition, img_id)

get_thresh <- function(.df, .bl = 10, .strain_rate = 0.5) {
  dat_af <- select(.df, first_peak, n_cells_norm_cum_activity)
  
  #### Remove stragglers by cutting at 110sec
  dat_steep <- filter(dat_af, first_peak <= 110)
  
  
  ## Fit logistic curve on partial curve
  mod <- nls(n_cells_norm_cum_activity ~ logistic_f(first_peak, b, c),
               dat_steep,
               list(b = 0.05, c = 130))
  
  # Predict threshold
  round((coef(mod)["c"]-.bl)*.strain_rate, 1)

}

dat_thresh <- dat_auto %>% mutate(thresh = map_dbl(data, get_thresh))

ggplot(dat_thresh, aes(condition, thresh, group = condition, color = condition)) +
  stat_summary(geom = "hpline", show.legend = FALSE, alpha = 0.7, width = 0.8) +
  geom_point(show.legend = FALSE) +
  # geom_text(aes(label = sample), show.legend = FALSE, nudge_x = 0.2, cex = 3) +
  theme_classic() +
  scale_color_manual(values = cc) + 
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9)) +
  labs(x = element_blank(), y = "Threshold [%]")
  # scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = c(0, 2))


dat_act %>% group_by(condition) %>% 
  summarise(n = n(),
            mean = mean(n_cells_norm_cum_activity)*100,
            sd = sd(n_cells_norm_cum_activity))

dat_thresh %>% group_by(condition) %>% 
  summarise(n = n(),
            mean = mean(thresh),
            sd = sd(thresh))


# Conclusion --------------------------------------------------------------

# -> Works well for samples with >10% max. activity but gives huge variation
#    for samples below...