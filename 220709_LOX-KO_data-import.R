# Import datasets from experiments LOX KO # 1, 2, 4, 5 and tidy up and
# combine them into one dataset

# Packages ----------------------------------------------------------------

library(tidyverse)
source("SCA/10_import_raw_data.R")
source("SCA/20_tidy_data.R")


# Tidy raw data -----------------------------------------------------------

out_path <- "data/220705_LOX_KO_data_ALL/"
# dir.create(out_path)

raw_data <- list(
  "D:/iMic/220704_pat_TC_test1/videos/res",
  "D:/iMic/220708_pat_ko6/res"
)

tidy_data_filenames <- list(
  "ko5tc.csv",
  "ko6.csv"
)

full_out_path <- paste0(out_path, tidy_data_filenames)

for (i in 1:length(raw_data)) {
  path <- raw_data[[i]]
  filename <- full_out_path[[i]]

  discard_trash_and_save_tidy_data(path, filename, .bl = 10, .fps = 4)
  print(paste("file", i, "saved"))
}


# Import tidy data --------------------------------------------------------

tidy_files <- list.files("data/220705_LOX_KO_data_ALL/", 
                         pattern = ".csv",
                         full.names = TRUE) %>% 
  .[str_detect(., c("ko5tc", "ko6"))]

dat <- tibble(dataset = str_remove(tidy_data_filenames, ".csv"))
dat$data <- map(tidy_files, read_csv)

# Renest data and tidy up names
unnest(dat, data) %>% pluck("condition") %>% unique()

datt <- dat %>%
  unnest(data) %>%
  group_nest(across(1:10)) %>%
  tidy_conditions()

print(datt, n = nrow(datt))

# Condition overview
datt %>% 
  group_by(dataset, condition) %>% 
  summarise(n = n())


# Discard short tracks ----------------------------------------------------

## Overview ----

# Find lengths of tracks (for each cell)
dat_l <- datt %>% 
  unnest(data) %>% 
  group_nest(dataset, img_id, cell_id) %>% 
  mutate(l = map_int(data, ~dim(.)[[1]]))


# How are the track lengths distributed?
dat_l %>% filter(dataset == "ko5tc", img_id %in% c(1, 14)) %>% 
ggplot(aes(l, group = dataset)) +
  geom_freqpoly() +
  facet_wrap(~dataset)


## Find most prevalent track length (per sample) ----


dat_l2 <- dat_l %>% 
  select(-data) %>% 
  group_by(dataset, img_id) %>% 
  summarise(most = find_most_prevalent_length(l))

dat_l2 %>% print(n= nrow(.))


## Merge data with sample track lengths ----

dat2 <- dat_l %>% 
  unnest(data) %>% 
  group_nest(dataset, img_id) %>% 
  full_join(., dat_l2)


## Discard invalid track lengths ----

dat_f <- dat2 %>% 
  mutate(data, data = map2(data, 
                           most, 
                           ~filter(.x, l %in% get_valid_lengths(.y)) %>% 
                             select(-l)
  ))


# Normalize signal --------------------------------------------------------

## Assign FPS ----
dat_f$fps <- 4

## Remove sample 1 because it's weird
dat_f <- dat_f[-1, ]

## Normalize calcium signal and convert frame to time ----
dat <- dat_f %>% 
  select(-most) %>% 
  unnest(data) %>% 
  group_nest(dataset, date, img_id, donor, passage, 
             day, condition, strain_rate, max_strain, sample, cell_id, fps) %>% 
  mutate(data = map2(data, fps, ~reformat_signal(.x, .bl = 10, .fps = .y)))

# Renest data into samples ----
dat <- dat %>% 
  select(-fps) %>% 
  unnest(data) %>% 
  group_nest(dataset, date, img_id, donor, passage, 
             day, condition, strain_rate, max_strain, sample)


## Save data ----
dat_5.1_6 <- dat
save(dat_5.1_6, file = "data/220709_LOX_KO_5.1_6.RData")


# Load data ---------------------------------------------------------------

# load("data/220707_LOX_KO_1245.RData")

