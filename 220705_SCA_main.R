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
  "D:/iMic/211205_pat_KO1/res",
  "D:/iMic/211222_pat_ko2/res",
  "D:/iMic/220620_pat_ko4/res",
  "D:/iMic/220704_pat_ko5/res"
)

tidy_data_filenames <- list(
  "ko1.csv",
  "ko2.csv",
  "ko4.csv",
  "ko5.csv"
)

full_out_path <- paste0(out_path, tidy_data_filenames)

# for (i in 1:length(raw_data)) {
#   path <- raw_data[[i]]
#   filename <- full_out_path[[i]]
# 
#   discard_trash_and_save_tidy_data(path, filename, .bl = 10, .fps = 4)
#   print(paste("file", i, "saved"))
# }


# Import tidy data --------------------------------------------------------

tidy_files <- list.files("data/220705_LOX_KO_data_ALL/", 
                         pattern = ".csv",
                         full.names = TRUE)

dat <- tibble(dataset = str_remove(tidy_data_filenames, ".csv"))
dat$data <- map(tidy_files, read_csv)


# Renest data and tidy up names
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
# ggplot(dat_l, aes(l, group = dataset)) +
#   geom_density() +
#   facet_wrap(~dataset)


## Find most prevalent track length (per sample) ----
find_most_prevalent_length <- function(.x) {
  # Find the most prevalent number in a vector
  
  max_length <- max(.x)
  x_reasonable <- .x[.x > max(.x)*0.5]
  counts_table <- table(.x)
  
  # Some samples have a huge number of super short tracks, e.g. 2, and some
  # samples end sooner than 620, or the tissue tears at an early timepoint.
  # This removes too short tracks while considering the total video duration.
  # counts_table <- counts_table[counts_table > 0.5*max_length]
  
  max_count <- counts_table[counts_table == max(counts_table)]
  
  # Return the longest of the max. counts
  max(as.integer(names(max_count)))[[1]]
}

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

get_valid_lengths <- function(.most) {
  # Get valid track lengths based on most prevalent track length
  limits <- c(round(.most-0.05*.most), round(.most+0.05*.most))
  limits[1]:limits[2]
}

dat_f <- dat2 %>% 
  mutate(data, data = map2(data, 
                           most, 
                           ~filter(.x, l %in% get_valid_lengths(.y)) %>% 
                             select(-l)
                           ))


# Normalize signal --------------------------------------------------------

## Assign FPS ----
# (for some reason 2 samples only have 2 fps)
dat_f$fps <- 4
dat_f$fps[1:2] <- 2


## Remove very short samples ----
dat_f <- dat_f[-c(38, 44, 50), ]


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
dat1245 <- dat
save(dat1245, file = "data/220707_LOX_KO_1245.RData")


# Load data ---------------------------------------------------------------

load("data/220707_LOX_KO_1245.RData")

