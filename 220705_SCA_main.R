# Workflow ----------------------------------------------------------------

#' 1. Use discard_trash_and_save_tidy_data() to import individual datasets from
#'    from different days
#' 2. Combine datasets from step 1 to form the base dataset
#' 3. Run all processing and analyses on the base dataset

# Packages ----------------------------------------------------------------

library(tidyverse)
source("SCA/10_import_raw_data.R")
source("SCA/20_tidy_data.R")


# Tidy raw data -----------------------------------------------------------

out_path <- "data/220705_LOX_KO_data_ALL/"
# dir.create(out_path)

# raw_data <- list(
#   "D:/iMic/211205_pat_KO1/res",
#   "D:/iMic/211222_pat_ko2/res",
#   "D:/iMic/220620_pat_ko4/res",
#   "D:/iMic/220704_pat_ko5/res"
# )

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
dat <- dat %>%
  unnest(data) %>%
  group_nest(across(1:10)) %>%
  tidy_conditions()

print(dat, n = nrow(dat))

# Condition overview
dat %>% 
  group_by(dataset, condition) %>% 
  summarise(n = n())


# Discard short tracks ----------------------------------------------------

## Overview ----

# Find lengths of tracks
dat_l <- dat %>% 
  unnest(data) %>% 
  group_nest(dataset, img_id, cell_id) %>% 
  mutate(l = map_int(data, ~dim(.)[[1]]))


# How are the track lengths distributed?
ggplot(dat_l, aes(l, group = dataset)) +
  geom_density() +
  facet_wrap(~dataset)


# What are the most prevalent track lengths above 600?
dat_l %>% 
  group_by(dataset) %>% 
  count(l) %>% 
  filter(l > 600) %>% 
  arrange(desc(l)) %>% 
  print(n = nrow(.))


## Discard short tracks ----

dat_l %>% 
  filter(l %in% 610:620) %>% 
  select(-l) %>% 
  
  



ggplot(dat_ds, aes(time, calcium, group = condition, color = condition))