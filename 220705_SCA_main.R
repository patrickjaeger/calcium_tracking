# Workflow ----------------------------------------------------------------

#' 1. Use discard_trash_and_save_tidy_data() to import individual datasets from
#'    from different days
#' 2. Combine datasets from step 1 to form the base dataset
#' 3. Run all processing and analyses on the base dataset

# Packages ----------------------------------------------------------------

library(tidyverse)
source("SCA/10_import_raw_data.R")


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
#   filename <- tidy_data_file_names[[i]]
#   
#   discard_trash_and_save_tidy_data(path, filename, .bl = 10, .fps = 4)
#   print(paste("file", i, "saved"))
# }

