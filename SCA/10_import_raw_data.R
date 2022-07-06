# Description -------------------------------------------------------------
#' Load raw data and bring into useful state:
#' 1. Read sample infos from file names and read files
#' 2. Discard useless data
#' 3. Normalize calcium signal and convert frames to time
#' 4. Save tidy dataset


# Main function -----------------------------------------------------------

discard_trash_and_save_tidy_data <- function(.dir, .out_path, .bl, .fps) {
  # Read imageJ output, extract relavant info from filenames, 
  # discard corrupt tracks, select relavant data, and rename columns.
  # Then save "tidy" data in a new CSV.
  
  # .dir (str): path to folder with imageJ output
  # .bl (double): duration of baseline [sec]
  # .fps (double): frames per second
  # .out_path (str): filepath for new CSV file
  # folder/filename.csv, e.g. 'data/211222_LOX_ko.csv'
  # Input filename scheme: 
  # date_img-id_donor_passage_day_condition_strain-rate_max-strain_sample_...
  # Output: CSV file
  
  read_csv_plus(.dir) %>% 
    mutate(data = map(data, ~trash_and_rename(.))) %>% 
    mutate(data = map(data, ~reformat_signal(., .bl = .bl, .fps = .fps))) %>% 
    unnest(cols = data) %>% 
    write_csv(., .out_path)
}


## Sub-functions ----------------------------------------------------------

read_csv_plus <- function(.dir) {
  # Import data, reformat into dataframe and discard tracks without ID.
  
  # Read data.
  tibble(
    file_paths = list.files(.dir, full.names = T, pattern = ".csv"),
    data = map(file_paths, data.table::fread)) %>%
    mutate(file_paths = basename(file_paths)) %>%
    separate(col = file_paths,
             into = c('date', 'img_id', 'donor', 'passage', 'day', 'condition', 
                      'strain_rate', 'max_strain', 'sample'),
             sep = '_', extra = 'drop') %>%
    mutate(strain_rate = parse_number(strain_rate),
           max_strain = parse_number(max_strain),
           sample = parse_number(sample)) %>% 
    # Remove rows with invalid TRACK_ID; some TRACK_IDs are "None", which
    # also converts the column type to chr
    mutate(data = map(data, ~filter(., TRACK_ID != 'None'))) %>%
    mutate(data = map(data, ~mutate(., TRACK_ID = as.numeric(TRACK_ID))))
}

trash_and_rename <- function(.df) {
  # discard irrelevant columns and rename remaining columns
  .df %>% 
    select(TRACK_ID, MEAN_INTENSITY02, FRAME) %>%
    rename(cell_id = TRACK_ID, calcium_raw = MEAN_INTENSITY02, frame = FRAME)
}

reformat_signal <- function(.df, .bl, .fps) {
  # normalize signal and convert frame to time
  # .bl (double): duration of baseline [sec]
  # .fps (double): frames per second
  
  bl_end <- .bl*.fps
  
  .df %>% 
    group_by(cell_id) %>%
    # Normalize calcium signal
    na.omit() %>% 
    mutate(calcium = calcium_raw/mean(calcium_raw[1:bl_end])) %>%
    ungroup() %>% 
    rename(time = frame) %>% 
    mutate(time = time/.fps) %>%  # [time] = seconds
    select(-calcium_raw)
}


# Test code ---------------------------------------------------------------

# dat <- read_csv_plus("D:/iMic/220704_pat_ko5/res")
# dat1 <- pluck(dat, "data", 1) %>% trash_and_rename()
# 
# for (i in 1:nrow(dat)) {
#   datt <- pluck(dat, "data", i) %>% trash_and_rename() 
#   print(datt)
# }
# 
# reformat_signal(dat1)
# 
# for (i in 1:nrow(dat)) {
#   datt <- pluck(dat, "data", i) %>% 
#     trash_and_rename() %>% 
#     reformat_signal(.bl = 10)
#   # print(datt)
# }
# 
# 
# discard_trash_and_save_tidy_data("D:/iMic/220704_pat_ko5/res",
#                                  "data/220704_LOX-screening_ko5.csv",
#                                  .bl = 10,
#                                  .fps = 4)
