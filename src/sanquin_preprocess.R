suppressPackageStartupMessages(library(slider, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
suppressPackageStartupMessages(library(tictoc, quietly = TRUE))

source("helper_functions.R")  # For hours_to_numeric

read_sanquin_donations <- function(donation_file) {
  ########## DONATION
  # In the full dataset there are lots of missing values. This causes automatic recognition of column types to fail.
  # Therefore we give them explicitly here.
  
  input_col_types <- list(
    KEY_DONOR = col_character(),
    KEY_DONAT_INDEX_DATE = col_character(),
    DONAT_PHLEB_START = col_character(),
    DONAT_STATUS = col_character(),
    KEY_DONAT_PHLEB = col_character(),
    DONAT_VOL_DRAWN = col_integer(),
    DONAT_RESULT_CODE = col_double()
  )
  
  donations <- read_delim(donation_file, col_names=TRUE, delim='|', col_types=input_col_types)
  message(sprintf("Read %i rows from file %s\n", nrow(donations), donation_file))
  
  return(donations)
}

read_sanquin_donors <- function(donor_file) {
  input_col_types2 <- list(
    KEY_DONOR     = col_character(),
    KEY_DONOR_SEX = col_character(),
    KEY_DONOR_DOB = col_character(),
    DONOR_DATE_FIRST_DONATION = col_character(),
    FERRITIN_LAST_DATE = col_character()
  )
  donors <- read_delim(donor_file, delim="|", col_types = input_col_types2)
  message(sprintf("Read %i rows from file %s\n", nrow(donors), donor_file))

  return(donors)
}


# The first two parameters can be either filenames or dataframes
sanquin_preprocess <- function(donations, donors, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, southern_hemisphere,
                               max_diff_date_first_donation, restrict_time_window=TRUE, cores=1, logger) {
  #tic()
  tic()
  if (is.character(donations)) {   # is a filename instead of a dataframe?
    donations <- read_sanquin_donations(donations)
  }
  if (is.character(donors)) {   # is a filename instead of a dataframe?
    donors <- read_sanquin_donors(donors)
  }
  
  helper <- function(donations2, donors2, logger) {
    #cat(paste(format(get_object_sizes(rlang::current_env()), n=Inf), collapse="\n"))
    freadFRC(donations2, donors2, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, 
             southern_hemisphere, max_diff_date_first_donation, restrict_time_window=restrict_time_window, 
             logger=logger)
  }
  cat(sprintf("Number of cores is %i\n", cores))
  if (cores == 1) {
    data <- helper(donations, donors, logger)
  } else {
    # Split the donors into 'cores' group and preprocess them in parallel
    loggers <- map(1:cores, function(i) {new_logger(prefix=sprintf("Preprocess %i:", i), 
                                                    file=sprintf("/tmp/exclusions-%i.txt", i))})
    folds <- createFolds(1:nrow(donors), k = cores, list = TRUE, returnTrain = FALSE)
    options(future.globals.maxSize = 1.5e9)  # This is the maximum amount of data that can be passed to workers.
    future::plan(multicore, workers = cores)  # Multicore does not work with Rstudio. Multisession causes problems with logging
    data <- furrr::future_map2_dfr(folds, loggers,
                                   function(indices, logger) {
                                     helper(donations, donors[indices,], logger)},
                                     .options = furrr_options(seed=TRUE)
                                  )
  }
  
  toc()
  return(data)
}



sanquin_sample_raw_progesa <- function(donations, donors, donation.out = NULL, donor.out = NULL, ndonor = 1.0) {
  
  if (is.character(donations)) {   # is a filename instead of a dataframe?
    donations <- read_sanquin_donations(donations)
  }
  
  if (is.character(donors)) {   # is a filename instead of a dataframe?
    donors <- read_sanquin_donors(donors)
  }
  
  message(sprintf("Sampling to %f\n", ndonor))
  if (ndonor > 1.0) {   # is a count instead of proportion?
    donors <- slice_sample(donors, n=ndonor)
  } else if (ndonor < 1.0) {
    donors <- slice_sample(donors, prop=ndonor)
  }
  donor_ids <- donors$KEY_DONOR
  
  if (ndonor != 1.0)
    donations <- donations %>% filter(KEY_DONOR %in% donor_ids)  
  
  if (!is.null(donation.out)) {
    write_delim(donations, donation.out, delim="|", col_names = TRUE)
    message(sprintf("Wrote %i rows to file %s\n", nrow(donations), donation.out))
  }
  if (!is.null(donor.out)) {
    write_delim(donors, donor.out, delim="|", col_names = TRUE)
    message(sprintf("Wrote %i rows to file %s\n", nrow(donors), donor.out))
  }

  return(list(donations=donations, donors=donors))
}



sanquin_preprocess_donor_specific <- function(donor, fulldata_preprocessed, use_only_first_ferritin) {
  if (use_only_first_ferritin) {
    donor_specific <- donor %>% select(donor = KEY_DONOR, FERRITIN_FIRST)
    message("hep2\n")
    old_count <- nrow(donor_specific)
    donor_specific <- donor_specific %>% 
      filter(!is.na(FERRITIN_FIRST))
    message(sprintf("Dropped %i / %i donors due to FERRITIN_FIRST being NA\n", 
                old_count - nrow(donor_specific), old_count))
  } else {
    stopifnot(all(c("FERRITIN_FIRST", "FERRITIN_LAST", "FERRITIN_LAST_DATE") %in% names(donor)))
    donor_specific <- donor %>% select(donor = KEY_DONOR, FERRITIN_FIRST, FERRITIN_LAST, FERRITIN_LAST_DATE)
    message("hep2\n")
    old_count <- nrow(donor_specific)
    donor_specific <- donor_specific %>% 
      filter(!is.na(FERRITIN_FIRST), !is.na(FERRITIN_LAST), !is.na(FERRITIN_LAST_DATE)) %>%
      mutate(FERRITIN_LAST_DATE=lubridate::as_date(FERRITIN_LAST_DATE))
    message(sprintf("Dropped %i / %i donors due to FERRITIN_FIRST/LAST/LAST_DATE being NA\n", 
                old_count - nrow(donor_specific), old_count))
    
    # Select only donors whose last ferritin is not from the last donation
    last_donations <- fulldata_preprocessed %>% group_by(donor) %>% slice_max(order_by=dateonly) %>% ungroup() %>% select(donor, dateonly)
    old_count <- nrow(donor_specific)
    donor_specific <- donor_specific %>% anti_join(last_donations, by=c("donor"="donor", "FERRITIN_LAST_DATE"="dateonly")) # %>% select(-FERRITIN_LAST_DATE)
    message(sprintf("Dropped %i / %i donors due to FERRITIN_LAST_DATE being equal to last donation date\n", 
                old_count - nrow(donor_specific), old_count))
  }
  old_count <- nrow(donor_specific)
  donor_specific <- donor_specific %>% semi_join(fulldata_preprocessed, by="donor") # make sure these were not preprocessed away
  message(sprintf("Dropped %i / %i donors due to joining with preprocessed data\n", 
              old_count - nrow(donor_specific), old_count))
  return(donor_specific)
}
