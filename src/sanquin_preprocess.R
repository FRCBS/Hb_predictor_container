suppressPackageStartupMessages(library(slider, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
suppressPackageStartupMessages(library(tictoc, quietly = TRUE))

source("helper_functions.R")  # For hours_to_numeric

# max_diff_date_first_donation is a non-negative integer, which specifies the maximum allowed difference
# between min(KEY_DONAT_INDEX_DATE) and DONOR_DATE_FIRST_DONATION
sanquin_freadFRC <- function(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, max_diff_date_first_donation)
{
  
  
  ########## DONATION
  # In the full dataset there lots of missing values. This causes automatic recognition of column types to fail.
  # Therefore we give them explicitly here.
  # input_col_types <- list(
  # X1 = col_character(),
  # X2 = col_character(),
  # X3 = col_character(),
  # X4 = col_double(),
  # X5 = col_character(),
  # X6 = col_character(),
  # X7 = col_character(),
  # X8 = col_character(),
  # X9 = col_character(),
  # X10 = col_character(),
  # X11 = col_double(),
  # X12 = col_double()
  # )

  input_col_types <- list(
    #X1 = col_character(),
    KEY_DONOR = col_character(),
    #X3 = col_character(),
    KEY_DONAT_INDEX_DATE = col_double(),
    DONAT_PHLEB_START = col_character(),
    DONAT_STATUS = col_character(),
    KEY_DONAT_PHLEB = col_character(),
    #X8 = col_character(),
    #½X9 = col_character(),
    DONAT_VOL_DRAWN = col_character(),
    #X11 = col_double(),
    DONAT_RESULT_CODE = col_double()
  )
  
  donation <- read_delim(donation.file, col_names=TRUE, delim='|', col_types=input_col_types)
  old_names <- c("KEY_DONOR", "KEY_DONAT_INDEX_DATE", "DONAT_PHLEB_START", 
                 "DONAT_STATUS", "KEY_DONAT_PHLEB", 
                 "DONAT_VOL_DRAWN",
                 "DONAT_RESULT_CODE")
  new_names <- c('donor', 'date', 'phleb_start',
                    'status', 'donat_phleb', 
                    'volume_drawn', 
                    'Hb')
  #names(donation) <- new_names
  temp <- old_names
  names(temp) <- new_names
  donation <- donation %>% rename(!!!temp)
  #print(head(donation))
  
  donation <- donation %>%
    mutate(donat_phleb = as.factor(donat_phleb),
           volume_drawn = as.integer(volume_drawn),
           #Hb = as.numeric(Hb) * 10,  # convert g/dL to g/L
           Hb = convert_hb_unit(Hb_input_unit, "gperl", as.numeric(Hb)),  # convert to g/L
           donat_phleb = recode(donat_phleb, `Whole blood`="K"),
           phleb_start = as.numeric(phleb_start))
           #donStartTime = as.integer(donStartTime))

  

  mytemp <- ymd_hm(sprintf("%s %04i", as.character(donation$date), donation$phleb_start))
  mm <- is.na(mytemp)
  cat("Failed to parse dates:", sum(mm), "\n")
  if (sum(mm) > 0) {
      print(donation %>% filter(mm) %>% summary %>% mutate_at("status", as.factor))
  }
  donation$date <- mytemp
  print(summary(donation %>% mutate_at("status", as.factor)))
  
  if (FALSE) {
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  yn=grep("^Y\\d{14}.$", as.vector(donation[["donation"]]),perl=TRUE) #The last one can be any character. #data.table way
  #But the ones used in luhti have 15 chars?
  bad <- unique(as.character(donation$donation)[-yn])
  cat("Bad ones look like this:\n",head(bad),"...",tail(bad),"\n")
  donation <- donation[yn,]
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) due to badly formed ID\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  }
  
  ######### DONOR
  input_col_types2 <- list(
    KEY_DONOR = col_character())
  donor <- read_delim(donor.file, delim="|", col_types = input_col_types2)
  #old_donor_names <- c("KEY_DONOR", "KEY_DONOR_SEX", "KEY_DONOR_DOB", "DONOR_DATE_FIRST_DONATION")
  #new_donor_names <- c('donor', 'gender', 'dob', 'date_first_donation')
  #temp <- old_donor_names
  conversion <- c(donor="KEY_DONOR", gender="KEY_DONOR_SEX", dob="KEY_DONOR_DOB", date_first_donation="DONOR_DATE_FIRST_DONATION")
  #names(temp) <- new_donor_names
  donor <- donor %>% rename(!!!conversion)
  #names(donor) <- new_donor_names
  #1 "9626820"|
  #2 "YYYY XXXX"|
  #3 "ZZZZ"
  #4 |"Mies"|
  #5 "syntymaaika"
  #6 |"FI"
  #7 |"A Rh(D) pos"
  #8 |"osoite"|
  #9 "65200"|
  #10 "VAASA"|
  #11 "puh1"|
  #12 ""|
  #13 "puh2"
  #14 |"Ei"
  #15 |"01"
  #16 |"02"
  #17 |""
  #18 |"7"
  #19 |"7"
  #20 |""
  #21|"20030731"
  #22 |"7"
  #23 |""
  #24 |""
  #25 |"20070807"
  #26 |"H1157"
  #print(head(donor))
  donor <- donor %>%
    mutate(gender = as.factor(gender))
  print(summary(donor))
  
  conversion2 <- c(nb_donat_progesa="DONOR_NB_DONAT_PROGESA", nb_donat_outside="DONOR_NB_DONAT_OUTSIDE")
  variables <- c("donor", "gender", "dob", "date_first_donation", conversion2)
  if (length(intersect(conversion2, names(donor))) == 0)  { # numbers of donations not provided
    donor <- donor %>% mutate(DONOR_NB_DONAT_PROGESA=NA, DONOR_NB_DONAT_OUTSIDE=0)
    compute_donation_counts <- TRUE;
  } else {
    compute_donation_counts <- FALSE;
  }

  donor2 <- donor %>% 
#    select(donor, gender, dob, date_first_donation, nb_donat_progesa, nb_donat_outside) %>%
    select(!!!variables) %>%
    filter(donor %in% unique(donation$donor)) #Remove extra donors to get clean join  
  
  
  #Droplevels so that they don't bother you later
  donation <- droplevels(donation)
  donor2 <- droplevels(donor2)
  #The make the ordering of factors the same, as well, to avoid complaints from join
  levels(donor2$donor) < levels(donation$donor)
  
  #Format dates
  donor2 <- donor2 %>% mutate(dob = ymd(as.character(dob)),
                              date_first_donation = ymd(date_first_donation))
  
  #JOIN
  donation2 <- donation %>% inner_join(donor2, by = c("donor" = "donor"))
  
  stopifnot(nrow(donation2)==nrow(donation))
  donation <- donation2
  
  if (compute_donation_counts) {
    donation <- donation %>% group_by(donor) %>% mutate(nb_donat_progesa = n()) %>% ungroup()
  }
  
  print(table(donation$gender))
  #English sex
  levels(donation$gender) = c('Men','Women')

  #Sort
  donation <- donation %>% arrange(date)  
  #Add the 13 char string
  donation <- donation %>%
      mutate(#donation13 = str_sub(donation, 1, 13),
             #donation = as.factor(donation),
             donor = as.factor(donor))
  
  #Drop cases where date and dob are identical
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  ids <- paste0(year(donation$date), month(donation$date), day(donation$date)) == paste0(year(donation$dob), month(donation$dob), day(donation$dob))
  donation <- donation[!ids,]
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) due to indentical date and dob\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  
  #Drop cases where either date or dob is NA
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  m <- is.na(donation$date) | is.na(donation$dob)
  donation <- donation %>% filter(!m)
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) due to date or dob being NA\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  
  #Drop cases where date is "19390101"
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  ids <- paste0(year(donation$date),month(donation$date),day(donation$date)) ==  "193911"
  donation <- donation[!ids,]
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) due to date '19390101'\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  
  # Drop cases where date_first_donation is NA
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  donation <- donation %>%
    filter(!is.na(date_first_donation))
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) because date_first_donation is not known.\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  
  #make age at time of donation
  #https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
  age <- as.period(interval(start = donation$dob, end = donation$date))$year
  donation$age <- age
  
  #Drop anything of age below 18
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  ids <- donation$age < 18
  donation <- donation[!ids,]
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) due to age at time of donation below 18\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  
  donation <- droplevels(donation)
  #age groups
  age.group <- cut(donation$age,breaks=c(min(donation$age),seq(from=25,to=65,by=10),max(donation$age)),include.lowest = TRUE)
  donation$age.group <- factor(age.group)
  
  #Split date into parts
  donation <- donation %>% mutate(Year = year(date), 
                                  Month = month(date), 
                                  Day = day(date), 
                                  Hour=hour(date), 
                                  Week = week(date))
  
  donation <- donation %>% 
    group_by(Month)  %>% 
    mutate(monthHb=mean(Hb,na.rm=TRUE)) %>%
    ungroup()
  
  donation <- donation %>% 
    group_by(Day)  %>% 
    mutate(dayHb=mean(Hb,na.rm=TRUE)) %>%
    ungroup()

  
  #Add deferral rate
  hbd <- rep(0, nrow(donation))
  hbd[donation$donat_phleb == '*' & donation$Hb < Hb_cutoff_male & donation$gender == 'Men'] <- 1
  hbd[donation$donat_phleb == '*' & donation$Hb < Hb_cutoff_female & donation$gender == 'Women'] <- 1
  donation$'Hb_deferral' <- as.factor(hbd)
  print(table(donation$gender,donation$Hb_deferral))
  donation$Hb_deferral <- as.integer(as.character(donation$Hb_deferral))   # Fix Hb_deferral everywhere !!!!!!!!!!!!!!!!!

  #donation <- donation %>% 
  #  select(-c("first",    "family",   "language")) #%>% 
  

  donation <- donation %>% mutate(dateonly = date(date))
  
  # Find the number of tries per day, and select the last try of the day
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  donation <- donation %>% 
    group_by(donor, dateonly) %>% 
    mutate(triesOnTheDay=n()) %>% 
    filter(max(date) == date) %>% 
    ungroup()
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) because only last try of the day is selected\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))

  # Select only those donors whose first blood donation is close to the date as progesa's date_first_donation tells
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  donation <- donation %>%
    group_by(donor) %>%
    mutate(imputed_first = min(dateonly),
           given_first = min(date_first_donation),
           difference = as.numeric(imputed_first - given_first)) %>% # get a single value not vector
    filter(0 <= difference, difference <= max_diff_date_first_donation ) %>%
    ungroup() %>% 
    select(-difference)
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) because the given date_first_donation was not the oldest donation for that donor\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  donation <- donation %>%
    mutate(first_event = dateonly==imputed_first)
    #mutate(first_event = dateonly==date_first_donation)
  
  # Drop donors whose first Hb is NA
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  bad_donors <- donation %>%
    filter(first_event==TRUE & is.na(Hb)) %>%
    pull(donor)
  donation <- donation %>%
    filter(!(donor %in% bad_donors))
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) whose first Hb is NA\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2))
  
  invisible(donation)
}





sanquin_decorate_data <- function(data) {
  
  #Take all donations events (regardless of type)
  data$Hb[is.nan(data$Hb)] <- NA
  
  # Sort the data into ascending timeseries
  data <- data %>% arrange(date)
 
  
  # * The donors with K and - events that had 0, or -1 IPVs had indeed donated a normal amount (blood has been processed and distributed)
  # * Donation events with K and R codes thatwere initially platelet collections (location H0092) are still counted as normal donations as longas their IPV is over 100
  # * A few donations events that were coded * with  IPV > 100 are kept as deferrals, further inverstigation by Elina showed that these were typos.
  #
  # Find the difference in days to previous donation for full blood donations only !!!
  # first FB donation in history ~ DaysToPrevious = 0
  # FB donation ~ DaysToPrevious = nb of days to previous FB donation
  # not FB donation ~ NA
  #before, NA meant FB donatsion
  
  ### Add don_id variable to identify each donation event
  data$don_id <- as.character(seq.int(1:nrow(data)))
  
  
  # Compute days to previous full blood donation
  tic("Days to previous")
  data <- data %>%
    mutate(current_or_previous_full_blood_date = as_date(ifelse(donat_phleb == 'K' & status == "-" |
                                                          donat_phleb == 'K' & status != "-" & volume_drawn > 100 |
                                                          donat_phleb == 'H' & volume_drawn > 100, 
                                                        dateonly, 
                                                        NA))) %>%
    group_by(donor) %>%
    fill(current_or_previous_full_blood_date) %>%  # impute missing values from previous full blood donation
    mutate(days_to_previous_fb = as.integer(round(dateonly - lag(current_or_previous_full_blood_date)))) %>%
    ungroup() %>%
    select(-current_or_previous_full_blood_date)
  toc()
  
  # Assumes Hb_deferral is never NA
  consecutive_deferrals_f <- function(Hb_deferral) {
    c <- cumsum(Hb_deferral)
    c_at_previous_non_deferral <- ifelse(Hb_deferral, NA, c)
    if (is.na(c_at_previous_non_deferral[1])) {
      c_at_previous_non_deferral[1] <- 0
    }
    c_at_previous_non_deferral <- fill(enframe(c_at_previous_non_deferral), value)$value
    consecutive_deferrals <- lag(c - c_at_previous_non_deferral, default=0)
    return(consecutive_deferrals)
  }
  
  
  #print(head(data))
  cat("Before Hb computations\n")
  tic("Previous and first Hb, previous_Hb_def, and amount of dererrals")
  # Get previous and first Hb values, previous_Hb_def, and amount of deferrals since last succesful donation event
  
  data <- data %>%
    group_by(donor) %>%
    mutate(previous_Hb_def = lag(as.integer(as.character(Hb_deferral)), default = NA),
           Hb_first = Hb[first_event == T],
           previous_Hb = lag(Hb, default=0)
           ) %>%
    fill(previous_Hb) %>% # fills NA with previous non-NA
    mutate(consecutive_deferrals = consecutive_deferrals_f(Hb_deferral)) %>%
    ungroup()
  toc()
  
  # Get year, hour, and season of donation
  tic("Season")
  data <- data %>%
    mutate(hour = hours_to_numeric(date)) %>%
    mutate(year = as.integer(year(dateonly))) %>%
    mutate(warm_season = as.logical(unlist(lapply(month(dateonly), FUN = get_season))))
  toc()
  
  # Fix values where hour is 0
  hour.mean <- mean(data$hour)
  data <- mutate(data, hour = ifelse(hour == 0, mean(data$hour), hour))
  

  
  tic("Two year donations/deferrals")
  two_year_sliding_window_sum <- function(weight, date) {
    v <- as.numeric(slider::slide_index(weight, date, sum, .before = lubridate::dyears(2))) # years(2) had problems with leap days
    return(v)
  }
  
  data <- data %>%
    mutate(weight_donation=ifelse(donat_phleb == "K", 1, 0),
           Hb_deferral = as.integer(as.character(Hb_deferral))) %>%
    arrange(date) %>%
    group_by(donor) %>%
    mutate(recent_donations = two_year_sliding_window_sum(weight_donation, date)) %>%
    mutate(recent_deferrals = two_year_sliding_window_sum(Hb_deferral, date)) %>%
    ungroup() %>%
    mutate(recent_donations = as.integer(recent_donations-weight_donation),   # exclude the current donation from previous two years
           recent_deferrals = as.integer(recent_deferrals-Hb_deferral))       # exclude the current deferral from previous two years
  toc()
  
  # This isn't used anywhere !!!!!!!!!!!!!!!!
  # Add variable for how many times a donor has donated in the data
  # data <- data %>%
  #   group_by(donor) %>%
  #   mutate(times_donated = 1:n()) %>%
  #   ungroup()
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>%
    filter(donat_phleb == 'K' | donat_phleb == '*')
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) because donat_phleb was not 'K' nor '*'\n", 
              old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2))
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  print(data %>% filter(!(first_event==TRUE | (!is.na(days_to_previous_fb) & !is.na(Hb)))) %>% select(days_to_previous_fb, Hb), n=Inf)
  data <- data %>%
    filter(first_event==TRUE | (!is.na(days_to_previous_fb) & !is.na(Hb)))
  cat(sprintf("Dropped %i / %i donations (%i / %i donors) because Hb or days_to_previous_fb was NA for a non-first donation\n", 
              old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2))
  
  # Select only the interesting variables, rename some of them and change the types
  tic("Final selection")
  data <- data %>%
    mutate(don_id = as.factor(don_id), 
           donor = donor, 
           Hb_deferral = as.logical(Hb_deferral),
           previous_Hb_def = as.logical(previous_Hb_def),
           consecutive_deferrals = as.integer(consecutive_deferrals),
           nb_donat_progesa = as.integer(nb_donat_progesa),
           nb_donat_outside = as.integer(nb_donat_outside)) %>%
    select(don_id, donor, Hb, dateonly, previous_Hb_def, days_to_previous_fb, donat_phleb, gender, age,
           Hb_deferral, nb_donat_progesa, nb_donat_outside,
           first_event, previous_Hb, year, warm_season, Hb_first, hour, consecutive_deferrals, recent_donations,
           recent_deferrals) %>%
    arrange(donor)
  toc()
  
  cat(sprintf("Final preprocessed data has %i donations and %i donors\n", nrow(data), ndonor(data)))
  invisible(data)
  return(data)
}

sanquin_preprocess <- function(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, max_diff_date_first_donation) {
  tic()
  tic()
  data <- sanquin_freadFRC(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, max_diff_date_first_donation)
  toc()
  tic()
  data <- sanquin_decorate_data(data)
  toc()
  toc()
  return(data)
}

sanquin_preprocess_helper <- function(dir, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, max_diff_date_first_donation) {
  tic()
  tic()
  donation.file <- paste0(dir,"/FRC.DW_DONATION.dat")
  donor.file <- paste0(dir,"/FRC.DW_DONOR.dat")
  data <- sanquin_freadFRC(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit, max_diff_date_first_donation)
  toc()
  tic()
  data <- sanquin_decorate_data(data)
  toc()
  toc()
  return(data)
}

sanquin_sample_raw_progesa <- function(donation.file, donor.file, donation.out = donation.file, donor.out = donor.file, ndonor = 1.0) {
  
  
  # Maybe all the fields should have type col_character, so that they would be kept the same after reading the csv and writing it back
  
  # In the full dataset there lots of missing values. This causes automatic recognition of column types to fail.
  # Therefore we give them explicitly here.
  input_col_types <- list(
    #X1 = col_character(),
    KEY_DONOR = col_character(),
    #X3 = col_character(),
    KEY_DONAT_INDEX_DATE = col_character(),
    DONAT_PHLEB_START = col_character(),
    DONAT_STATUS = col_character(),
    KEY_DONAT_PHLEB = col_character(),
    #X8 = col_character(),
    #½X9 = col_character(),
    DONAT_VOL_DRAWN = col_character(),
    #X11 = col_double(),
    DONAT_RESULT_CODE = col_double()
    #.default = col_character()
  )
  
  
  donation <- read_delim(donation.file, col_names=TRUE, delim='|', col_types=input_col_types)

  cat(sprintf("Read %i rows from file %s\n", nrow(donation), donation.file))
    
  input_col_types2 <- list(
    KEY_DONOR = col_character(),
    KEY_DONOR_DOB = col_character(),
    DONOR_DATE_FIRST_DONATION = col_character(),
    FERRITIN_LAST_DATE = col_character()
    #.default = col_character()
    )
  donor <- read_delim(donor.file, col_names=TRUE, delim="|", col_types = input_col_types2)


  cat(sprintf("Read %i rows from file %s\n", nrow(donor), donor.file))
  
#  if (ndonor != 1.0) {
    cat(sprintf("Sampling to %f\n", ndonor))
    if (ndonor > 1.0) {   # is a count instead of proportion?
      donor <- slice_sample(donor, n=ndonor)
    } else if (ndonor < 1.0) {
      donor <- slice_sample(donor, prop=ndonor)
    }
    donor_ids <- donor$KEY_DONOR
    
    if (ndonor != 1.0)
      donation <- donation %>% filter(KEY_DONOR %in% donor_ids)  
    
    #if (!file.exists(donation.out)) {
    write_delim(donation, donation.out, delim="|", col_names = TRUE)
    cat(sprintf("Wrote %i rows to file %s\n", nrow(donation), donation.out))
    #}
    #if (!file.exists(donor.out)) {
    write_delim(donor, donor.out, delim="|", col_names = TRUE)
    cat(sprintf("Wrote %i rows to file %s\n", nrow(donor), donor.out))
    #}
#  }
  return(list(donation=donation, donor=donor))
}

sanquin_preprocess_donor_specific <- function(donor, fulldata_preprocessed, use_only_first_ferritin) {
  if (use_only_first_ferritin) {
    donor_specific <- donor %>% select(donor = KEY_DONOR, FERRITIN_FIRST)
    cat("hep2\n")
    old_count <- nrow(donor_specific)
    donor_specific <- donor_specific %>% 
      filter(!is.na(FERRITIN_FIRST))
    cat(sprintf("Dropped %i / %i donors due to FERRITIN_FIRST being NA\n", 
                old_count - nrow(donor_specific), old_count))
  } else {
    stopifnot(all(c("FERRITIN_FIRST", "FERRITIN_LAST", "FERRITIN_LAST_DATE") %in% names(donor)))
    donor_specific <- donor %>% select(donor = KEY_DONOR, FERRITIN_FIRST, FERRITIN_LAST, FERRITIN_LAST_DATE)
    cat("hep2\n")
    old_count <- nrow(donor_specific)
    donor_specific <- donor_specific %>% 
      filter(!is.na(FERRITIN_FIRST), !is.na(FERRITIN_LAST), !is.na(FERRITIN_LAST_DATE)) %>%
      mutate(FERRITIN_LAST_DATE=lubridate::as_date(FERRITIN_LAST_DATE))
    cat(sprintf("Dropped %i / %i donors due to FERRITIN_FIRST/LAST/LAST_DATE being NA\n", 
                old_count - nrow(donor_specific), old_count))
    
    # Select only donors whose last ferritin is not from the last donation
    last_donations <- fulldata_preprocessed %>% group_by(donor) %>% slice_max(order_by=dateonly) %>% ungroup() %>% select(donor, dateonly)
    old_count <- nrow(donor_specific)
    donor_specific <- donor_specific %>% anti_join(last_donations, by=c("donor"="donor", "FERRITIN_LAST_DATE"="dateonly")) # %>% select(-FERRITIN_LAST_DATE)
    cat(sprintf("Dropped %i / %i donors due to FERRITIN_LAST_DATE being equal to last donation date\n", 
                old_count - nrow(donor_specific), old_count))
  }
  old_count <- nrow(donor_specific)
  donor_specific <- donor_specific %>% semi_join(fulldata_preprocessed, by="donor") # make sure these were not preprocessed away
  cat(sprintf("Dropped %i / %i donors due to joining with preprocessed data\n", 
              old_count - nrow(donor_specific), old_count))
  return(donor_specific)
}