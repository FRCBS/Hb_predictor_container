suppressPackageStartupMessages(library(slider, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
suppressPackageStartupMessages(library(tictoc, quietly = TRUE))

#source("helper_functions.R")  # For hours_to_numeric
source("common.R") # For new_logger

compute_donations_counts <- FALSE

read_donations <- function(donation_file) {
  # In the full dataset there are lots of missing values. This causes automatic recognition of column types to fail.
  # Therefore we give them explicitly here.
  input_col_types <- list(
    X1 = col_character(),
    X2 = col_character(),
    X3 = col_character(),
    X4 = col_double(),
    X5 = col_character(),
    X6 = col_character(),
    X7 = col_character(),
    X8 = col_character(),
    X9 = col_character(),
    X10 = col_character(),
    X11 = col_double(),
    X12 = col_double()
  )
  
  donations <- read_delim(donation_file, col_names=FALSE, delim='|', col_types=input_col_types)
  message(sprintf("Read %i rows from file %s\n", nrow(donations), donation_file))
  
    old_names <-   c("KEY_DONAT_INDEX_DON", "KEY_DONOR", "KEY_DONAT_COLLECT", "KEY_DONAT_INDEX_DATE", "DONAT_PHLEB_START", 
                   "DONAT_STATUS", "KEY_DONAT_PHLEB", "DONAT_DIRECTED", "Field name?", "DONAT_VOL_DRAWN", "KEY_DONAT_INDEX_TEST", 
                   "DONAT_RESULT_CODE")
  names(donations) <- old_names
  return(donations)
}

read_donors <- function(donor_file) {
  input_col_types2 <- list(
    X1 = col_character())
  donors <- read_delim(donor_file, col_names=FALSE, delim="|", col_types = input_col_types2)
  message(sprintf("Read %i rows from file %s\n", nrow(donors), donor_file))
  old_names <-
    c("KEY_DONOR", "DONOR_FIRST", "DONOR_NAME", "KEY_DONOR_SEX", "KEY_DONOR_DOB", "DONOR_LANGUAGE", "KEY_DONOR_ABORH", "DONOR_ADDR_1_A", 
    "KEY_DONOR_ZIP_1", "DONOR_CITY_1", "DONOR_TEL_1", "DONOR_E_MAIL", "DONOR_TEL_MOBILE", "DONOR_NOTIFIABLE", "DONOR_NOTIFICATION_METHOD_1", 
    "DONOR_NOTIFICATION_METHOD_2", "DONOR_NOTIFICATION_METHOD_3", "DONOR_NB_DONATIONS", "DONOR_NB_DONAT_PROGESA", "DONOR_NB_DONAT_OUTSIDE", 
    "DONOR_DATE_FIRST_DONATION", "DONOR_NB_WB", "DONOR_NB_PLA", "DONOR_NB_THR", "DONOR_LAST_DONAT_PHLEB", "DONOR_LAST_COLLECT")
  names(donors) <- old_names
	
  donors <- donors %>% mutate(KEY_DONOR_SEX = case_when(KEY_DONOR_SEX == "Man"   ~ "M",
                                              KEY_DONOR_SEX == "Woman" ~ "F"))
  return(donors)
}

# max_diff_date_first_donation is a non-negative integer, which specifies the maximum allowed difference
# between min(KEY_DONAT_INDEX_DATE) and DONOR_DATE_FIRST_DONATION
freadFRC <- function(donation, donor, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit,
                             southern_hemisphere, max_diff_date_first_donation, restrict_time_window=TRUE, logger)
{
  message("In function freadFRC")
  
  
  
  conversion <- c(donor = "KEY_DONOR", 
                  date = "KEY_DONAT_INDEX_DATE", 
                  phleb_start = "DONAT_PHLEB_START", 
                  status = "DONAT_STATUS", 
                  donat_phleb = "KEY_DONAT_PHLEB", 
                  volume_drawn = "DONAT_VOL_DRAWN",
                  Hb = "DONAT_RESULT_CODE")
  
  ########## DONATION
  
#  names(donation)=c('donation', 'donor', 'site', 'date', 'phleb_start',
#                    'status', 'donat_phleb', 
#                    'directed', 'donStartTime', 'volume_drawn', 'index_test', 
#                    'Hb')
  donation <- donation %>% rename(!!!conversion)

    #print(head(donation))
  mean_hb <- mean(donation$Hb, na.rm=TRUE)
  if (!is_hb_value_sane(mean_hb, Hb_input_unit)) {
    warning(sprintf("The mean Hb value %f does not seem to agree with the Hb unit %s\n", mean_hb, Hb_input_unit))
  }
  donation <- donation %>%
    mutate(donat_phleb = as.factor(donat_phleb),
           volume_drawn = as.integer(volume_drawn),
           Hb = convert_hb_unit(Hb_input_unit, "gperl", as.numeric(Hb)),  # convert to g/L
           phleb_start = as.numeric(phleb_start))
           #donStartTime = as.integer(donStartTime))

  

  mytemp <- ymd_hm(sprintf("%s %04i", donation$date, donation$phleb_start))
  mm <- is.na(mytemp)
  message("Failed to parse dates:", sum(mm), "\n")
  if (sum(mm) > 0) {
      print(donation %>% filter(mm) %>% mutate_at("status", as.factor) %>% summary)
  }
  donation$date <- mytemp
  print(summary(donation %>% mutate_at("status", as.factor)))
  
  # No point filtering by this rule
  if (FALSE && "KEY_DONAT_INDEX_DON" %in% colnames(donation)) {
    old_count <- nrow(donation); old_count2 <- ndonor(donation)
    yn=grep("^Y\\d{14}.$", as.vector(donation[["KEY_DONAT_INDEX_DON"]]),perl=TRUE) #The last one can be any character. #data.table way
    #But the ones used in luhti have 15 chars?
    bad <- unique(as.character(donation$KEY_DONAT_INDEX_DON)[-yn])
    message(sprintf("Bad ones look like this:\n%s ... %s\n", paste(head(bad), collapse=" "), paste(tail(bad), collapse=" ")))
    donation <- donation[yn,]
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to badly formed ID\n", 
                   old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
    message(msg)
    print(logger, msg)
  }
  
  ######### DONOR
  # conversion <- names(donor)
  #   
  # new_col_names=c('donor','first','family', 'sex', 'dob', 'language', 'aborh', 'address', 'zip', 'city',
  #                     'tel','email', 'mobile',
  #                     'notifiable', 'notification_method_1', 'notification_method_2', 'notification_method_3', 
  #                     'nb_donations', 'nb_donat_progesa', 'nb_donat_outside', 
  #                     'date_first_donation', 'nb_wb', 'nb_pla',
  #                     'nb_thr', 'last_donat_phleb', 'last_collect'
  #                     
  # )
  # if (ncol(donor) == length(new_col_names) + 1) {   # label is included in the input data
  #   new_col_names <- c(new_col_names, "label")   
  # }
  # names(conversion) <- new_col_names
  # donor <- donor %>% rename(!!!conversion)
  
  conversion <- c(donor="KEY_DONOR", sex="KEY_DONOR_SEX", dob="KEY_DONOR_DOB", date_first_donation="DONOR_DATE_FIRST_DONATION")
  donor <- donor %>% rename(!!!conversion)


  donor <- donor %>%
    mutate(sex = factor(sex, levels=c("M", "F")))
  print(summary(donor))
  
  # These variables are optional. Really they should not be used at all.
  conversion2 <- c(nb_donat_progesa="DONOR_NB_DONAT_PROGESA", nb_donat_outside="DONOR_NB_DONAT_OUTSIDE")
  variables <- c("donor", "sex", "dob", "date_first_donation", conversion2)
  if (length(intersect(conversion2, names(donor))) == 0)  { # numbers of donations not provided
    donor <- donor %>% mutate(DONOR_NB_DONAT_PROGESA=NA, DONOR_NB_DONAT_OUTSIDE=0)
    compute_donation_counts <<- TRUE;
  } else {
    compute_donation_counts <<- FALSE;
  }

  # Keep label column, if it is in the input
  variables <- c(variables, intersect(names(donor), "label"))
  
  common_donors <- intersect(unique(donation$donor), unique(donor$donor))
  
  donor2 <- donor %>% 
    select(!!!variables) %>%
    filter(donor %in% common_donors) #Remove extra donors to get clean join  

  donation <- donation %>%  
    filter(donor %in% common_donors) #Remove extra donors to get clean join  
  
  
  #Format dates
  donor2 <- donor2 %>% mutate(dob = ymd(as.character(dob)),
                              date_first_donation = ymd(date_first_donation))
  
  #JOIN
  donation2 <- donation %>% inner_join(donor2, by = c("donor" = "donor"))
  
  stopifnot(nrow(donation2)==nrow(donation))
  donation <- donation2
  rm(donation2)
  
  print(table(donation$sex))
  #English sex
  #levels(donation$sex) = c('Men','Women')   # This is wrong!!!!!!!!!!
  donation <- donation %>% mutate(sex=fct_recode(sex, "female" = "F", "male" = "M"),
                                  sex=fct_relevel(sex, c("male", "female")))  # Give fixed order to levels so that caret's
                                                                                  # variable coding is predictable

  #Sort
  donation <- donation %>% arrange(date)  
  #Add the 13 char string
  donation <- donation %>%
      mutate(#donation13 = str_sub(donation, 1, 13),
             #donation = as.factor(donation),
             donor = as.factor(donor))
  
  #Drop donations where date and dob are identical
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  ids <- paste0(year(donation$date), month(donation$date), day(donation$date)) == paste0(year(donation$dob), month(donation$dob), day(donation$dob))
  donation <- donation[!ids,]
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to identical date and dob\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)
  
  #Drop cases where either date or dob is NA
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  m <- is.na(donation$date) | is.na(donation$dob)
  donation <- donation %>% filter(!m)
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to date or dob being NA\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)
  
  #Drop cases where date is "19390101"
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  ids <- paste0(year(donation$date),month(donation$date),day(donation$date)) ==  "193911"
  donation <- donation[!ids,]
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to date '19390101'\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)
  
  
  if (FALSE) {
    # Drop donors whose date_first_donation is NA
    old_count <- nrow(donation); old_count2 <- ndonor(donation)
    donation <- donation %>%
      filter(!is.na(date_first_donation))
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because date_first_donation is not known.\n", 
                   old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
    message(msg)
    print(logger, msg)
  }
  
  #make age at time of donation
  #https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
  age <- as.integer(as.period(interval(start = donation$dob, end = donation$date))$year)
  donation$age <- age
  
  #Drop donations done when younger than 18 years
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  ids <- donation$age < 18
  donation <- donation[!ids,]
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to age at time of donation below 18\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)
  
  donation <- droplevels(donation)
  #age groups, not used
  # age.group <- cut(donation$age,breaks=c(min(donation$age),seq(from=25,to=65,by=10),max(donation$age)),include.lowest = TRUE)
  # donation$age.group <- factor(age.group)
  
  #Split date into parts
  donation <- donation %>% mutate(Year = year(date), 
                                  Month = month(date), 
                                  Day = day(date), 
                                  Hour=hour(date), 
                                  Week = week(date))
  
  # monthHb and dayHb are not used
  # donation <- donation %>% 
  #   group_by(Month)  %>% 
  #   mutate(monthHb=mean(Hb,na.rm=TRUE)) %>%
  #   ungroup()
  # 
  # donation <- donation %>% 
  #   group_by(Day)  %>% 
  #   mutate(dayHb=mean(Hb,na.rm=TRUE)) %>%
  #   ungroup()

  
  #Add deferral rate
  donation <- donation %>%
    mutate(Hb_deferral=case_when(
      Hb < Hb_cutoff_male   & sex == 'male'   ~ 1,
      Hb < Hb_cutoff_female & sex == 'female' ~ 1,
      TRUE ~ 0
    ))
  print(table(donation$sex, as.factor(donation$Hb_deferral)))
  #donation$Hb_deferral <- as.integer(as.character(donation$Hb_deferral))   # Fix Hb_deferral everywhere !!!!!!!!!!!!!!!!!

  # donation <- donation %>% 
  #   select(-c("first",    "family",   "language")) #%>% 
  

  donation <- donation %>% mutate(dateonly = date(date))
  
  # Drop donors whose Hb is NA
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  donation <- donation %>%
    filter(!is.na(Hb))
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) whose Hb is NA\n", 
                 old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)
  
  # Find the number of tries per day, and select the last try of the day
  old_count <- nrow(donation); old_count2 <- ndonor(donation)
  donation <- donation %>% 
    group_by(donor, dateonly) %>% 
    mutate(triesOnTheDay=n()) %>% 
    filter(max(date) == date) %>% 
    ungroup()
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because only last try of the day is selected\n", 
             old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)

  time_window_end <- max(donation$dateonly)
  time_window_start <- time_window_end - lubridate::dyears(5)

  # Drop old donations
  if (restrict_time_window) {
    old_count <- nrow(donation); old_count2 <- ndonor(donation)
    donation <- donation %>%
      filter(dateonly >= time_window_start)
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because donation happened before date %s\n", 
                   old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2, time_window_start)
    message(msg)
    print(logger, msg)
  }

  # Find the first donation ('imputed_first') of each donor
  donation <- donation %>%
    group_by(donor) %>%
    mutate(imputed_first = first(dateonly)) %>% # get a single value not vector
    ungroup()
    
  if (FALSE) {
    # Select only those donors whose first blood donation is close to the date as progesa's date_first_donation tells
    old_count <- nrow(donation); old_count2 <- ndonor(donation)
    donation <- donation %>%
      group_by(donor) %>%
      mutate(given_first = min(date_first_donation, na.rm=TRUE),
             difference = as.numeric(imputed_first - given_first)) %>% # get a single value not vector
      ungroup() %>%
      filter(0 <= difference, difference <= max_diff_date_first_donation )
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because the given date_first_donation was not the oldest donation for that donor\n", 
                   old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
    message(msg)
    print(logger, msg)
  }
  
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
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) whose first Hb is NA\n", 
              old_count - nrow(donation), old_count, old_count2 - ndonor(donation), old_count2)
  message(msg)
  print(logger, msg)
  
  data <- donation
  rm(donation)




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
  
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  n <- nrow(data %>% filter(first_event==TRUE & !(donat_phleb == 'K' | donat_phleb == '*')))
  message(sprintf("There are %i first donations with donat_phleb being neither 'K' nor '*'\n", n))
  data <- data %>%
    filter(donat_phleb == 'K' | donat_phleb == '*')
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because donat_phleb was not 'K' nor '*'\n", 
                 old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  message(msg)
  print(logger, msg)
  
  # Drop donor if his first donation got deleted
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>%
    group_by(donor) %>%
    filter(any(first_event)) %>%
    ungroup()
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because first event of a donor got deleted because it wasn't 'K' or '*'\n", 
                 old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  message(msg)
  print(logger, msg)
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>%
    filter(first_event==TRUE | (!is.na(days_to_previous_fb) & !is.na(Hb)))
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because days_to_previous_fb was NA for a non-first donation\n", 
                 old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  message(msg)
  print(logger, msg)
  
  
  # Assumes Hb_deferral is never NA
  consecutive_deferrals_f <- function(Hb_deferral) {
    c <- cumsum(Hb_deferral)
    c_at_previous_non_deferral <- ifelse(Hb_deferral==1, NA, c)
    if (is.na(c_at_previous_non_deferral[1])) {
      c_at_previous_non_deferral[1] <- 0
    }
    c_at_previous_non_deferral <- fill(enframe(c_at_previous_non_deferral), value)$value
    consecutive_deferrals <- lag(c - c_at_previous_non_deferral, default=0)
    return(consecutive_deferrals)
  }
  
  
  #print(head(data))
  message("Before Hb computations\n")
  tic("Previous and first Hb, previous_Hb_def, and amount of dererrals")
  # Get previous and first Hb values, previous_Hb_def, and amount of deferrals since last succesful donation event
  
  data <- data %>%
    group_by(donor) %>%
    mutate(previous_Hb_def = lag(Hb_deferral, default = NA),
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
    #mutate(warm_season = as.logical(unlist(lapply(month(dateonly), FUN = get_season))))
    mutate(warm_season = get_season(month(dateonly), {{southern_hemisphere}}))
  toc()
  
  # Fix values where hour is 0
  #hour.mean <- mean(data$hour)
  data <- mutate(data, hour = ifelse(hour == 0, mean(data$hour), hour))
  

  
  tic("Two year donations/deferrals")
  x_year_sliding_window_sum <- function(weight, date, x=2) {
    v <- as.numeric(slider::slide_index(weight, date, sum, .before = lubridate::dyears(x))) # years(2) had problems with leap days
    return(v)
  }
  
  data <- data %>%
    mutate(weight_donation=ifelse(donat_phleb == "K", 1, 0),
           Hb_deferral = Hb_deferral) %>%
    arrange(date) %>%
    group_by(donor) %>%
    mutate(recent_donations = x_year_sliding_window_sum(weight_donation, date, x=5)) %>%   # five years
    mutate(recent_deferrals = x_year_sliding_window_sum(Hb_deferral, date, x=2)) %>%
    ungroup() %>%
    mutate(recent_donations = as.integer(recent_donations-weight_donation),   # exclude the current donation from previous five years
           recent_deferrals = as.integer(recent_deferrals-Hb_deferral))       # exclude the current deferral from previous two years
  toc()
  
  # This isn't used anywhere !!!!!!!!!!!!!!!!
  # Add variable for how many times a donor has donated in the data
  # data <- data %>%
  #   group_by(donor) %>%
  #   mutate(times_donated = 1:n()) %>%
  #   ungroup()
  
  
  

  if (restrict_time_window) {
    any_donations_during_last_year <- function(dateonly, time_window_end) {
      year_ago <- time_window_end - lubridate::dyears(1)
      return(any(year_ago <= dateonly & dateonly <= time_window_end))
    }
    old_count <- nrow(data); old_count2 <- ndonor(data)
    data <- data %>%
      group_by(donor) %>%
      filter(any_donations_during_last_year(dateonly, time_window_end)) %>%
      ungroup()
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because there was no donation attempt in the year before %s\n", 
                   old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2, time_window_end)
    message(msg)
    print(logger, msg)
  }
  
  # old_count <- nrow(data); old_count2 <- ndonor(data)
  # data <- data %>%
  #   filter(year(dateonly) <= 2020)
  # msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because donation year was more recent than 2020\n", 
  #                old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  # message(msg)
  # print(logger, msg)
  
  possibly_drop_last <- function(g, donor) {
    n <- nrow(g)
    if (n > 1 && g[[n-1, "Hb_deferral"]] == TRUE && !is.na(g[[n-1, "volume_drawn"]]) && g[[n-1, "volume_drawn"]] > 100) {
      return(head(g, n=n-1))
    } else {
      return(g)
    }
  }
  
  # This is for Belgian data
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data2 <- data %>%
    group_by(donor) %>%
    arrange(dateonly) %>%
    group_map(possibly_drop_last, .keep=TRUE) %>%
    bind_rows()
  # Get the time series from which the last donation would be dropped
  bad_donors <- setdiff(data, data2)$donor
  bad_data <- data %>% filter(donor %in% bad_donors)
  saveRDS(bad_data, "/tmp/bad_data.rds")
  data <- data2
  rm(data2)
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) because second last event had low hb but volume drawn > 100\n", 
                 old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  message(msg)
  print(logger, msg)
  
  if (compute_donation_counts) {
    data <- data %>% group_by(donor) %>% mutate(nb_donat_progesa = n()) %>% ungroup()
  }
  
  # Select only the interesting variables, rename some of them and change the types
  variables <- c("don_id", "donor", "Hb", "dateonly", "previous_Hb_def", "days_to_previous_fb", "donat_phleb", "sex", "age",
                 "Hb_deferral", "nb_donat_progesa", "nb_donat_outside",
                 "first_event", "previous_Hb", "year", "warm_season", "Hb_first", "hour", "consecutive_deferrals", "recent_donations",
                 "recent_deferrals")
  # Keep label column, if it is in the input
  variables <- c(variables, intersect(names(data), "label"))
  tic("Final selection")
  data <- data %>%
    mutate(don_id = as.factor(don_id), 
           donor = donor, 
           Hb_deferral = as.logical(Hb_deferral),
           previous_Hb_def = as.logical(previous_Hb_def),
           consecutive_deferrals = as.integer(consecutive_deferrals),
           nb_donat_progesa = as.integer(nb_donat_progesa),
           nb_donat_outside = as.integer(nb_donat_outside)) %>%
    select(!!!variables) %>%
    arrange(donor)
  toc()
  
  message(sprintf("Final preprocessed data has %i donations and %i donors\n", nrow(data), ndonor(data)))
  invisible(data)
  return(data)
}



# Compare two vectors so that NA has no special rules
compare_vectors <- function(f1, f2) {
  m1 <- f1 == f2
  m2 <- is.na(f1) & is.na(f2)
  m <- m1 | m2   # NA == NA is TRUE, NA == TRUE is TRUE, NA = FALSE is NA
  m[is.na(m)] = FALSE  # convert NA to FALSE
  return(m)
}

# Returns a boolean matrix whether the two dataframes differ, with 
# - as many columns as there are common fields
# - as many rows as the inner join by the 'by' field gives
myequal2 <- function(df1, df2, by="donation") {
  fields_only_in_first <-  setdiff(colnames(df1), colnames(df2))
  fields_only_in_second <- setdiff(colnames(df2), colnames(df1))
  if (length(fields_only_in_first) > 0)
    cat("Fields only in the first:", fields_only_in_first, "\n")
  if (length(fields_only_in_second) > 0)
    cat("Fields only in the second:", fields_only_in_second, "\n")
  common_fields <- intersect(colnames(df1), colnames(df2))
  joined <- inner_join(df1, df2, by)
  for (field in setdiff(common_fields, by)) {
    f1 <- paste0(field,".x")
    f2 <- paste0(field,".y")
    cat("Comparing fields", f1, "and", f2, "\n")
    joined[field] <- !compare_vectors(joined[f1], joined[f2])
  }
  return(joined %>% select(all_of(common_fields)))
}

# Join dataframes by the 'by' column and place common fields next to each other.
# Optionally, restrict to rows where donation %in% values.
myjoin <- function(df1, df2, by="donation", values=NULL) {
  common_fields <- intersect(colnames(df1), colnames(df2))
  if (!is.null(values)) {
    df1 <- semi_join(df1, values)
  }
  joined <- inner_join(df1, df2, by)
  
  helper <- function(field) {
    f1 <- paste0(field, ".x")
    f2 <- paste0(field, ".y")
    if (all(compare_vectors(joined[f1], joined[f2]))) {
      return(f1)
    } else {
      return(c(f1, f2))
    }
  }
  common_fields2 <- setdiff(common_fields, by)
  new_fields <- map(common_fields2, helper)
  
  new_fields <- c(by, flatten_chr(new_fields))
  
  #print(new_fields)
  joined <- joined %>% select(all_of(new_fields))
  L <- character()
  for (field in common_fields2) {
    f1 <- paste0(field, ".x")
    f2 <- paste0(field, ".y")
    #print(f1)
    #print(f2)
    #print(field)
    
    if (!(f2 %in% new_fields)) {
      L <- c(L,f1)
      
    }
  }
  names(L) <- str_replace(L, "\\.x$", "")
  #print(L)
  joined <- joined %>% rename(all_of(L)) # Remove .x from the end of names of fields are that equal in both tables
  
  return(joined)
}



get_object_sizes <- function(e = rlang::global_env()) {
  s <- lobstr::obj_sizes(!!!as.list(e))
  t <- tibble(object=names(s), bytes=as.numeric(s), isfunction=map_lgl(object, ~ is.function(get(.x, envir = e))))
  return(arrange(t, isfunction, desc(bytes)))
}

preprocess <- function(donations, donors, Hb_cutoff_male = 135, Hb_cutoff_female = 125, Hb_input_unit = "gperl",
                       southern_hemisphere=FALSE, max_diff_date_first_donation, restrict_time_window=TRUE, cores=1, logger) {
#  tic()
  tic()
  if (is.character(donations)) {   # is a filename instead of a dataframe?
    donations <- read_donations(donations)
  }
  
  if (is.character(donors)) {   # is a filename instead of a dataframe?
    donors <- read_donors(donors)
  }
  
  # For testing purposes
  #cat(paste(format(get_object_sizes(rlang::current_env()), n=Inf), collapse="\n"))
  
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

preprocess_helper <- function(dir, Hb_cutoff_male = 135, Hb_cutoff_female = 125, Hb_input_unit) {
  tic()
  tic()
  donation.file <- paste0(dir,"/FRC.DW_DONATION.dat")
  donor.file <- paste0(dir,"/FRC.DW_DONOR.dat")
  preprocess(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female, Hb_input_unit)
  toc()
  toc()
  return(data)
}

sample_raw_progesa <- function(donations, donors, donation.out = NULL, donor.out = NULL, ndonor = 1.0) {
  
  if (is.character(donations)) {   # is a filename instead of a dataframe?
    donations <- read_donations(donations)
  }
  
  if (is.character(donors)) {   # is a filename instead of a dataframe?
    donors <- read_donors(donors)
  }
  
  cat(sprintf("Sampling to %f\n", ndonor))
  if (ndonor > 1.0) {   # is a count instead of proportion?
    donors <- slice_sample(donors, n=ndonor)
  } else {
    donors <- slice_sample(donors, prop=ndonor)
  }
  donor_ids <- donors$donor
  
  if (ndonor != 1.0)
    donations <- donations %>% filter(KEY_DONOR %in% donor_ids)  
  #donation <- donation %>% filter(donor %in% donor_ids)  
  
  
  if (!is.null(donation.out)) {
    write_delim(donations, donation.out, delim="|", col_names = TRUE)
    cat(sprintf("Wrote %i rows to file %s\n", nrow(donations), donation.out))
  }
  if (!is.null(donor.out)) {
    write_delim(donors, donor.out, delim="|", col_names = TRUE)
    cat(sprintf("Wrote %i rows to file %s\n", nrow(donors), donor.out))
  }
  
  return(list(donations=donations, donors=donors))
}


