suppressPackageStartupMessages(library(slider, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
suppressPackageStartupMessages(library(tictoc, quietly = TRUE))

source("helper_functions.R")  # For hours_to_numeric

mycounter=0

freadFRC <- function(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female)
{
  
  
  
  ########## DONATION
  # In the full dataset there lots of missing values. This causes automatic recognition of column types to fail.
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

  donation <- read_delim(donation.file, col_names=FALSE, delim='|', col_types=input_col_types)
  names(donation)=c('donation', 'donor', 'site', 'date', 'phleb_start',
                    'status', 'donat_phleb', 
                    'directed', 'donStartTime', 'volume_drawn', 'index_test', 
                    'Hb')
  print(head(donation))
  donation <- donation %>%
    mutate(donat_phleb = as.factor(donat_phleb),
           volume_drawn = as.integer(volume_drawn),
           Hb = as.numeric(Hb),
           phleb_start = as.character(phleb_start),
           donStartTime = as.integer(donStartTime))

  
  donation <- donation %>% filter(!(is.na(date) | is.na(phleb_start)))
  mytemp <- ymd_hm(paste0(as.character(donation$date)," ",donation$phleb_start))
  mm <- is.na(mytemp)
  cat("Failed to parse dates:", sum(mm), "\n")
  if (sum(mm) > 0) {
      print(donation %>% filter(mm) %>% head(20))
  }
  donation$date <- mytemp
  
  #print(summary(donation))
  yn=grep("^Y\\d{14}.$", as.vector(donation[["donation"]]),perl=TRUE) #The last one can be any character. #data.table way
  #But the ones used in luhti have 15 chars?
  cat("Dropping", nrow(donation) - length(yn), "from", nrow(donation), "due to badly formed ID\n")
  bad <- unique(as.character(donation$donation)[-yn])
  cat("Bad ones look like this:\n",head(bad),"...",tail(bad),"\n")
  donation <- donation[yn,]
  
  #ADD THE PARSING OF volume_drawn TO A NUMBER
  
  ######### DONOR
  donor <- read_delim(donor.file, col_names=FALSE, delim="|")
  names(donor)=c('donor','first','family', 'gender', 'dob', 'language', 'aborh', 'address', 'zip', 'city',
                 'tel','email', 'mobile',
                 'notifiable', 'notification_method_1', 'notification_method_2', 'notification_method_3', 
                 'nb_donations', 'nb_donat_progesa', 'nb_donat_outside', 
                 'date_first_donation', 'nb_wb', 'nb_pla',
                 'nb_thr', 'last_donat_phleb', 'last_collect'
                 
                 )
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
  
  donor2 <- donor %>% 
    select(donor,first,family,gender,dob,language,aborh,zip,city,date_first_donation, nb_donat_progesa, nb_donat_outside) %>%
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
  #print(c(nrow(donation2), nrow(donation)))
  #print(setdiff(donation2$donation, donation$donation))
  stopifnot(nrow(donation2)==nrow(donation))
  donation <- donation2
  
  print(table(donation$gender))
  #English sex
  levels(donation$gender) = c('Men','Women')

  #Sort
  donation <- donation %>% arrange(date)  
  #Add the 13 char string
  donation <- donation %>%
      mutate(donation13 = str_sub(donation, 1, 13),
             donation = as.factor(donation),
             donor = as.factor(donor))
  
  #Drop cases where date and dob are identical
  ids <- paste0(year(donation$date), month(donation$date), day(donation$date)) == paste0(year(donation$dob), month(donation$dob), day(donation$dob))
  cat("Dropping", table(ids)['TRUE'],"from",nrow(donation),"due to indentical date and dob \n")
  donation <- donation[!ids,]
  
  #Drop cases where either date or dob is NA
  m <- is.na(donation$date) | is.na(donation$dob)
  cat("Dropping", sum(m), "from", nrow(donation), "due to date or dob being NA\n")
  donation <- donation %>% filter(!m)

  #Drop cases where date is "19390101"
  ids <- paste0(year(donation$date),month(donation$date),day(donation$date)) ==  "193911"
  cat("Dropping", table(ids)['TRUE'],"from",nrow(donation),"due to date \"19390101\" \n")
  donation <- donation[!ids,]
  
  # Drop cases where date_first_donation is NA
  cat(sprintf("Dropping %i donations from %i because date_first_donation is not known.\n", sum(is.na(donation$date_first_donation)), nrow(donation)))
  donation <- donation %>%
    filter(!is.na(date_first_donation))
  
  #make age at time of donation
  #https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
  age <- as.period(interval(start = donation$dob, end = donation$date))$year
  donation$age <- age
  
  #Drop anything of age below 18
  ids <- donation$age < 18
  cat("Dropping", table(ids)['TRUE'],"from",nrow(donation),"due to age at time of donation below 18 \n")
  donation <- donation[!ids,]
  
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

  donation <- donation %>% 
    select(-c("first",    "family",   "language")) #%>% 
  

  donation <- donation %>% mutate(dateonly = date(date))
  
  # Find the number of tries per day, and select the last try of the day
  donation <- donation %>% 
    group_by(donor, dateonly) %>% 
    mutate(triesOnTheDay=n()) %>% 
    filter(max(date) == date) %>% 
    ungroup()
  

  # If first donation date is not given, then impute it from data.
  donation <- donation %>%
    group_by(donor) %>%
    mutate(imputed_first = min(dateonly)) %>%
    mutate(given_first = min(date_first_donation)) %>% # get a single value not vector
    filter(given_first == imputed_first) %>%
    ungroup()

  donation <- donation %>%
    mutate(first_event = dateonly==date_first_donation)
  
  # Drop donors whose first Hb is NA
  bad_donors <- donation %>%
    filter(first_event==TRUE & is.na(Hb)) %>%
    .$donor
  donation <- donation %>%
    filter(!(donor %in% bad_donors))
  
  #print(donation)
  #print(str(donation))
  invisible(donation)
}





decorateData <- function(data) {
  

  

  
  #Take all donations events (reagrdless of type)
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
    #mutate(days_to_previous_fb_jarkko = ifelse(is.na(days_to_previous_fb_jarkko), 0, days_to_previous_fb_jarkko))
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
  data <- data %>%
    group_by(donor) %>%
    mutate(times_donated = 1:n()) %>%
    ungroup()
  
  data <- data %>%
    filter(donat_phleb == 'K' | donat_phleb == '*') %>%
    filter(!(first_event==FALSE & (is.na(days_to_previous_fb) | is.na(Hb))))
  
  
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



preprocess <- function(donation.file, donor.file, Hb_cutoff_male = 135, Hb_cutoff_female = 125) {
  tic()
  tic()
  data <- freadFRC(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female)
  toc()
  tic()
  data <- decorateData(data)
  toc()
  toc()
  return(data)
}

preprocess_helper <- function(dir, Hb_cutoff_male = 135, Hb_cutoff_female = 125) {
  tic()
  tic()
  donation.file <- paste0(dir,"/FRC.DW_DONATION.dat")
  donor.file <- paste0(dir,"/FRC.DW_DONOR.dat")
  data <- freadFRC(donation.file, donor.file, Hb_cutoff_male, Hb_cutoff_female)
  toc()
  tic()
  data <- decorateData(data)
  toc()
  toc()
  return(data)
}

