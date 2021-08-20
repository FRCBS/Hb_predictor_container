#!/usr/bin/env Rscript

# Jarkko Toivonen 2020
# Enrich data for random forest fitting.
# From donors with deferrals, drop last accepted donations
# so that the last donation is deferral.
# Drop donors without deferrals until given percentage
# of donors have at least one deferral.

usage="usage:
enrich_deferrals_rf.R inputfile [target_fraction] [outputfile]

If only the inputfile is given, then information about deferrals is printed.
Target fraction is the wanted fraction of donors with deferral as last event.
This is achieved by dropping some donors that have never had deferrals, and
by dropping last non-deferral events from donors.
If outputfile is given, then the new dataset is written to it.  
"
suppressPackageStartupMessages(library(tidyverse, quietly = T))



trim_time_series <- function(df) {
  df <- df %>% mutate(donor = as.character(donor))
  
  # Drop donors with only one donation
  df <- df %>% 
    group_by(donor) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Drop donors who have their only deferral as the first event
  df <- df %>% 
    group_by(donor) %>%
    arrange(dateonly) %>%
    filter((sum(Hb_deferral) != 1) | (first(Hb_deferral) != TRUE)) %>%
    ungroup()
  
  if (nrow(df) == 0) return(df)
  
  set.seed(56)  # Initialize random number generator  
  
  # Get donors without deferrals
  without_deferrals <- df %>% 
    group_by(donor) %>%
    filter(max(Hb_deferral)==0) %>%   # TOIMIIKO TÄMÄ OIKEIN?
    ungroup()
  
  # Trim accepted donations from the time series
  with_deferrals <- df %>% 
    arrange(donor, dateonly) %>%
    group_by(donor) %>%
    filter(max(Hb_deferral) > 0) %>%
    ungroup()
  if (nrow(with_deferrals) > 0) {
    with_deferrals <- with_deferrals %>% 
      arrange(donor, dateonly) %>%
      group_by(donor) %>%
      slice(1:max(which(Hb_deferral))) %>%
      ungroup()
  }

  df <- rbind(without_deferrals, with_deferrals)
  return(df)
}

# Returns two data frames. First contains those donors that have at least one deferral.
# The second contains those donors that have never had deferrals.
get_deferrals <- function(df) {
  donor <- df %>% group_by(donor) %>% summarise(Hb_deferral_max = max(Hb_deferral))  # does donor have deferrals?
  never_deferred <- donor %>% filter(Hb_deferral_max==0) %>% select(donor)
  sometime_deferred <- donor %>% filter(Hb_deferral_max==1) %>% select(donor)
  
  n_donor <- nrow(donor)
  n_deferred_donor <- nrow(sometime_deferred)
  message(sprintf("Donors with at least one deferral %i/%i (%.2f%%)\n", n_deferred_donor, n_donor, 100*n_deferred_donor/n_donor))
  
  n_donation <- nrow(df)
  n_deferred_donation <- nrow(df %>% filter(Hb_deferral==1))
  message(sprintf("Donations deferred %i/%i (%.2f%%)\n", n_deferred_donation, n_donation, 100*n_deferred_donation/n_donation))
  return(list(sometime_deferred=sometime_deferred, never_deferred=never_deferred))
}

# Try make the fraction of donors with and without deferrals to be approximately equal to 'target_fraction'.
# This is achieved to dropping donor.
# Input is a dataframe and target_fraction is a real number in the range [0, 1].
# Returns a dataframe.
balance_classes <- function(df, target_fraction) {
  stopifnot(typeof(df$donor) == "character")
  lst <- get_deferrals(df)
  sometime_deferred <- lst[["sometime_deferred"]]$donor
  never_deferred <- lst[["never_deferred"]]$donor
  n_sometime_deferred <- length(sometime_deferred)
  n_never_deferred <- length(never_deferred)
  message(sprintf("nrow(sometime_deferred)=%i, nrow(never_deferred)=%i, target_fraction=%f", nrow(sometime_deferred), nrow(never_deferred), target_fraction))

  if (target_fraction == 0.0) {
    df2 <- df %>% filter(donor %in% never_deferred)
    get_deferrals(df2)
    return(df2 %>% mutate(donor = as.factor(donor)))
  }
  ratio <- (1 - target_fraction) / target_fraction
  n <- as.integer(n_sometime_deferred * ratio)
  if (n <= n_never_deferred) {
    never_deferred <- sample(never_deferred, n)
    message(sprintf("Dropping %i random donors with no deferrals:\n", n))
  } else {
    n <- as.integer(n_never_deferred / ratio)
    sometime_deferred <- sample(sometime_deferred, n)
    message(sprintf("Dropping %i random donors with deferrals:\n", n))
  }
  df2 <- df %>% filter(donor %in% c(sometime_deferred, never_deferred))
  
  get_deferrals(df2)
  df2 <- df2 %>% mutate(donor = as.factor(donor))
  return (df2)  
}

enrich_deferrals_rf <- function(df, target_fraction) {
  #save(df, file="/tmp/taulu.rdata")
  df <- trim_time_series(df)
  enriched <- balance_classes(df, target_fraction)
  return(enriched)  
}

if (sys.nframe() == 0L) {  # This is a trick to find out whether the file was executed or sourced
  message("Script executed!\n")
  args = commandArgs(TRUE)
  
  if (length(args) < 1 | length(args) > 3) {
    stop(paste("Wrong number of parameters!", usage, sep="\n"))
  }
  
  inputfilename <- args[1]
  target_fraction <- as.numeric(args[2])
  #target_fraction <- target_percentage/100
  output_filename <- args[3]
  
  if (!is.na(target_fraction) && !(0.0 <= target_fraction && target_fraction <= 1.0)) {
    stop(paste("The target fraction should be between 0.0 and 1.0",
               usage, sep="\n"))
  }
  
  df <- get(load(inputfilename))
  
  if (!is.null(target_fraction) && !is.na(target_fraction)) {
    enriched <- enrich_deferrals_rf(df, target_fraction)
    if (!is.na(output_filename)) {
      save(enriched, file=output_filename)
      cat(sprintf("Saved the new dataset to file %s\n", output_filename))
    }
  } else {
    df <- trim_time_series(df)
    lst <- get_deferrals(df)
  }
} else {
  message("Script sourced!\n")
}

