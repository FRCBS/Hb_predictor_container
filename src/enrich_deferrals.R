#!/usr/bin/env Rscript

usage="usage:
enrich_deferrals.R inputfile [target_fraction] [outputfile]

If only the inputfile is given, then information about deferrals is printed.
Target fraction is the wanted fraction of sometime deferred donors.
This is achieved by dropping some donors that have never had deferrals.
If outputfile is given, then the new dataset is written to it.  
"
suppressPackageStartupMessages(library(tidyverse, quietly = T))

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
df <- df %>% mutate(donor = as.character(donor))

set.seed(56)  # Initialize random number generator

get_deferrals <- function(df) {
  donor <- df %>% group_by(donor) %>% summarise(Hb_deferral_max = max(Hb_deferral))
  never_deferred <- donor %>% filter(Hb_deferral_max==0) %>% select(donor)
  sometime_deferred <- donor %>% filter(Hb_deferral_max==1) %>% select(donor)
  
  n_donor <- nrow(donor)
  n_deferred_donor <- nrow(sometime_deferred)
  cat(sprintf("Donors with at least one deferral %i/%i (%.2f%%)\n", n_deferred_donor, n_donor, 100*n_deferred_donor/n_donor))
  
  n_donation <- nrow(df)
  n_deferred_donation <- nrow(df %>% filter(Hb_deferral==1))
  cat(sprintf("Donations deferred %i/%i (%.2f%%)\n", n_deferred_donation, n_donation, 100*n_deferred_donation/n_donation))
  return(list(sometime_deferred=sometime_deferred, never_deferred=never_deferred))
}

lst <- get_deferrals(df)

if (!is.null(target_fraction) && !is.na(target_fraction)) {
  sometime_deferred <- lst[["sometime_deferred"]]$donor
  never_deferred <- lst[["never_deferred"]]$donor
  n_sometime_deferred <- length(sometime_deferred)
  n_never_deferred <- length(never_deferred)
  #print(n_sometime_deferred)
  #print(n_never_deferred)
  #print(target_fraction)
  n <- as.integer(n_sometime_deferred / target_fraction - n_sometime_deferred)
  #print(n)
  never_deferred <- sample(never_deferred, n)
  #print(never_deferred)
  df2 <- df %>% filter(donor %in% c(sometime_deferred, never_deferred))
  
  cat("After dropping some random donors with no deferrals:\n")
  get_deferrals(df2)
  df2 <- df2 %>% mutate(donor = as.factor(donor))
  
  if (!is.na(output_filename)) {
    fulldata_preprocessed <- df2
    save(fulldata_preprocessed, file=output_filename)
    cat(sprintf("Saved the new dataset to file %s\n", output_filename))
  }
}
