library(tidyverse)
library(rstan)
library(caret)

use_new_method = TRUE;

# This file will open the nescessary data files and create Rdump and RData
# files that can be used in the stan-analyses
# Rdumps are used by cmdstan and RDatas by Rstan

# The Rdata files will be small subsets to make quick comparisons between
# the models in R and Rdump files will have all of the data.

# At the moment the RData files this will generate are:
# 1. Model 1, no icp (separate lists for male and female)
# 2. Model 1, icp-fix (separate list for male and female)
# 3. Model 2, no icp (separate list for male and female)
# 4. Model 2, icp-fix (separate list for male and female)
# 5. Model 3, no icp (separate list for male and female)
# 6. Model 3, icp-fix (separate list for male and female)

# These Rdump files will be generated
# 1. Model 1, no icp, male donors
# 2. Model 1, no icp, female donors
# 3. Model 1, icp, male donors
# 4. Model 1, icp, female donors
# 5. Model 2, no icp, male donors   
# 6. Model 2, no icp, female donors
# 7. Model 2, icp, male donors
# 8. Model 2, icp, female donors
# 9. Model 3, no icp, male donors
# 10. Model 3, no icp, female donors
# 11. Model 3, icp-fix, male donors
# 12. Model 3, icp-fix, female donors

# Helper function for creating stan-lists:
subset_analyses_create_stan_list <- function(df, slopevar = NULL, icpfix = FALSE, rdump = FALSE, filename = NULL, dumpdir = NULL, threshold = NULL, date=NULL) {
  # Takes output of stan_preprocess or stan_preprocess_icp as input
  
  # Create the list without slope
  if (is.null(slopevar)) {
    x_train <- df$x_train
    y_train <- df$y_train
    x_test <- unname(as.matrix(df$x_test))
    y_test <- df$y_test
    if (use_new_method) {
      train_donors <- df$train_donors
      test_donors <- df$test_donors
    } else {
      train_donors <- df$train_dons
      test_donors <- df$test_dons
    }
    C <- df$C
    M = ncol(C)
    first_events = df$first_events
  } else {
    # Create the list with given slope variable
    x_train <- df$x_train
    slope_train <- pull(x_train, slopevar)
    x_train <- select(x_train, -slopevar)
    
    y_train <- df$y_train
    
    x_test <- df$x_test
    slope_test <- pull(x_test, slopevar)
    x_test <- select(x_test, -slopevar)
    x_test <- unname(as.matrix(x_test))
    
    y_test <- df$y_test
    if (use_new_method) {
      train_donors <- df$train_donors
      test_donors <- df$test_donors
    } else {
      train_donors <- df$train_dons
      test_donors <- df$test_dons
    }
    C <- df$C
    M = ncol(C)
    first_events = df$first_events
  }
  #x_train <- x_train %>% select(-don_id, -gender)
  # All models use QR-reparametrizarion so it will be done here
  tryCatch(
    error = function(cnd) {
      s <- capture.output(summary(x_train))
      s <- paste(s, collapse="\n")
      message(s)
      constants <- colnames(mutate_all(x_train, as.numeric) %>% keep(function(c) sd(c)==0.0))
      cnd$message <- paste("\nError in qr_decomposition of x_train matrix:", cnd$message, 
                           "The following variables are constant:", paste(constants, collapse=","), sep="\n")
      stop(cnd)
    },
    {
      qr0 <- qr_decomposition(x_train)
    }
  )

  
  stanlist <- list(N = nrow(x_train),
                   K = ncol(x_train),
                   C = C,
                   M = M,
                   Ndon = length(unique(train_donors)),
                   Q_star = qr0$Q_star,
                   R_star = qr0$R_star,
                   R_star_inv = qr0$R_star_inv,
                   Hb = y_train,
                   donor = train_donors,
                   Ntest = nrow(x_test),
                   #Ntest_don = length(unique(test_dons)),
                   x_test = x_test,
                   test_donor = test_donors)
  if (icpfix) {
    Z <- df$Z
    Hb0 <- df$Hb0
    stanlist <- c(stanlist, list(L = ncol(Z), Z = Z, Hb_0 = unname(as.numeric(unlist(Hb0))), first_events=first_events))
  }
  # Check if slope variable has to be added to the list
  if (!is.null(slopevar)) {
    stanlist$x1 <- slope_train
    stanlist$x_1_test <- slope_test
    stanlist$x_2_test <- x_test
    stanlist$x_test <- NULL
  }
  
  if (rdump == FALSE) {return(stanlist)}
  else {
    N = nrow(x_train)
    K = ncol(x_train)
    Ndon = length(unique(train_dons))
    Q_star = qr0$Q_star
    R_star = qr0$R_star
    R_star_inv = qr0$R_star_inv
    Hb = y_train
    donor = train_dons
    Ntest = nrow(x_test)
    #Ntest_don = length(unique(test_dons))
    x_test = x_test
    test_donor = test_dons
    threshold = threshold
    if (icpfix == TRUE) {
      L = ncol(Z)
      Z = Z
      #Hb_0 = unname(as.numeric(unlist(Hb0_train)))
      Hb_0 = unname(as.numeric(unlist(Hb0)))
      #Z_test = Z_test
      #Hb_0_test = unname(as.numeric(unlist(Hb0_test)))
    }
    if (!is.null(slopevar)) {
      x1 <- slope_train
      x_1_test <- slope_test
      x_2_test <- x_test
    }
    if (icpfix == TRUE & !is.null(slopevar)) {
      stan_rdump(c("N", "K", "Ndon", "L", "x1", "Q_star", "R_star", "R_star_inv", "Hb", "donor", "Z", "Hb_0", "Ntest", 
                  "x_1_test", "x_2_test", "test_donor", "threshold"),
                 file = paste(dumpdir,filename,date,".R", sep = ''))
    } else if (icpfix == TRUE) {
      stan_rdump(c("N", "K", "Ndon", "L", "Q_star", "R_star", "R_star_inv", "Hb", "donor", "Z", "Hb_0", "Ntest", 
                   "x_test", "test_donor","threshold"),
                 file = paste(dumpdir,filename,date,".R", sep = ''))
    } else if (!is.null(slopevar)) {
      stan_rdump(c("N", "K", "Ndon", "x1", "Q_star", "R_star", "R_star_inv", "Hb", "donor", "Ntest", 
                   "x_1_test", "x_2_test", "test_donor", "threshold"),
                 file = paste(dumpdir,filename,date,".R", sep = ''))
    } else {
      stan_rdump(c("N", "K", "Ndon","Q_star", "R_star", "R_star_inv", "Hb", "donor","Ntest", 
                   "x_test", "test_donor", "threshold"),
                 file = paste(dumpdir,filename,date,".R", sep = ''))      
    }
  }
}

drop_some_fields <- function(df) {
  #df <- df %>% select(-don_id, -gender, -previous_Hb)
  df <- df %>% select(-don_id, -gender)
  return(df)
}


create_datasets <- function(data, rdatadir, dumpdir, id, sample_fraction, hlen=NULL, hlen_exactly=FALSE, Hb_cutoff_male = 135, Hb_cutoff_female = 125, 
                            basic_variables, basic_variables_icp,
                            donor_variables=NULL,
                            compute_male_nofix, compute_female_nofix, compute_male_icpfix, compute_female_icpfix) {
  # Set the directory where the files will be saved:
  #dumpdir = "~/FRCBS/interval_prediction/data/rdump/"
  #rdatadir = "~/FRCBS/interval_prediction/data/rdata/"
  
  # Date or identifier value
  #date = "15_08"
  
  #setwd("~/FRCBS/interval_prediction/src/")
  source(file = "helper_functions.R")
  
  # Load the male and female datasets
  #load("../data/split_data_full.RData")
  #load("~/FRCBS/interval_prediction/data/full_data_preprocessed.rdata")
  #data <- fulldata_preprocessed
  
  set.seed(123)
  message(sprintf("Full dataset size: %i", ndonor(data)))

  temp <- data %>% 
    filter(is.na(Hb))   # There should not be any of these left
  stopifnot(nrow(temp) == 0)
  
  data <- data %>% select("Hb", everything())  # Move Hb to first column, because Yrjo's Hb_index stuff does not work
  data <- data %>% select(-nb_donat_progesa, -nb_donat_outside)  # Drop these as they contain NAs
  # Testing if this helps stop qr_decomposition from complaining
  # old_variables <- c("Hb", "donor", "days_to_previous_fb", "previous_Hb_def", "age", "year", "warm_season", "Hb_first", "hour", 
  #                    "consecutive_deferrals", "recent_donations", "recent_deferrals")
  #data <- data %>% select(all_of(old_variables))
  # Different datasets for male and female donors
  data.male <-
    data %>% filter(gender == "Men")
  message(sprintf("Male dataset size: %i", ndonor(data.male)))
  data.female <-
    data %>% filter(gender == "Women")
  message(sprintf("Female dataset size: %i", ndonor(data.female)))
  
  # Split for men and women separately
  
  data.male <- split_set(data.male, 1.0)
  data.female <- split_set(data.female, 1.0)
  
  names(data.male) <- c("general","validation")
  names(data.female) <- c("general","validation")
  message(sprintf("Male dataset size after pseudosplit: %i", ndonor(data.male$general)))
  message(sprintf("Female dataset size after pseudosplit: %i", ndonor(data.female$general)))
  
  
  # Small test sets
  smallm <- split_set(data.male$general, sample_fraction)
  smallf <- split_set(data.female$general, sample_fraction)
  message(sprintf("Male dataset size after sample_fraction split: %i", ndonor(smallm$train)))
  message(sprintf("Female dataset size after sample_fraction split: %i", ndonor(smallf$train)))

    
  # smallm.stan <- stan_preprocess(data.male, 0.8)
  # smallf.stan <- stan_preprocess(data.female, 0.8)
  # 
  # smallm.stan.icp <- stan_preprocess_icp(data.male, 0.8)
  # smallf.stan.icp <- stan_preprocess_icp(data.female, 0.8)

  stan_preprocessed_objects <- c()  
  if (!use_new_method) {
    smallm.stan <- stan_preprocess_le(smallm$train, hlen=hlen)
    smallf.stan <- stan_preprocess_le(smallf$train, hlen=hlen)
    smallm.stan.icp <- stan_preprocess_icp_le(smallm$train, hlen=hlen)
    smallf.stan.icp <- stan_preprocess_icp_le(smallf$train, hlen=hlen)
  } else {
    Hb_index <- which(colnames(data)=="Hb")
    stopifnot(Hb_index == 1)
    if (compute_male_nofix) {
      stan.preprocessed.male.nofix <- stan_preprocess_new(drop_some_fields(smallm$train) %>% select(-previous_Hb), 
                                                          Hb_index=Hb_index, hlen=hlen, hlen_exactly=hlen_exactly, 
                                                          basic_variables=basic_variables, donor_variables = donor_variables)
      stan_preprocessed_objects <- c(stan_preprocessed_objects, "stan.preprocessed.male.nofix")
    }
    if (compute_female_nofix) {
      stan.preprocessed.female.nofix <- stan_preprocess_new(drop_some_fields(smallf$train) %>% select(-previous_Hb), 
                                                            Hb_index=Hb_index, hlen=hlen, hlen_exactly=hlen_exactly, 
                                                            basic_variables=basic_variables, donor_variables = donor_variables)
      stan_preprocessed_objects <- c(stan_preprocessed_objects, "stan.preprocessed.female.nofix")
    }
    if (compute_male_icpfix) {
      stan.preprocessed.male.icpfix <- stan_preprocess_icp_new(drop_some_fields(smallm$train) %>% select(-Hb_first), 
                                                               Hb_index=Hb_index, hlen=hlen, hlen_exactly=hlen_exactly, 
                                                               basic_variables=basic_variables_icp, donor_variables = donor_variables)
      stan_preprocessed_objects <- c(stan_preprocessed_objects, "stan.preprocessed.male.icpfix")
    }
    if (compute_female_icpfix) {
      stan.preprocessed.female.icpfix <- stan_preprocess_icp_new(drop_some_fields(smallf$train) %>% select(-Hb_first), 
                                                                 Hb_index=Hb_index, hlen=hlen, hlen_exactly=hlen_exactly, 
                                                                 basic_variables=basic_variables_icp, donor_variables = donor_variables)
      stan_preprocessed_objects <- c(stan_preprocessed_objects, "stan.preprocessed.female.icpfix")
    }
  }
  stan_preprocessed_filename <- paste(rdatadir,"stan_preprocessed_datasets_", id, ".RData", sep = '')
  save(list=stan_preprocessed_objects, file = stan_preprocessed_filename)
  
  # Create Stan lists
  if (compute_male_nofix) {
    stan.lists.male.nofix <- subset_analyses_create_stan_list(stan.preprocessed.male.nofix)
    save(stan.lists.male.nofix, file = paste(rdatadir,"stan_lists_male_nofix_", id, ".RData", sep = ''))
    rm(stan.lists.male.nofix)
  }
  if (compute_female_nofix) {
    stan.lists.female.nofix <- subset_analyses_create_stan_list(stan.preprocessed.female.nofix)
    save(stan.lists.female.nofix, file = paste(rdatadir,"stan_lists_female_nofix_", id, ".RData", sep = ''))
    rm(stan.lists.female.nofix)
  }
  gc()
  
  if (compute_male_icpfix) {
    stan.lists.male.icpfix <- subset_analyses_create_stan_list(stan.preprocessed.male.icpfix, icpfix = TRUE)
    save(stan.lists.male.icpfix, file = paste(rdatadir,"stan_lists_male_icpfix_", id, ".RData", sep = ''))
    rm(stan.lists.male.icpfix)
  }
  if (compute_female_icpfix) {
    stan.lists.female.icpfix <- subset_analyses_create_stan_list(stan.preprocessed.female.icpfix, icpfix = TRUE)  
    save(stan.lists.female.icpfix, file = paste(rdatadir,"stan_lists_female_icpfix_", id, ".RData", sep = ''))
    rm(stan.lists.female.icpfix)
  }
  gc()
  

  

  return(list(small.data.male=smallm$train, small.data.female=smallf$train))  
}