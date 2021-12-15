library(tidyverse)
library(rstan)
library(caret)


# This file will open the necessary data files and create Rdump and RData
# files that can be used in the stan-analyses
# Rdumps are used by cmdstan and RDatas by Rstan

# The Rdata files will be small subsets to make quick comparisons between
# the models in R and Rdump files will have all of the data.

# At the moment the RData files this will generate are:
# 1. Model 1, no icp (separate lists for male and female)
# 2. Model 1, icp-fix (separate list for male and female)


# These Rdump files will be generated
# 1. Model 1, no icp, male donors
# 2. Model 1, no icp, female donors
# 3. Model 1, icp, male donors
# 4. Model 1, icp, female donors


# Helper function for creating stan-lists:
subset_analyses_create_stan_list <- function(df, slopevar = NULL, icpfix = FALSE, rdump = FALSE, filename = NULL, dumpdir = NULL, 
                                             threshold = NULL, date=NULL, out_of_sample_predictions=FALSE) {
  message("In subset_analyses_create_stan_list function")
  
  # Takes output of stan_preprocess or stan_preprocess_icp as input
  
  # Create the list without slope

  x_train <- df$x_train
  y_train <- df$y_train
  train_donors <- df$train_donors

  if (! out_of_sample_predictions) {
    x_test <- unname(as.matrix(df$x_test))
    y_test <- df$y_test
    test_donors <- df$test_donors
  }
  C <- df$C
  M = ncol(C)
  first_events = df$first_events

  #x_train <- x_train %>% select(-don_id, -sex)
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
                   donor = train_donors)
  
  if (! out_of_sample_predictions) {
    stanlist <- c(stanlist,
                  list(
                    Ntest = nrow(x_test),
                    #Ntest_don = length(unique(test_dons)),
                    x_test = x_test,
                    test_donor = test_donors))
  }
  
  if (icpfix) {
    Z <- df$Z
    #Hb0 <- df$Hb0
    stanlist <- c(stanlist, list(L = ncol(Z), Z = Z, #Hb_0 = unname(as.numeric(unlist(Hb0))), 
                                 first_events=first_events))
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
      ##Hb_0 = unname(as.numeric(unlist(Hb0_train)))
      #Hb_0 = unname(as.numeric(unlist(Hb0)))
      #Z_test = Z_test
      #Hb_0_test = unname(as.numeric(unlist(Hb0_test)))
    }
    # if (!is.null(slopevar)) {
    #   x1 <- slope_train
    #   x_1_test <- slope_test
    #   x_2_test <- x_test
    # }
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

drop_some_fields <- function(df, sex2) {
  message("In drop_some_fields function")
  #df <- df %>% select(-don_id, -sex, -previous_Hb)
  if (sex2=="both") {
    df <- df %>% select(-don_id, -label) %>% mutate(sex = ifelse(sex=="female", TRUE, FALSE))
  } else {
    df <- df %>% select(-don_id, -label, -sex)
  }
  return(df)
}


create_stan_datasets <- function(data, datadir, dumpdir, #id, #hlen=NULL, hlen_exactly=FALSE, 
                            Hb_cutoff_male = 135, Hb_cutoff_female = 125, 
                            basic_variables, basic_variables_dlmm,
                            donor_variables=NULL,
                            compute_lmm, compute_dlmm,
                            sex,
                            out_of_sample_predictions=FALSE) {
  message("In create_stan_datasets function")
  # Set the directory where the files will be saved:
  #dumpdir = "~/FRCBS/interval_prediction/data/rdump/"
  #rdatadir = "~/FRCBS/interval_prediction/data/rdata/"
  

  
  #setwd("~/FRCBS/interval_prediction/src/")
  source(file = "helper_functions.R")
  
  # Load the male and female datasets
  #load("../data/split_data_full.RData")
  #load("~/FRCBS/interval_prediction/data/full_data_preprocessed.rdata")
  #data <- fulldata_preprocessed
  
  #set.seed(123)
  message(sprintf("Full dataset size: %i", ndonor(data)))

  temp <- data %>% 
    filter(is.na(Hb))   # There should not be any of these left
  stopifnot(nrow(temp) == 0)
  
  data <- data %>% select("Hb", everything())  # Move Hb to first column, because Yrjo's Hb_index stuff does not work

  message(sprintf("%s dataset size: %i", sex, ndonor(data)))

  # Take a sample
  #small <- sample_set(data, sample_fraction)
  #message(sprintf("%s dataset size after sample_fraction split: %i", sex, ndonor(small)))
  small <- data

#  stan_preprocessed_objects <- c()  
  Hb_index <- which(colnames(data)=="Hb")
  stopifnot(Hb_index == 1)
  if (compute_lmm) {
    stan_preprocessed_filename <- sprintf("%s/stan_preprocessed_datasets_%s_%s.rds", datadir, "lmm", sex)
    stan.preprocessed.lmm <- stan_preprocess_new(drop_some_fields(small, sex) %>% select(-previous_Hb), 
                                                        Hb_index=Hb_index, #hlen=hlen, hlen_exactly=hlen_exactly, 
                                                        basic_variables=basic_variables, donor_variables = donor_variables, 
                                                 test_data = ! out_of_sample_predictions)
    #stan_preprocessed_objects <- c(stan_preprocessed_objects, "stan.preprocessed.lmm")
    saveRDS(stan.preprocessed.lmm, stan_preprocessed_filename)
  }
  if (compute_dlmm) {
    stan_preprocessed_filename <- sprintf("%s/stan_preprocessed_datasets_%s_%s.rds", datadir, "dlmm", sex)
    stan.preprocessed.dlmm <- stan_preprocess_icp_new(drop_some_fields(small, sex) %>% select(-Hb_first), 
                                                             Hb_index=Hb_index, #hlen=hlen, hlen_exactly=hlen_exactly, 
                                                             basic_variables=basic_variables_dlmm, donor_variables = donor_variables,
                                                      test_data = ! out_of_sample_predictions)
    saveRDS(stan.preprocessed.dlmm, stan_preprocessed_filename)
    #stan_preprocessed_objects <- c(stan_preprocessed_objects, "stan.preprocessed.dlmm")
  }
  #stan_preprocessed_filename <- paste(rdatadir,"stan_preprocessed_datasets_", id, ".RData", sep = '')
  #save(list=stan_preprocessed_objects, file = stan_preprocessed_filename)
  
  # Create Stan lists
  if (compute_lmm) {
    stan_lists_filename <- sprintf("%s/stan_lists_%s_%s.rds", datadir, "lmm", sex)
    
    stan.lists.lmm <- subset_analyses_create_stan_list(stan.preprocessed.lmm, out_of_sample_predictions = out_of_sample_predictions)
#    save(stan.lists.lmm, file = paste(rdatadir,"stan_lists_lmm_", id, ".RData", sep = ''))
    saveRDS(stan.lists.lmm, stan_lists_filename)
    rm(stan.lists.lmm)
  }
  gc()
  
  if (compute_dlmm) {
    stan_lists_filename <- sprintf("%s/stan_lists_%s_%s.rds", datadir, "dlmm", sex)
    stan.lists.dlmm <- subset_analyses_create_stan_list(stan.preprocessed.dlmm, icpfix = TRUE, out_of_sample_predictions = out_of_sample_predictions)
#    save(stan.lists.dlmm, file = paste(rdatadir,"stan_lists_dlmm_", id, ".RData", sep = ''))
    saveRDS(stan.lists.dlmm, stan_lists_filename)
    rm(stan.lists.dlmm)
  }
  gc()
  

  

  return(list(small.data=small))  
}