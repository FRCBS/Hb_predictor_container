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
  qr0 <- qr_decomposition(x_train)
  
  
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

create_datasets <- function(data, rdatadir, dumpdir, date, sample_fraction, hlen=NULL, Hb_cutoff_male = 135, Hb_cutoff_female = 125, donor_variables=NULL) {
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
  
  data <- data %>% 
    filter(!is.na(Hb))
  data <- data %>% select("Hb", everything())  # Move Hb to first column, because Yrjo's Hb_index stuff does not work
  data <- data %>% select(-nb_donat_progesa, -nb_donat_outside)  # Drop these as they contain NAs
  # Testing if this helps stop qr_decomposition from complaining
  # old_variables <- c("Hb", "donor", "days_to_previous_fb", "previous_Hb_def", "age", "year", "warm_season", "Hb_first", "hour", 
  #                    "consecutive_deferrals", "recent_donations", "recent_deferrals")
  #data <- data %>% select(all_of(old_variables))
  # Different datasets for male and female donors
  data.male <-
    data %>% filter(gender == "Men")
  
  data.female <-
    data %>% filter(gender == "Women")
  
  # Split for men and women separately
  
  data.male <- split_set(data.male, 1.0)
  data.female <- split_set(data.female, 1.0)
  
  names(data.male) <- c("general","validation")
  names(data.female) <- c("general","validation")
  
  
  
  # Small test sets
  smallm <- split_set(data.male$general, sample_fraction)
  smallf <- split_set(data.female$general, sample_fraction)
  
  # smallm.stan <- stan_preprocess(data.male, 0.8)
  # smallf.stan <- stan_preprocess(data.female, 0.8)
  # 
  # smallm.stan.icp <- stan_preprocess_icp(data.male, 0.8)
  # smallf.stan.icp <- stan_preprocess_icp(data.female, 0.8)
  
  if (!use_new_method) {
    smallm.stan <- stan_preprocess_le(smallm$train, hlen=hlen)
    smallf.stan <- stan_preprocess_le(smallf$train, hlen=hlen)
    smallm.stan.icp <- stan_preprocess_icp_le(smallm$train, hlen=hlen)
    smallf.stan.icp <- stan_preprocess_icp_le(smallf$train, hlen=hlen)
  } else {
    Hb_index <- which(colnames(data)=="Hb")
    stopifnot(Hb_index == 1)
    
    smallm.stan <- stan_preprocess_new(drop_some_fields(smallm$train) %>% select(-previous_Hb), Hb_index=Hb_index, hlen=hlen, donor_variables = donor_variables)
    smallf.stan <- stan_preprocess_new(drop_some_fields(smallf$train) %>% select(-previous_Hb), Hb_index=Hb_index, hlen=hlen, donor_variables = donor_variables)
    smallm.stan.icp <- stan_preprocess_icp_new(drop_some_fields(smallm$train) %>% select(-Hb_first), Hb_index=Hb_index, hlen=hlen, donor_variables = donor_variables)
    smallf.stan.icp <- stan_preprocess_icp_new(drop_some_fields(smallf$train) %>% select(-Hb_first), Hb_index=Hb_index, hlen=hlen, donor_variables = donor_variables)
    
    #    
  }
  
  save(smallm.stan, smallf.stan, smallm.stan.icp, smallf.stan.icp, file = paste(rdatadir,"small_stan_datasets_",date,".RData", sep = ''))
  
  # 1.
  smallm.sl.1 <- subset_analyses_create_stan_list(smallm.stan)
  smallf.sl.1 <- subset_analyses_create_stan_list(smallf.stan)
  save(smallm.sl.1, smallf.sl.1, file = paste(rdatadir,"small_model1_noicp_",date,".RData", sep = ''))
  rm(smallm.sl.1, smallf.sl.1)
  gc()
  
  # 2.
  smallm.sl.icp.1 <- subset_analyses_create_stan_list(smallm.stan.icp, icpfix = TRUE)
  smallf.sl.icp.1 <- subset_analyses_create_stan_list(smallf.stan.icp, icpfix = TRUE)
  save(smallm.sl.icp.1, smallf.sl.icp.1, file = paste(rdatadir,"small_model1_icp_",date,".RData", sep = ''))
  rm(smallm.sl.icp.1, smallf.sl.icp.1)
  gc()
  
  # # 3.
  # smallm.sl.2 <- subset_analyses_create_stan_list(smallm.stan, slopevar = "age")
  # smallf.sl.2 <- subset_analyses_create_stan_list(smallf.stan, slopevar = "age")
  # save(smallm.sl.2, smallf.sl.2, file = paste(rdatadir,"small_model2_noicp_",date,".RData", sep = ''))
  # rm(smallm.sl.2, smallf.sl.2)
  # gc()
  # 
  # # 4.
  # smallm.sl.icp.2 <- subset_analyses_create_stan_list(smallm.stan.icp, slopevar = "age", icpfix = TRUE)
  # smallf.sl.icp.2 <- subset_analyses_create_stan_list(smallf.stan.icp, slopevar = "age", icpfix = TRUE)
  # save(smallm.sl.icp.2, smallf.sl.icp.2, file = paste(rdatadir,"small_model2_icp_",date,".RData", sep = ''))
  # rm(smallm.sl.icp.2, smallf.sl.icp.2)
  # gc()
  # 
  # #5.
  # smallm.sl.3 <- subset_analyses_create_stan_list(smallm.stan, slopevar = "days_to_previous_fb")
  # smallf.sl.3 <- subset_analyses_create_stan_list(smallf.stan, slopevar = "days_to_previous_fb")
  # save(smallm.sl.3, smallf.sl.3, file = paste(rdatadir,"small_model3_noicp_",date,".RData", sep = ''))
  # rm(smallm.sl.3, smallf.sl.3)
  # gc()
  # 
  # #6.
  # smallm.sl.icp.3 <- subset_analyses_create_stan_list(smallm.stan.icp, slopevar = "days_to_previous_fb", icpfix = TRUE)
  # smallf.sl.icp.3 <- subset_analyses_create_stan_list(smallf.stan.icp, slopevar = "days_to_previous_fb", icpfix = TRUE)
  # save(smallm.sl.icp.3, smallf.sl.icp.3, file = paste(rdatadir,"small_model3_icp_",date,".RData", sep = ''))
  # rm(smallm.sl.icp.3, smallf.sl.icp.3)
  # gc()
  
  # Full data sets
  #datam.stan <- stan_preprocess_le(data.male$general, hlen=hlen)
  #dataf.stan <- stan_preprocess_le(data.female$general, hlen=hlen)
  #datam.stan.icp <- stan_preprocess_icp_le(data.male$general, hlen=hlen)
  #dataf.stan.icp <- stan_preprocess_icp_le(data.female$general, hlen=hlen)
  
  
  # Rdump the icp-fix data
  #threshold.m <- (Hb_cutoff_male - mean(data.male$general$Hb)) / sd(data.male$general$Hb)
  #threshold.f <- (Hb_cutoff_female - mean(data.female$general$Hb)) / sd(data.female$general$Hb)
  
  # 1. Male donors first model no icp
  #subset_analyses_create_stan_list(datam.stan, filename = "full_model1_male_", rdump = TRUE, dumpdir=dumpdir, threshold = threshold.m, date=date)
  # 2. Female donors first model no icp
  #subset_analyses_create_stan_list(dataf.stan, filename = "full_model1_female_", rdump = TRUE, dumpdir=dumpdir, threshold = threshold.f, date=date)
  # 3. Model 1, icp, male donors
  #subset_analyses_create_stan_list(datam.stan.icp, filename = "full_model1_male_icp_", icpfix = TRUE, rdump = TRUE, dumpdir=dumpdir, threshold = threshold.m, date=date)
  # 4. Model 1, icp, female donors
  #subset_analyses_create_stan_list(dataf.stan.icp, filename = "full_model1_female_icp_", icpfix = TRUE, rdump = TRUE, dumpdir=dumpdir, threshold = threshold.f, date=date)
  
  # 5. Model 2, no icp, male donors
  #subset_analyses_create_stan_list(datam.stan, filename = "full_model2_male_", rdump = TRUE, slopevar = "age", threshold = threshold.m)
  # 6. Model 2, no icp, female donors
  #subset_analyses_create_stan_list(dataf.stan, filename = "full_model2_female_", rdump = TRUE, slopevar = "age", threshold = threshold.f)
  # 7. Model 2, icp, male donors
  #subset_analyses_create_stan_list(datam.stan.icp, filename = "full_model2_male_icp_", icpfix = TRUE, rdump = TRUE, slopevar = "age", threshold = threshold.m)
  # 8. Model 2, icp, female donors
  #subset_analyses_create_stan_list(dataf.stan.icp, filename = "full_model2_female_icp_", icpfix = TRUE, rdump = TRUE, slopevar = "age", threshold = threshold.f)
  # 9. Model 3, no icp, male donors
  #subset_analyses_create_stan_list(datam.stan, filename = "full_model3_male_", rdump = TRUE, slopevar = "days_to_previous_fb", threshold = threshold.m)
  # 10. Model 3, no icp, female donors
  #subset_analyses_create_stan_list(dataf.stan, filename = "full_model3_female_", rdump = TRUE, slopevar = "days_to_previous_fb", threshold = threshold.f)
  # 11. Model 3, icp-fix, male donors
  #subset_analyses_create_stan_list(datam.stan.icp, filename = "full_model3_male_icp_", icpfix = TRUE, rdump = TRUE, slopevar = "days_to_previous_fb", threshold = threshold.m)
  # 12. Model 3, icp-fix, female donors
  #subset_analyses_create_stan_list(dataf.stan.icp, filename = "full_model3_female_icp_", icpfix = TRUE, rdump = TRUE, slopevar = "days_to_previous_fb", threshold = threshold.f)
  return(list(small.data.male=smallm$train, small.data.female=smallf$train))  
}