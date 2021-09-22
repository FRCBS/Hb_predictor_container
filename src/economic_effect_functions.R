#!/usr/bin/env Rscript

library(tidyverse)
library(caret)
library(boot)

#ids <- c("progesa-female-lmm", "progesa-female-dlmm", "progesa-male-lmm", "progesa-male-dlmm", "finngen-male-dlmm", "finngen-female-dlmm", 
#         "findonor-female-dlmm", "findonor-male-dlmm", "progesa-both-dt", "progesa-both-rf")
ids <- c("progesa-male-lmm", "progesa-female-lmm", "progesa-male-dlmm", "progesa-female-dlmm", "finngen-male-dlmm", "finngen-female-dlmm", 
         "findonor-male-dlmm", "findonor-female-dlmm", "progesa-both-dt", "progesa-both-rf")

dummy_ids <- c("progesa-male-baseline", "progesa-female-baseline")
#dummy2_ids <- c("finngen-male-stratified", "finngen-male-most-frequent", "finngen-male-prior", "finngen-male-uniform", "finngen-male-deferred")

all_ids <- c(ids, dummy_ids)

# These are from Progesa data
get_mikkos_cost_constants <- function() {
  return(list(
    mr=0.4732352,
    fr=0.5267648,
    mdr=0.2335795,
    fdr=0.7664205,
    d=0.03269718
  ))
}

data_parameters <- get_mikkos_cost_constants()
fixed_parameters <- list(Pm=2, Pd=60, Fn=0.1066, rloss=0)

all_parameters <- c(data_parameters, fixed_parameters)

load_single <- function(filename) {
  names <- load(filename, verbose=FALSE)
  stopifnot(length(names) == 1)
  return(get(names))
}

filter_donations <- function(data, 
                             donor=NULL, donation13=NULL, site=NULL,
                             date=NULL, phleb_start=NULL, status=NULL,
                             donat_phleb=NULL, Hb=NULL, gender=NULL,
                             dob=NULL, aborh=NULL, age=NULL,
                             agegroup=NULL, Hb_deferral=NULL){
  temp <- data
  if(!is.null(donor)){
    temp <- temp[temp$donor==donor,]
  }
  if(!is.null(donation13)){
    temp <- temp[temp$donation13==donation13,]
  }
  if(!is.null(site)){
    temp <- temp[temp$site==site,]
  }
  if(!is.null(date)){
    temp <- temp[temp$date > date[1] & temp$date < date[1],]
  }
  if(!is.null(phleb_start)){
    temp <- temp[temp$phleb_start==phleb_start,]
  }
  if(!is.null(status)){
    temp <- temp[temp$status==status,]
  }
  if(!is.null(donat_phleb)){
    temp <- temp[temp$donat_phleb==donat_phleb,]
  }
  if(!is.null(Hb)){
    temp <- temp[temp$Hb > Hb[1] & temp$Hb < Hb[2],]
  }
  if(!is.null(gender)){
    temp <- temp[temp$gender==gender,]
  }
  if(!is.null(site)){
    temp <- temp[temp$site==site,]
  }
  if(!is.null(dob)){
    temp <- temp[temp$dob > dob[1] & temp$dob < dob[2],]
  }
  if(!is.null(aborh)){
    temp <- temp[temp$aborh==aborh,]
  }
  if(!is.null(age)){
    temp <- temp[temp$age > age[1] & temp$age < age[2],]
  }
  if(!is.null(agegroup)){
    temp <- temp[temp$agegroup==agegroup,]
  }
  if(!is.null(Hb_deferral)){
    temp <- temp[temp$Hb_deferral==Hb_deferral,]
  }
  
  return(temp)
}

get_cost_constants <- function(data) {
  data <- data %>% mutate(date=dateonly, status="-")
  
  # All successful whole blood donations
  dons <- filter_donations(data, donat_phleb = "K", status = "-")
  # Order donations by date
  dons <- dons[with(dons, order(date)), ]
  # Find only returning donors
  rdons <- dons[duplicated(dons$donor),]
  
  # Successful male
  mdon <- filter_donations(data = rdons, donat_phleb = "K", status = "-", gender = "Men")
  # Successful female
  fdon <- filter_donations(data = rdons, donat_phleb = "K", status = "-", gender = "Women")
  # Deferrals
  def <- filter_donations(data = data, donat_phleb = "*", Hb_deferral = 1)
  mdef <- filter_donations(data = def, gender = "Men")
  fdef <- filter_donations(data = def, gender = "Women")
  
  # Windows
  walldons <- subset(dons, date >= "2018-01-01" & date <= "2019-12-30")
  wdons <- subset(rdons, date >= "2018-01-01" & date <= "2019-12-30")
  wmdon <- subset(mdon, date >= "2018-01-01" & date <= "2019-12-30")
  wfdon <- subset(fdon, date >= "2018-01-01" & date <= "2019-12-30")
  wdef <- subset(def, date >= "2018-01-01" & date <= "2019-12-30")
  wmdef <- subset(mdef, date >= "2018-01-01" & date <= "2019-12-30")
  wfdef <- subset(fdef, date >= "2018-01-01" & date <= "2019-12-30")
  
  # Male ratio
  mr <- nrow(wmdon)/nrow(wdons)
  # Female ratio
  fr <- nrow(wfdon)/nrow(wdons)
  # Male deferral ratio
  mdr <- nrow(wmdef) / nrow(wdef)
  # Female deferral ratio
  fdr <- nrow(wfdef) / nrow(wdef)
  # Total deferral prevalence
  d <- nrow(wdef)/(nrow(wdef) + nrow(walldons))
  return(list(
    mr=mr,
    fr=fr,
    mdr=mdr,
    fdr=fdr,
    d=d
  ))
}


cost_func <- function(q, atot, Pm, Pd, F, Fn, rloss, d) {Pm * ((F*atot) / (F + Fn*(atot-1)) - 1 - d*q*rloss) - Pd*d*q}

cost_func_simplified <- function(q, atot) {2 / ((1-0.1066)/atot + 0.1066)  - 2 - 60*0.03269718*q}

cost_func_factory <- function(Pm, Pd, F, Fn, rloss, d) {
  function(q, atot) {
    return(Pm * ((F*atot) / (F + Fn*(atot-1)) - 1 - d*q*rloss) - Pd*d*q)
  }
}

#get_cost <- function(TPR6, FPR6, TPR12, FPR12, d.=d, mr.=mr, fr.=fr, mdr.=mdr, fdr.=fdr, Pm.=Pm, Pd.=Pd, Fn.=Fn, rl.=rl) {
get_cost <- function(TPR6, FPR6, TPR12, FPR12, sex, p) {
  d.=p$d
  mr.=p$mr
  fr.=p$fr
  mdr.=p$mdr
  fdr.=p$fdr
  Pm.=p$Pm
  Pd.=p$Pd
  Fn.=p$Fn
  rloss.=p$rloss
  
  # OLD. DON'T USE! This assumes a common model for males and females
  get_cost2 <- function(TPR, FPR, new_interval_length) {
    male_interval_factor   <- new_interval_length / 2
    female_interval_factor <- new_interval_length / 3
    q <- d. * TPR
    # Average donation interval extension over all donors.
    # The right parts of the terms sum to one.
    a <- 1 * ((1 - FPR) * (1 - d.)) + # no extension, donors correctly predicted as accepted
      # Small eror below. mr. -> mar. and fr. -> far.
      male_interval_factor * mr. * (FPR * (1 - d.)) + female_interval_factor * fr. * (FPR * (1 - d.)) + # donors falsely predicted as deferred 
      male_interval_factor * mdr. * d.              + female_interval_factor * fdr. * d.                # donors that are deferred or predicted as such 
    Fratio <- a / (1 + Fn. * (a - 1))
    Em <- Pm. * (Fratio - 1 - q * rloss.)
    Ed <- Pd. * q
    E <- Em - Ed
    tibble(q=q, a=a, E=E, TPR=TPR, FPR=FPR)
  }
  # This assumes separate models for males and females
  get_cost3 <- function(TPR, FPR, new_interval_length, sex) {
    male_interval_factor   <- new_interval_length / 2
    female_interval_factor <- new_interval_length / 3
    stopifnot(male_interval_factor >= 1.0)
    stopifnot(female_interval_factor >= 1.0)
    #q <- d. * (mdr. * mTPR + fdr. * fTPR)
    q <- case_when(
      sex=="male"   ~ mdr. * TPR,
      sex=="female" ~ fdr. * TPR,
      sex=="both"   ~ TPR
    )
    # Average donation interval extension over all donors.
    # The right parts of the terms sum to one.
    mar. <- (mr. - mdr.*d.) / (1-d.)   # ratio of males among accepted
    far. <- (fr. - fdr.*d.) / (1-d.)   # ratio of females among accepted
    #print(mar.)
    #print(far.)
    helper <- function(factor, TPR, FPR, ar, dr, d) {
      stopifnot(factor >= 1.0)
      a <- 1 * (1 - FPR) * ar * (1 - d) +           # no extension, donors correctly predicted as accepted
        factor * FPR * (ar * (1 - d))  +                # donors falsely predicted as deferred 
        factor * dr * d                               # donors that are deferred or predicted as such 
      return(a)
    }
    #print(sex)
    if (sex=="male") {
      ma <- helper(male_interval_factor, TPR, FPR, mar., mdr., d.)
      fa <- fr.   # No extension
    } else if (sex=="female") {
      ma <- mr.   # No extension
      fa <- helper(female_interval_factor, TPR, FPR, far., fdr., d.)
    } else {
      ma <- helper(male_interval_factor,   TPR, FPR, mar., mdr., d.)
      fa <- helper(female_interval_factor, TPR, FPR, far., fdr., d.)
    }
    # cat(sprintf("Male extension is %f\n", ma))
    # cat(sprintf("Female extension is %f\n", fa))
    a <- ma + fa
    # Fratio <- a / (1 + Fn. * (a - 1))
    # Em <- Pm. * (Fratio - 1 - d. * q * rl.)
    # Ed <- Pd. * d. * q
    # E <- Em - Ed
    E <- cost_func(q=q, atot=a, Pm=Pm., Pd=Pd., F=1, Fn=Fn., rloss=rloss., d=d.)
    tibble(q=q, a=a, E=E, TPR=TPR, FPR=FPR)
  }
  if (sex=="both") {
    df6 <- get_cost3(TPR6, FPR6, 6, "both")
    df12 <- get_cost3(TPR12, FPR12, 12, "both")
  } else if (sex=="male") {
    df6  <- get_cost3(TPR6, FPR6, 6, "male")
    df12 <- get_cost3(TPR12, FPR12, 12, "male")
  } else if (sex == "female") {
    df6  <- get_cost3(TPR6, FPR6, 6, "female")
    df12 <- get_cost3(TPR12, FPR12, 12, "female")
  } else stop()
  names(df6) <- paste0(names(df6), "6")
  names(df12) <- paste0(names(df12), "12")
  df <- bind_cols(df6, df12) %>% 
    select(q6, q12, a6, a12, E6, E12, TPR6, FPR6, TPR12, FPR12)
  return(df)
  
}

get_rates <- function(df, threshold) {  # Gets true and false positive rates (TPR and FPR)
  #classify predictions
  pred.class <- factor(ifelse(df$Deferred >= threshold, "Deferred", "Accepted"), levels=c("Accepted", "Deferred"))
  #get TPR and FPR
  conf <- caret::confusionMatrix(reference=df$obs, data=pred.class, positive = "Deferred", mode="sens_spec")
  TPR <- unname(conf$byClass['Sensitivity']) 
  FPR <- unname(1 - conf$byClass['Specificity'])
  return(list(TPR=TPR,FPR=FPR))
}

# Extract sex from the id
get_sex <- function(s) {
  str_split(s, "-", simplify = TRUE)[,2]   # The second part is the sex
}

# Returns a dataframe with a row for each threshold
get_thresholds <- function(df,  p, thresholds = seq(0.1, .9, .1), id=id) {
  sex <- get_sex(id)
  tprs <- map_dfr(thresholds, function(t) get_rates(df, t))
  tprs$probability = thresholds
  
  tprs <- tprs %>% rowwise() %>% mutate(get_cost(TPR, FPR, TPR, FPR, sex=sex, p=p), Id=id) %>% ungroup()
  return(tprs)
}

get_optimal_thresholds <- function(df, p, thresholds = seq(0.1, .9, .1), id=id) {
  
  # pred.classes <- data.frame(matrix(nrow=nrow(df),ncol=length(thresholds)))
  # colnames(pred.classes) <- paste0("p_",thresholds)
  # for (t in thresholds ) {
  #   colname <- paste0("p_",t)
  #   pred.classes[, colname] <- factor(ifelse(df$Deferred >= t ,"Deferred", "Accepted"), levels=c("Accepted", "Deferred"))
  # }
  # 
  # # Count TPR and FPR for each probability cut-off
  # 
  # 
  # sen <- vector()
  # spe <- vector()
  # for(i in names(pred.classes)) {
  #   conf <- caret::confusionMatrix(reference=df$obs, data=pred.classes[,i], positive = "Deferred", mode="everything")
  #   #Sensitivity  = TPR
  #   sen <- c(sen,conf$byClass['Sensitivity']) 
  #   spe <- c(spe, 1 - conf$byClass['Specificity']) 
  # }

  tprs <- get_thresholds(df, p, thresholds, id)
  
  #print(tprs)
  row <- tprs %>% slice_min(E6, with_ties=FALSE)
  E6 <- row %>% pull(E6)
  threshold6 <- row %>% pull(probability)
  row <- tprs %>% slice_min(E12, with_ties=FALSE)
  E12 <- row %>% pull(E12)
  threshold12 <- row %>% pull(probability)
  # threshold6 <- tprs %>% filter(E6 == min(E6)) %>% pull(probability)
  # threshold12 <- tprs %>% filter(E12 == min(E12)) %>% pull(probability)
  # E6 <- tprs %>% filter(probability==threshold6) %>% pull(E6)
  # E12 <- tprs %>% filter(probability==threshold12) %>% pull(E12)
  return(list(threshold6=threshold6, threshold12=threshold12, E6=E6, E12=E12))
  #return(tprs)
}

get_linear_progesa_rates <- function() {
  dir="~/FRCBS/interval_prediction/data/raw_results/"
  mfile <- "raw_result_male_icp_date-2020-08-18-gender-male-sample_fraction-1.0-method-icp-fix-hlen-7-iterations-2400-extra_id-progesa-test.rdata"
  ffile <- "raw_result_female_icp_date-2020-08-12-gender-female-sample_fraction-1.0-method-icp-fix-hlen-7-extra_id-progesa-test.rdata"
  male <- load_single(paste0(dir, mfile))$comp_df
  female <- load_single(paste0(dir, ffile))$comp_df
  both <- bind_rows(male, female)
  positives <- both %>% filter(deferral==1) %>% nrow()
  negatives <- both %>% filter(deferral==0) %>% nrow()
  true_positives <- both %>% filter(predicted_labels==1 & deferral==1) %>% nrow()
  false_positives <- both %>% filter(predicted_labels==1 & deferral==0) %>% nrow()
  TPR <- true_positives / positives
  FPR <- false_positives / negatives
  return(list(TPR=TPR,FPR=FPR))
}


get_raw_result_list <- function(id) {
  dir="~/FRCBS/interval_prediction/data/raw_results/"
  if (id == "progesa-female-lmm") {
    file <- "raw_result_female_date-2020-08-12-gender-female-sample_fraction-1.0-method-no-fix-hlen-7-cores-3-extra_id-progesa-test.rdata"
  } else if (id == "progesa-female-dlmm") {
    file <- "raw_result_female_icp_date-2020-08-12-gender-female-sample_fraction-1.0-method-icp-fix-hlen-7-extra_id-progesa-test.rdata"
  } else if (id == "progesa-male-lmm") {
    file <- "raw_result_male_date-2020-08-18-gender-male-sample_fraction-1.0-method-no-fix-hlen-7-iterations-2400-extra_id-progesa-test.rdata"
  } else if (id == "progesa-male-dlmm") {
    file <- "raw_result_male_icp_date-2020-08-18-gender-male-sample_fraction-1.0-method-icp-fix-hlen-7-iterations-2400-extra_id-progesa-test.rdata"
  } else if (id == "finngen-male-dlmm") {
    file <- "raw_result_male_icp_date-2020-08-09-gender-both-sample_fraction-1.0-method-icp-fix-hlen-7-extra_id-finngen-test-data.rdata"
  } else if (id == "finngen-female-dlmm") {
    file <- "raw_result_female_icp_date-2020-08-09-gender-both-sample_fraction-1.0-method-icp-fix-hlen-7-extra_id-finngen-test-data.rdata"
  } else {
    stop(sprintf("Unknown id %s", id))
  }
  result_list<- load_single(paste0(dir, file))
  return(result_list)
}

# The result dataframes are stored in various places and forms. This gets them in uniform manner. 
# The returned dataframe has columns Deferred (probability of deferral) and obs (Accepted/Deferred)
get_data_frame <- function(id) {
  
  process <- function(df) { 
    return(df %>% select(Deferred=scores, obs=deferral) %>% mutate(obs=factor(ifelse(obs==1, "Deferred", "Accepted"), levels=c("Accepted", "Deferred")))) 
  }

  if (id == "findonor-female-dlmm") {
    file <- "/home/toivoja/FRCBS/interval_prediction/data/model_fits/hfc_2108.rdata"
    temp <- load_single(file)
    df <- bind_rows(temp[[1]]$results, temp[[2]]$results, temp[[3]]$results, temp[[4]]$results)
    df <- df %>% select(Deferred=fractions, obs=deferral) %>% mutate(obs=factor(ifelse(obs == 1, "Deferred", "Accepted"), levels=c("Accepted", "Deferred")))
    return(df)
  } else if (id == "findonor-male-dlmm") {
    file <- "/home/toivoja/FRCBS/interval_prediction/data/model_fits/hmc_2108.rdata"
    temp <- load_single(file)
    df <- bind_rows(temp[[1]]$results, temp[[2]]$results, temp[[3]]$results, temp[[4]]$results)
    df <- df %>% select(Deferred=fractions, obs=deferral) %>% mutate(obs=factor(ifelse(obs == 1, "Deferred", "Accepted"), levels=c("Accepted", "Deferred")))
    return(df)
  } else if (id == "progesa-both-rf") {
#    df <- load_single("~/FRCBS/interval-sims/rrfFit_roc_validate_probs.rdata")
#    df <- load_single("~/FRCBS/interval-sims/rrfFit_roc_test_probs.rdata")
    df <- as_tibble(load_single("/home/toivoja/FRCBS/Hb_predictor_container/oversampling_results/rrfFit_roc_test_probs.rdata"))
    return(df)
  } else if (id == "progesa-both-dt") {
    #df <- as_tibble(load_single("~/FRCBS/interval-sims/rrfFit__dtree_roc_validate_probs.rdata"))
    df <- as_tibble(load_single("~/FRCBS/Hb_predictor_container/oversampling_results/rrfFit__dtree_roc_test_probs.rdata"))
    #df <- df %>% mutate(obs = factor(ifelse(obs == 2, "Deferred", "Accepted"), levels=c("Accepted", "Deferred")))
    return(df)
  } else if (id == "progesa-female-baseline") {
    df <- load_single("~/FRCBS/interval-sims/progesa-validate-female-dummy2.rdata")
    return(df)
  } else if (id == "progesa-male-baseline") {
    df <- load_single("~/FRCBS/interval-sims/progesa-validate-male-dummy2.rdata")
    return(df)
  } else if (id == "finngen-male-stratified") {
    df <- get_data_frame("finngen-male-dlmm")
    n <- nrow(df)
    prob <- mean(df$obs == "Deferred")
    set.seed(123)
    df <- df %>% mutate(Deferred = sample(c(0, 1), n, replace=TRUE, prob=c(1-prob, prob)))
  } else if (id == "finngen-male-most-frequent") {
    df <- get_data_frame("finngen-male-dlmm")
    n <- nrow(df)
    t <- table(df$obs)
    prob <- ifelse(t[["Deferred"]] >= t[["Accepted"]], 1, 0)
    df <- df %>% mutate(Deferred = prob)
  } else if (id == "finngen-male-prior") {
    df <- get_data_frame("finngen-male-dlmm")
    n <- nrow(df)
    prob <- mean(df$obs == "Deferred")
    df <- df %>% mutate(Deferred = prob)
  } else if (id == "finngen-male-uniform") {
    df <- get_data_frame("finngen-male-dlmm")
    n <- nrow(df)
    set.seed(456)
    df <- df %>% mutate(Deferred = runif(n, min=0, max=1))
  } else if (id == "finngen-male-deferred") {
    df <- get_data_frame("finngen-male-dlmm")
    n <- nrow(df)
    df <- df %>% mutate(Deferred = 1.0)
  } else {         # Linear models
    df <- get_raw_result_list(id)$comp_df
    return(process(df))
  }
}


# Rates from random forest
# get_rf_rates <- function() {
#   TPR <- 1662/(1662 + 454) #78.5%
#   FPR <- 9137 / (31719 + 9137) # 22.4%
#   return(list(TPR=TPR,FPR=FPR))
# }

# Rates and other parameters from random forest for different probability thresholds
# get_rf_rates2 <- function(threshold) {
#   params <- tibble::tribble(
#     ~probability,  ~TPR,   ~FPR,     ~q,  ~a6, ~a12,    ~E6,   ~E12,
#     0.1, 0.983,  0.672, 0.0322,    2, 3.68, -0.317,    1.8,
#     0.2, 0.949,  0.508,  0.031, 1.76, 3.05, -0.599,   1.15,
#     0.3, 0.909,  0.394, 0.0297,  1.6, 2.62, -0.771,  0.684,
#     0.4, 0.855,    0.3,  0.028, 1.47, 2.26,  -0.88,  0.308,
#     0.5, 0.785,  0.224, 0.0257, 1.36, 1.97, -0.923, 0.0256,
#     0.6,  0.71,   0.16, 0.0232, 1.27, 1.73, -0.925, -0.188,
#     0.7, 0.618,  0.114, 0.0202,  1.2, 1.55, -0.856, -0.284,
#     0.8, 0.517,  0.079, 0.0169, 1.15, 1.41, -0.745, -0.304,
#     0.9, 0.401, 0.0571, 0.0131, 1.12, 1.33, -0.571, -0.214
#   )
#   df <- params %>% filter(probability==threshold)
#   return(list(TPR=df$TPR, FPR=df$FPR))
#   
# }


# Does not work at the moment
test_get_cost <- function(name, threshold=0.5) {
  
  if (name == "computed_consts_linear_progesa") {
    data <- load_single("~/FRCBS/interval_prediction/data/full_data_preprocessed-2020-05-25.rdata")
    consts <- get_cost_constants(data)
    ret <- get_linear_progesa_rates()
  } else if (name == "mikkos_consts_linear_progesa") {
    consts <- get_mikkos_cost_constants()
    ret <- get_linear_progesa_rates()
  } else if (name == "mikkos_consts_rf_progesa") {
    consts <- get_mikkos_cost_constants()
    ret <- get_rf_rates2(threshold)
    name <- sprintf("%s %.1f", name, threshold)
  }
  
  consts <- get_mikkos_cost_constants()
  #ret <- get_linear_progesa_rates()
  
  d <- consts$d
  mr <- consts$mr
  fr <- consts$fr
  mdr <- consts$mdr
  fdr <- consts$fdr
  
  TPR <- ret$TPR
  FPR <- ret$FPR
  #cat(TPR, "\n")
  #cat(FPR)
  q <- d * TPR
  rloss <- fixed_parameters$rloss
  Pm <- fixed_parameters$Pm
  Pd <- fixed_parameters$Pd
  Fn <- fixed_parameters$Fn
  cost <- get_cost(TPR, FPR, d, mr, fr, mdr, fdr, Pm, Pd, Fn, rloss)
  cost <- cost %>% mutate(name = name)
  return(cost)
  
}

# Computes the F1 score.
# The input dataframe 'df' should have two columns:
# 'obs': factor with levels "Accepted", "Deferred", where "Deferred" is the positive class.
# 'Deferred': a numerical vector. Score (e.g. probability) of deferral.
get_f1 <- function(df, threshold = 0.5) {
  pred.class <- factor( ifelse(df$Deferred >= threshold, "Deferred", "Accepted"), levels=c("Accepted", "Deferred"))
  cm <- caret::confusionMatrix(reference = df$obs, data = pred.class, positive = "Deferred", mode = "prec_recall")
  f1 <- cm$byClass["F1"]
  return(f1)   # Returns a single value
}

# Computes the area under the precision-recall curve
# See get_f1 for parameter description.
get_aupr <- function(df) {
  aupr <- suppressMessages(PRROC::pr.curve(scores.class0=df$Deferred, 
                          weights.class0=df$obs=="Deferred")$auc.davis.goadrich)
  return(aupr)   # Returns a single value
}

# Computest the area under the receiver operating characteristic curve
# See get_f1 for parameter description.
get_auroc <- function(df) {
  auroc <- suppressMessages(pROC::auc(response = df$obs=="Deferred",
                     predictor = df$Deferred))
  return(auroc)   # Returns a single value
}
          
#boot code from https://github.com/FRCBS/changes_in_donor_health/blob/master/src/hypothesis_regressions.Rmd
# boot_data is the original input dataframe and boot_ind is a set of indices that defines the sample used on this replicate.
boot_cost <- function(boot_data, boot_ind, f1_threshold, threshold6 = 0.6, threshold12 = 0.8, p, id){
  #sample
  boot_data <- boot_data[boot_ind,]       # a sample of the original data

    #get cost
  r6 <- get_rates(boot_data, threshold6)
  r12 <- get_rates(boot_data, threshold12)

  sex <- get_sex(id)
  costs <- get_cost(TPR6=r6$TPR, FPR6=r6$FPR, TPR12=r12$TPR, FPR12=r12$FPR, sex=sex, p=p)
  costs <- costs %>% select("E6", "E12", "a6", "a12", "q6", "q12", "TPR6", "TPR12", "FPR6", "FPR12")
  costs <- unlist(costs)
  
  # Compute the F1 score using threshold 0.5
  costs["F1"] <- get_f1(boot_data, threshold = f1_threshold)
  costs["AUPR"] <- get_aupr(boot_data)
  costs["AUROC"] <- get_auroc(boot_data)
  
  return(costs)   # returns a vector of statistics with element names E6, E12, F1, AUPR, and AUROC
}

# method is either "norm", "basic", "perc", or "bca"
compute_cis <-function(fit_boot, conf = 0.95, method="norm"){
  statistics <- fit_boot$t0  # for example E6, E12, F1, AUPR, and AUROC
  n_statistics <- length(statistics)
  CI_low = rep(0, n_statistics)  # Create arrays for end points of confidence intervals
  CI_high = rep(0, n_statistics)
  value = rep(0, n_statistics)   # the actual value of the statistic on full data
  for (i_regressor in 1:n_statistics){  # Iterate over all statistics returned by 'boot_cost' function.
    
    # The F1 score can be undefined if both precision and recall are zeros
    error_code <- tryCatch(
      condition = function(cnd) {
        # return exit code
        -1
      },
      {
        #https://stackoverflow.com/questions/6791290/r-boot-package-not-enough-memory-to-get-confidence-intervals
        #CI <- boot.ci(fit_boot, type = "bca", index=i_regressor)
        #Bca_low[i_regressor] <- CI$bca[4] # N.B type = 'normal' has 3 columns, while the other types 5 columns
        #Bca_high[i_regressor] <- CI$bca[5] #
        #there is just too much data for bca to be used
        CI <- boot.ci(fit_boot, conf = conf, type = method, index=i_regressor)  # using normal approximation
        var <- recode(method, "norm"="normal", "perc"="percent", "stud"="student")  # The name of the output field is stupidly sometimes not the same as the parameter name
        value[i_regressor] <- CI$t0
        if (method == "norm") {
          CI_low[i_regressor] <- CI$normal[2]
          CI_high[i_regressor] <- CI$normal[3]
        } else {
          CI_low[i_regressor] <- CI[[var]][4]
          CI_high[i_regressor] <- CI[[var]][5]
        }
      }
    )
    if (!is.null(error_code) && error_code == -1) {
      CI_low[i_regressor] <- NaN   # I have to do these outside the tryCatch block because the error handler cannot access these variables.
      CI_high[i_regressor] <- NaN
      value[i_regressor] <- NaN
    }
  }
  
  return(tibble(variable = names(fit_boot$t0), value, CI_low, CI_high))
}

get_confidence_intervals <- function(df, f1_threshold, threshold6=0.6, threshold12=0.8, conf=0.95, method="norm", n.boot=2000, id,
                                     parameters) {
  if (is.null(n.boot)) {
    n.boot <- nrow(df)  # as many replications as there are data rows
  }
  fit_boot <- boot(df, f1_threshold=f1_threshold, threshold6=threshold6, threshold12=threshold12, p=parameters, id=id,
                   statistic = boot_cost, R = n.boot, parallel="multicore", ncpus=4)
  cis <- compute_cis(fit_boot, conf=0.95, method=method)
  cis <- cis %>% 
    filter(variable %in% c("E6", "E12", "F1", "AUPR", "AUROC", "a6", "a12", "q6", "q12", "TPR6", "TPR12", "FPR6", "FPR12")) %>% 
    pivot_longer(cols=c(value, CI_low, CI_high), names_to="type")
  return(cis)
}

# Input:
# - df contains prediction results from a single algorithm
# - id (string) is the name of the data.
# - conf is the wanted confidence level
# - method is either "norm", "basic", "perc", or "bca"
# - n.boot tells the number of bootstrap replications. If it is NULL, then as many replicates are used as there are rows in the data 'df'
# Output:
# - dataframe in long form, for contents, see the next function
process_data <- function(df, id, conf=0.95, method="norm", n.boot=2000, threshold_range, parameters) {
  message(id)
  #thresholds <- seq(0.1, 0.9, 0.1)  # thresholds for probability of deferral
  
  f1_threshold <- 0.5
  
  res <-  get_optimal_thresholds(df, p=parameters, thresholds = threshold_range, id=id) # These thresholds are for cost effect computation (E6 and E12)

  # Thresholds
  threshold_df <- tibble(variable=c(#"E6", "E12", "F1", "AUPR", "AUROC", 
                              "threshold6", "threshold12"), 
                   type="value", 
                   value=c(#res$E6, res$E12, f1, aupr, auroc, 
                           res$threshold6, res$threshold12))
  # Rest variables with their CIs
  cis <- get_confidence_intervals(df, f1_threshold=f1_threshold, threshold6 = res$threshold6, threshold12 = res$threshold12, 
                                  conf=conf, method=method, n.boot=n.boot, id=id, parameters=parameters)
  return(bind_rows(threshold_df, cis))
}

# The main function in this file.
# Returns a dataframe in long form where for each data/method pair there is
# - threhold6 and threshold12 for probability of deferral
# - E6 and E12 economic effect per donation and 95% CIs
# - F1, AUPR, and AUROC values and CIs
process_all_data <- function(ids, conf=0.95, method="norm", n.boot=2000,
                             threshold_range = seq(0.02, 0.98, 0.02),  # thresholds for probability of deferral                           
                             parameters=all_parameters
                             ) {
  data_frames <- map(ids, get_data_frame)
  results <- map2(data_frames, ids, process_data, 
                  conf=conf, method=method, n.boot=n.boot, threshold_range=threshold_range, parameters=parameters)  # Process each dataframe
  names(results) <- ids
  df <- bind_rows(results, .id="Id") %>%
    mutate(type=recode(type, "CI_low" = "low", "CI_high" = "high"))

  return(df)
}



test_get_all_costs <- function() {
  costs <- bind_rows(test_get_cost("computed_consts_linear_progesa"), 
                     test_get_cost("mikkos_consts_linear_progesa"), 
                     test_get_cost("mikkos_consts_rf_progesa", 0.5),
                     test_get_cost("mikkos_consts_rf_progesa", 0.6),
                     test_get_cost("mikkos_consts_rf_progesa", 0.8),
  )
  return(costs)
}




roc_wrapper <- function(df, boot.n=2000) {
  df <- df %>% rename(scores = Deferred, labels=obs) %>% mutate(labels = ifelse(labels=="Accepted", 0, 1))
  return(create_roc_new(df$labels, df$scores, boot.n=boot.n))
}

pr_wrapper <- function(df, method="norm", boot.n=2000) {
  df <- df %>% rename(scores = Deferred, labels=obs) %>% mutate(labels = ifelse(labels=="Accepted", 0, 1))
  return(create_precision_recall_new(df$labels, df$scores, method=method, boot.n=boot.n))
}



# Below are cost surface drawing related functions


myoperator2 <- function(f) {
  function(L) { f(L[1], L[2])}  
}

myoperator7 <- function(f) {
  function(L) { f(L[1], L[2], L[3], L[4], L[5], L[6], L[7])}  
}


# Computes the gradient of the cost function at the given point.
# The result is a vector, whose components can be thought of as coefficients of a linear function
# that approximates the cost function at the given point.
compute_gradient_of_cost <- function(q=0.015, atot=1.5, Pm=2, Pd=60, F=1, Fn=0.107, rloss=0) {
  func <- myoperator7(myfunc2)
  point <- c(q=q, atot=atot, Pm=Pm, Pd=Pd, F=F, Fn=Fn, rloss=rloss)
  v <- numDeriv::grad(func, point)
  n <- c("q", "atot", "Pm", "Pd", "F", "Fn", "rloss")
  names(v) <- n
  cat("Gradient at point\n")
  print(point)
  cat("is\n")
  print(v)
  #cat(sprintf("Gradient at point %s is %s\n", paste(point, sep=",", collapse = ","),  paste(v, collapse=",")))
  return(invisible(v))
}

# Draw the cost surface on parameters q and atot
draw_surface <- function(scale=FALSE, results=NULL, cost_function = cost_func_simplified) {
  n <- 1000
  d <- 0.032
  df <- tibble(atot=seq(1, 2, length.out = n), 
               q=seq(0, 1, length.out = n))
  df <- df %>% 
    expand(atot, q) %>% 
    mutate(E = cost_function(q, atot))
  
  if (scale) {  # scale to range [0,1]
    df <- df %>% mutate(q=q/d)
  }
  g <- df %>% ggplot(aes(x=q, y=atot, fill=E)) +
    geom_raster() +
    stat_contour(mapping=aes(z=E), color = "white", size = 0.7, binwidth =  1) +
    scale_fill_gradientn(colours=c("blue", "red")) +
    labs(x="Rate of avoided deferrals q", y=expression(Donation~interval~coefficient~a[tot])) +
    theme_bw()
  
  if (!is.null(results)) {   # Show points and confidence intervals of E, a, and q.
    results <- results %>%
      filter(#Id=="progesa-both-rf",
             variable %in% c("E6", "E12", "a6", "a12", "q6", "q12")) %>%
      tidyr::extract(col=variable, regex = "([a-zA-Z]+)([0-9]+)", into=c("variable", "month")) %>% 
      mutate(month=as.integer(month)) %>%
      pivot_wider(names_from=c(variable, type))
    print(results)
    g <- g +
      geom_text(data=results, mapping=aes(x = q_value, 
                        y = a_value-0.05,
                        #label = paste0("6 mo: ", round(tprs$E6[which(tprs$E6 == min(tprs$E6))],2),' (',round(deframe(p6.ci[4,2]),2)," – ",round(deframe(p6.ci[4,3]),2),') €'  )),
                        label = sprintf("%i mo: %.2f (%.2f – %.2f) €", month, E_value, E_low, E_high)),
                        inherit.aes = FALSE,
                        colour = "white") +
      geom_point(data=results, mapping=aes(x=q_value, y=a_value), inherit.aes = FALSE, size=3, color="white", alpha=1) +
      geom_linerange(data=results, mapping=aes(x=q_value, ymin=a_low, ymax=a_high), inherit.aes = FALSE, size=1) +
      geom_linerange(data=results, mapping=aes(y=a_value, xmin=q_low, xmax=q_high), inherit.aes = FALSE, size=1) +
      labs(fill="Cost effect (€ / donation)") +
      theme(legend.position="bottom")
  }
  return(g)
}

# Draw the absolute value of the gradient of the cost function on parameters q and atot only
draw_gradient <- function() {
  mylength <- function(L) sqrt(L[1]^2 + L[2]^2)
  n <- 100
  d <- 0.032
  df <- tibble(atot=seq(1, 2, length.out = n), 
               q=seq(0, d, length.out = n))
  df <- df %>% expand(atot, q)
  func <- myoperator2(cost_func_simplified)
  df <- df %>% mutate(G = map2_dbl(q, atot, function(x, y) mylength(numDeriv::grad(func, c(x, y)))))
  
  g <- df %>% ggplot(aes(x=q, y=atot, fill=G)) +
    geom_raster()
  return(g)
}
