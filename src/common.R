# Variables that are in use
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(doParallel))
#suppressPackageStartupMessages(library(shapr))
suppressPackageStartupMessages(library(fastshap))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ranger))

descript <- tibble(Variable = c("donor", "Hb", "days_to_previous_fb", "age", "previous_Hb_def", 
                                "year", "warm_season", "consecutive_deferrals", "recent_donations",
                                "recent_deferrals", "hour", 
                                "previous_Hb", "previous_Hb2", "previous_Hb3", "previous_Hb4", "previous_Hb5", 
                                "Hb_first", "Hb_deferral", "sex"), 
                   Pretty = c("Donor ID", "Hemoglobin", "Days to previous full blood donation", "Age", "Previous event low hemoglobin", 
                              "Year", "Warm season", "Consecutive deferrals", "Recent donations", 
                              "Recent low hemoglobins", "Hour", 
                              "Previous Hb", "Previous Hb 2", "Previous Hb 3", "Previous Hb 4", "Previous Hb 5", 
                              "First Hb", "Low hemoglobin", "Sex"),
                   Type = c("Factor", "numeric", "numeric (int)", "numeric", "boolean",
                            "numeric (int)", "boolean", "numeric (int)", "numeric (int)", "numeric (int)", "numeric",
                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                            "numeric", "boolean", "Factor"),
                   Explanation = c("Donor identifier",
                                   "Amount of Hemoglobin",
                                   "Time (in days) between Hb measurement and previous full blood donation event",
                                   "Age of donor",
                                   "Indicates whether the donor had low hemoglobin at previous donation event",
                                   "Year of donation",
                                   "True if donation was given in April-September",
                                   "Number of times the donor has been deferred due to low hemoglobin since last succesful whole blood donation",
                                   "Number of donations in the last two years",
                                   "Number of low hemoglobins in the last two years",
                                   "Time of day when donation was given as hours (e.g. 13:45 = 13.75)",
                                   "Hb value at previous measurement (dynamic linear mixed model)",
                                   "Hb value two attemps ago",
                                   "Hb value three attemps ago",
                                   "Hb value four attemps ago",
                                   "Hb value five attemps ago",
                                   "Hb value at first donation of this donor (linear mixed model)",
                                   "Hemoglobin below deferral threshold",
                                   "Sex of the donor")
)

#tibble_row(Variable="one_deferral", Pretty="At least one deferral", Type="numeric (int)", Explanation="At least one deferral")

donor_descript <- tibble(
  Variable    = c("smoking", "height", "weight", "RNF43_mutant", "prs", "FERRITIN_FIRST", "FERRITIN_LAST", "one_deferral", "label"),
  Pretty      = c("Smoking", "Height", "Weight", "RNF43", "Polygenic score", "First ferritin", "Last ferritin", "At least one deferral", "Partition label"),
  Type        = c("boolean", "numeric", "numeric", "boolean", "numeric", "numeric", "numeric", "numeric (int)", "factor"),
  Explanation = c("Does the person smoke", "Height of the donor", "Weight of the donor", 
                  "Mutation at RNF43 gene in chromosome 17 position 58358769", "Polygenic risk score for hemoglobin", 
                  "First measured ferritin value", "Last measured ferritin value", "At least one deferral",
                  "The donors are partitioned into train, validate, and test sets")
)

# FinDonor donation specific:
descript.fd <- tibble(Variable = c("Eryt", "HKR", "Leuk", "Trom", "MCH", "MCHC", "MCV", "RDW", "CRP", "Ferritin", "TransferrinR"),
                      Pretty = c("Erythrocyte", "HCT", "Leukocyte", "Thrombocyte", "MCH", "MCHC", "MCV", "RDW", "CRP", "Ferritin", "Transferrin receptor"),
                      Type = c("Numeric", "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",
                               "Numeric"),
                      Description = c("Amount of erythrocytes, red blood cells [E12/l]",
                                      "Hematocrit (HTC) the volume percentage of RBCs in whole blood [%]",
                                      "Leukocytes, white blood cell counts [E9/l] (thousand cells per microliter)",
                                      "Thrombocytes/platelets [E9/l]",
                                      "Mean corpuscular hemoglobin, average mass of hemoglobin per red blood cell. Diminished in hypochromic anemias. [pg]",
                                      "Mean corpuscular hemoglobin concentration, average mass of hemoglobin in a litre of RBCs. [g/l]",
                                      "Mean Corpuscular Volume, the average volume of RBCs. [fl]",
                                      "Red blood cell distribution width, is a measure of the range of variation of RBC volume. [%]",
                                      "C-reactive protein, measure of inflammation. Its circulating concentrations rise in response to inflammation. [mg/l]",
                                      "An intracellular protein that stores iron and releases it in controlled fashion. Acts as a buffer against iron deficiency and iron overload. Plasma ferritin is an indirect marker of the total iron strorage. [µg/l]",
                                      "Transferrin receptor (TfR) is a carrier protein for transferrin. [mg/l]"))
# FinDonor donor specific
descript.fd.consts <- tibble(Variable=c("height", "weight", "smoking_status", "physical_condition", "meat_amount", "sleep_quality", "iron_supplement"),
                             Pretty=c("Height", "Weight", "Smoking status", "Physical condition", "Meat amount", "Sleep quality", "Iron supplement"),
                             Type=c("Numeric", "Numeric", "Boolean", "Numeric (int)", "Numeric (int)", "Numeric (int)", "Numeric (int)"),
                             Description=c("Height", "Weight", "Smoking status: no=0, sometimes or daily = 1", 
                                           "Physical condition: very bad=0, bad=1, satisfactory=2, rather good=3, good=4, excellent=5",
                                           "How often meat: less than one weekly=1, 1-3 per week=2, 4-6 per week=3, daily=4, several daily=5", 
                                           "How often do you feel like you've slept enough: never=0, rarely = 1, mostly=2, always or almost = 3",
                                           "How many iron table were eaten"))

# Converts column names of a dataframe 'df' to pretty format using dataframe 'description'.
to_pretty <- function(df, description) {
  old_names <- colnames(df)
  conversion <- description %>% filter(Variable %in% all_of(old_names)) %>% select(Pretty, Variable) %>% deframe()
  #print(conversion)
    #message(sprintf("descript names: %s\n", paste(description$Variable, collapse=" ")))
  #message(sprintf("old_names: %s\n", paste(old_names, collapse=" ")))
  #message(sprintf("new_names: %s\n", paste(new_names, collapse=" ")))
  df %>% rename(!!!conversion)
}

summary_plotter <- function(df, variable_descriptions, color, breaks=waiver(), ncol=NULL) {
  g <- df %>%
    mutate(across(where(is.logical), as.integer)) %>%
    keep(is.numeric) %>%
#    keep(function(c) is.numeric(c) | is.factor(c)) %>%
    to_pretty(variable_descriptions) %>%
    #gather() %>%
    pivot_longer(cols=everything()) %>%
    filter(!(str_detect(name, "^Previous Hb [2-5]") & is.na(value))) %>%   # Drop these explicitly so they won't cause a warning
    mutate(name = factor(name, levels=variable_descriptions$Pretty))  %>% # Don't sort alphabetically
    ggplot(aes(value)) +
    facet_wrap(~ name, scales = "free", ncol=ncol) +
    geom_histogram(fill = color) +
    scale_x_continuous(breaks=breaks)
  return(g)
}

# Splits data into bins, and return bin sizes. If the number of items in a bin is less than five,
# then the size of that bin is set to zero.
create_histograms <- function(df, variable_descriptions, filename=NULL, id=NULL) {
  tmp <- df %>%
    drop_na() %>%
    mutate(across(where(is.logical), as.integer)) %>%
    keep(is.numeric) %>% 
    to_pretty(variable_descriptions) %>% 
    pivot_longer(cols=everything())
  
  res <- tmp %>% 
    group_by(name) %>%
    mutate(value = cut(value, breaks=30)) %>%
    ungroup() %>%
    count(name, value) %>% 
    mutate(n=ifelse(n < 5, 0, n))
  if (! is.null(id))
    res <- res %>% mutate(id=id)
  if (! is.null(filename))
    write_csv(res, filename)
  res
}

double_summary_plotter <- function(male_df, female_df, variable_descriptions, geom = "freqpoly", breaks=waiver(), ncol=NULL) {
  df <- bind_rows(male=male_df, female=female_df, .id="Sex")
  #print(df, 5)
  df <- df %>%
    mutate(across(where(is.logical), as.integer)) %>%
    #keep(is.numeric(col) %>%
    select(where(is.numeric) | Sex) %>%
    to_pretty(variable_descriptions) %>%
    #gather() %>%
    pivot_longer(!Sex) %>%
    mutate(name = factor(name, levels=variable_descriptions$Pretty))   # Don't sort alphabetically
  
  #if (df %>% filter(name=="Days to previous full blood donation") %>% nrow() > 0) {
  # Abbreviate this long variable name
  if ("Days to previous full blood donation" %in% levels(df$name)) {
    df <- df %>%
      mutate(name = fct_recode(name, `Days to previous FB donation`="Days to previous full blood donation"))
  }
  
  if (geom=="freqpoly") {
    g <- df %>%
      ggplot(aes(value, color=Sex)) +
      facet_wrap(~ name, scales = "free", ncol=ncol) +
      geom_freqpoly() +
      scale_x_continuous(breaks=breaks)
  } else if (geom=="histogram") {
    g <- df %>%
      ggplot(aes(value, fill=Sex)) +
      facet_wrap(~ name, scales = "free", ncol=ncol) +
      geom_histogram(position="dodge") +
      scale_x_continuous(breaks=breaks)
  } else if (geom=="hollow_histogram") {
    g <- df %>%
      ggplot(aes(value, color=Sex)) +
      facet_wrap(~ name, scales = "free", ncol=ncol) +
      geom_histogram(fill=NA) +
      scale_x_continuous(breaks=breaks)
  } else {
    stop(sprintf("Unknow value for the geom parameter: %s", geom))
  }

  return(g)
}

time_series_length_plotter <- function(df, color) {
  n <- 20  # These will be shown individually, whereas longer time series will be pooled together
  levels <- c(1:n, "Longer")
  df2 <- df %>% 
    count(donor, name="Length") %>% 
    mutate(Length=factor(ifelse(Length <= n, Length, "Longer"), levels=levels)) %>% 
    count(Length, name="Count") #%>%
    #mutate(Length=fct_reorder(levels))
  g <- df2 %>% ggplot(aes(x = Length, y=Count)) + 
    geom_bar(stat="identity", fill=color) +
    labs(x="Time series length", y="Number of donors")
  g
}

data_counts <- function(data) {
  helper <- function(df, key) {
    dd <- sum(df %>% group_by(donor) %>% summarise(deferred=max(Hb_deferral)) %>% pull(deferred))
    dld <- sum(df %>% group_by(donor) %>% slice_max(order_by = dateonly, n=1) %>% ungroup() %>% pull(Hb_deferral))
    t <- tibble(Donations=nrow(df), Donors=n_distinct(df$donor), Deferrals=sum(df$Hb_deferral), 
                `Deferred donors`=dd, `Deferred last donations`=dld)
  }
  # sizes <- bind_rows(helper(train), helper(validate)) %>%
  #   mutate(Dataset = c("train", "validate")) %>%
  #   relocate(Dataset)
  sizes <- data %>%
    group_by(label) %>%
    group_modify(helper)
  sizes_by_age <- data %>% 
    mutate(age_class = cut(age, breaks=c(18, seq(25, 75, 5)), right = FALSE)) %>%
    group_by(label, age_class) %>% group_modify(helper)
  return(list(sizes=sizes, sizes_by_age=sizes_by_age))
}

# compute_deferral_by_age <- function(donation) {
#   rates <- donation %>% 
#     mutate(age_class = cut(age, breaks=c(18, seq(25, 75, 5)), right = FALSE)) %>%
#     group_by(age_class) %>%
#     summarise(deferral_rate = 100*sum(Hb_deferral) / n()) %>%
#     ungroup()
#   rates
# }

visualise_deferral_by_age <- function(sizes) {
  rates <- sizes %>% mutate(deferral_rate = 100 * `Deferred last donations` / Donors)
  rates %>% ggplot(aes(x=age_class, y=deferral_rate, color=label, group=label)) +
    geom_line() +
    geom_point() + labs(x="Age class", y="Last donation deferral rate (%)") +
    labs(color="Data")
}

read_hyperparameters <- function(filename) {
  if (str_detect(filename, "\\.json$")) {
    df <- as_tibble(rjson::fromJSON(file=filename)) 
    df <- df %>% mutate(across(c(Model, Sex), as.character))  # In case of empty tibble we have lost information about columns types. Restore them.
  } else {
    df <- readRDS(filename)
  }
  return(df)
}

write_hyperparameters <- function(hyperparameters, filename, json=TRUE) {
  if (json) {
    cat(rjson::toJSON(hyperparameters), file = filename)
  } else {
    saveRDS(hyperparameters, filename)
  }
}

learn_hyperparameters <- function(df, method, search_grid, cores, sampling, ...) {
  message("In function learn_hyperparameters")
  df <- df %>% filter(label=="train") %>% select(-label) # Drop donors that belong to the original validate set
  #file <- "../output/learned_hyperparameters.Rdata"
  
  # For debugging:
  #mypar <- list(df, method, search_grid, cores, ...)
  #saveRDS(mypar, "/tmp/parameterit.rds")
  
  #Initialise parallellisation
  # Option outfile="" keeps stdout and stderr, otherwise they are thrown away
  # https://stackoverflow.com/questions/34870249/caret-train-not-outputting-progress
  # For some reason this doesn't work inside a container
  message("moi1")
#  sink(file=stderr(), type="output");   # For some reason this caused problems, but it seems it is not needed after all!
  message("moi1.1")
#  cl <- parallel::makePSOCKcluster(cores)
  cl <- parallel::makePSOCKcluster(cores, outfile = "")
  message("moi2")
  doParallel::registerDoParallel(cl)
  message("moi3")
  
  
  #Define cross validation
  fitControl <- caret::trainControl(## 4-fold CV
    method = "repeatedcv",
    number = 10, # folds
    ## repeated ten times
    repeats = 10, #how many is good?
    verboseIter = TRUE,
    classProbs = TRUE,
    sampling = sampling,
    summaryFunction = twoClassSummary
    #savePredictions = TRUE
  )
  message("moi4")
  
#  if (method %in% c("svmPoly", "svmRadial")) {
#    fitControl$sampling <- "smote"
#  }
  message("moi5")
  
  #Train
  message("moi6")
  rrfFit_roc_hyper <- caret::train(Hb_deferral ~ ., data = df, 
                                   method = method, 
                                   trControl = fitControl, 
                                   verbose = FALSE, 
                                   ## Now specify the exact models 
                                   ## to evaluate:
                                   tuneGrid = search_grid,
                                   metric="ROC",
                                   ...
                                   #should we use Kappa or ROC?
                                   #                importance = "permutation"
                                   #https://stackoverflow.com/questions/18578861/variable-importance-using-the-caret-package-error-randomforest-algorithm
                                   #should we use , ’impurity’, ’impurity_corrected’, ’permutation’ ?
  )
  #save(rrfFit_roc_hyper, file=file)
  parallel::stopCluster(cl)
#  sink()
  return(rrfFit_roc_hyper)
}


# This is currently used by the RF and SVM models.
# These should be dropped or incorporated into the main preprocessing.
# Note that this also contains Finnish specific donation intervals 62 and 92
additional_preprocess <- function(data, variables, logger) {
  
  # This really should be done in the preprocessing
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>% group_by(donor) %>%
    dplyr::filter(n()>1) %>%  #Take out the ones with only one event 
    ungroup()
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to length of time series being one\n", 
                  old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  message(msg)
  print(logger, msg)
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>% group_by(donor) %>%
    slice_max(order_by = dateonly, n=1) %>%
    ungroup() 
  msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to taking only the last donation of each time series\n", 
                  old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
  message(msg)
  print(logger, msg)
  
  data <- data %>% 
    mutate(Hb_deferral=factor(ifelse(Hb_deferral, "Deferred", "Accepted"), levels = c("Accepted","Deferred")))

  if ("nb_donat" %in% variables) {  
    old_count <- nrow(data); old_count2 <- ndonor(data)
    data <- data %>%
      mutate(nb_donat = sum(nb_donat_progesa, nb_donat_outside ,na.rm=TRUE)) %>% 
      filter( !nb_donat == '0') #Some spurious mistake in the donation record
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to nb_donat being '0'\n", 
                   old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
    message(msg)
    print(logger, msg)
  }
  
  if (FALSE) {  
    old_count <- nrow(data); old_count2 <- ndonor(data)
    data <- data %>% #Take out people that return faster than they should as we do not really know what they mean
      filter(!(days_to_previous_fb < 62 & sex == "male"),
             !(days_to_previous_fb < 92 & sex == "female"))
    msg <- sprintf("Dropped %i / %i donations (%i / %i donors) due to days_to_previous_fb < 62 (men) or 92 (women)\n", 
                    old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2)
    message(msg)
    print(logger, msg)
  }
  
  data <- data %>%   select(all_of(variables))
  return(data)
}

# Split the data into train and test sets
old_split <- function(data) {
  donors <- unique(data$donor)
  #message(donors)
  a <- caret::createDataPartition(1:length(donors), p=0.8, list=FALSE)
  train<- data %>%
    filter(donor %in% donors[a])
  test <- data %>% 
    filter(donor %in% donors[-a])
  return(list(train=train, test=test))
}

new_split <- function(data, mode) {
  if (mode=="initial") {
    train <- data %>% filter(label=="train")
    test  <- data %>% filter(label=="validate")
  } else {  # mode=="final"
    train <- data %>% filter(label %in% c("train", "validate"))
    test  <- data %>% filter(label == "test")
  }
  return(list(train=train, test=test))
}

get_optimal_cut_point <- function(score, original_label, pos_class=NULL) {
  if (is.null(pos_class)) pos_class <- "Deferred"
  # Find the threshold for probability that maximizes the F1 score
  cp <- cutpointr::cutpointr(score, 
                             original_label, 
                             pos_class = pos_class, 
                             direction=">=",   # Larger values mean positive class 
                             #boot_runs = 1000,
                             method = maximize_metric, 
                             #metric = sum_sens_spec
                             metric = F1_score
  )
  
}

gather_results <- function(df, Id, Model, Pretty, Sex, f1_threshold = 0.5) {
  message("In gather_results function")
  #boot.n = 2000
  result <- list()
  result$df <- df
  # Confusion matrix
  result$confusion_matrix_plot <- create_confusion_matrix_plot(df$original_label, df$score_predicted_label) +
    labs(title = "Optimal F1 score cutoff confusion matrix")
  # ROC
  result$roc_plot <- create_roc_new(df$original_label, df$score) #, boot.n=boot.n)
  
  
  # Precision-Recall
  ret <- tryCatch(
    error =  function(cnd) {
      warning(paste("Could not compute precision-recall curve, possibly the deferral score is constant", cnd, sep="\n"))
      return(-1)      
    },
    {
      result$pr_plot <- create_precision_recall_new(df$original_label, df$score)#, boot.n=boot.n)
      NULL
    })
  if (!is.null(ret) && ret == -1) {
    message("taalla")
    pr_ci <- tibble("AUPR value"=NA, "AUPR low"=NA, "AUPR high"=NA)
    pr_plot <- list(pr_ci=pr_ci)
    result$pr_plot <- pr_plot
  }
  message("seko1")
  
  # F1 score
  result$f1_ci <- get_f1_ci(df, threshold = f1_threshold)#, boot.n=boot.n)
  result$f1_ci
  message("seko2")
  
  t <- tibble(Id=Id, Model=Model, Pretty=Pretty, Sex=Sex, 
              "MAE (g / L)"    = NA, "RMSE (g / L)"    = NA, 
              "MAE (mmol / L)" = NA, "RMSE (mmol / L)" = NA) 
  
  message("Here7")
  t <- bind_cols(c(t, result$roc_plot$roc_ci, result$pr_plot$pr_ci, result$f1_ci))
  result$summary <- t
  
  return(result)
}

# Creates plots for donation and donor specific variables and for timeseries length
create_summary_plots <- function(data, donor_specific_variables, sex, donation_description, donor_description) {
  
  color <- case_when(sex == "male" ~ "blue", sex =="female" ~ "orange", TRUE ~ "green")
  
  temp_donors <- data$donor
  
  # Find out do donors have at least one deferral
  at_least_one_deferral <- data %>% 
    group_by(donor) %>% 
    summarise(one_deferral=max(Hb_deferral),
              #            label=unique(label))
              label=as.integer(unique(label)))
  
  #############################
  # Donation specific variables
  #############################
  # Replace last space for a newline character. This is for splitting a long variable name into multiple lines
  replace_last <- function(s) {
    stringi::stri_reverse(str_replace(stringi::stri_reverse(s), " ", "\n"))
  }
  # Cut long variable names to multiple lines
  dd <- donation_description %>% 
    mutate(Pretty = case_when(Variable %in% c("previous_Hb_def", "consecutive_deferrals", "recent_donations") ~ replace_last(Pretty), 
                              Variable=="days_to_previous_fb" ~ "Days to previous\nfull blood\ndonation", 
                              TRUE ~ Pretty))
  donation_specific <- data %>%
    filter(first_event == FALSE) %>%
    select(all_of(donation_description$Variable))

  donation_specific <- summary_plotter(donation_specific, dd, color, breaks=scales::extended_breaks(n=4), ncol=3)
  #pboth
  
  ##########################
  # Donor specific variables
  ##########################
  temp_donor_specific <- at_least_one_deferral
  #tr <- tibble_row(Variable="one_deferral", Pretty="At least one deferral", Type="numeric (int)", Explanation="At least one deferral")
  if (!is.null(donor_specific_variables)) {
    temp_donor_specific <- donor_specific_variables %>% 
      inner_join(at_least_one_deferral, by="donor")
  }
  
  message(sprintf("Column names are: %s\n", paste(names(temp_donor_specific), collapse=" ")))
  
  donor_specific <- temp_donor_specific %>%
    filter(donor %in% temp_donors)
  donor_specific <- summary_plotter(donor_specific, 
                            donor_description,
                            color)
  #pdata2
  
  #####################
  # Time series lengths
  #####################
  lengths <- time_series_length_plotter(data, color)
  
  return(list(donation_specific, donor_specific, lengths))
}

create_variable_summary_table <- function(df) {
  df2 <- df %>% 
    mutate(warm_season = as.logical(warm_season),
           across(where(is.integer), as.double)) %>%
    select(-any_of(c("dateonly", "nb_donat_progesa", "nb_donat_outside", "year")))
  numeric <- df2 %>% select(where(is.double))
  bool <- df2 %>% select(where(is.logical))
  fct <- df2 %>% select(where(is.factor)) %>% select(sex)
  
  numeric2 <- numeric %>%
    pivot_longer(cols = everything()) %>% 
    group_by(name) %>%
    summarise(mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE), median=median(value, na.rm=TRUE), 
              q1=quantile(value, 0.25, na.rm=TRUE), q3=quantile(value, 0.75, na.rm=TRUE),
              min=min(value, na.rm=TRUE), max=max(value, na.rm=TRUE),
              "NA"=sum(is.na(value))) %>%
    mutate(type="numeric")

   bool2 <- bool %>% pivot_longer(cols = everything()) %>% count(name, value) %>%
     mutate(value=as.character(value), type="boolean", "NA"=0)
   
   fct2 <- fct %>% pivot_longer(cols = everything()) %>% count(name, value) %>%
     mutate(value=as.character(value), type="factor", "NA"=0)
   
   res <- bind_rows(numeric2, bool2, fct2) %>% relocate(name, type, value, n)
   saveRDS(res, "/tmp/s.rds")
   res <- res %>% mutate(`NA` = ifelse(is.na(value) & type!="numeric", n, `NA`))
   res
}

prettify_variables <- function(df, variables_renamed) {
  if (variables_renamed) {
    df <- df %>%
      mutate(Pretty = case_when(
        Variable == "previous_Hb_defTRUE" ~ "Previous donation deferred",
        Variable == "warm_seasonTRUE"     ~ "Warm season",
        Variable == "smokingTRUE"         ~ "Smoking",
        Variable == "RNF43_mutantTRUE"    ~ "RNF43 minor allele",
        Variable == "sexfemale"       ~ "Sex",
        Variable == "nb_donat"            ~ "Number of donations",
        TRUE                              ~ Pretty
      ))
  } else {
    df <- df %>%
      mutate(Pretty = case_when(
        Variable == "previous_Hb_def" ~ "Previous donation deferred",
        Variable == "warm_season"     ~ "Warm season",
        Variable == "smoking"         ~ "Smoking",
        Variable == "RNF43_mutant"    ~ "RNF43 minor allele",
        Variable == "sex"       ~ "Sex",
        Variable == "nb_donat"            ~ "Number of donations",
        TRUE                              ~ Pretty
      ))
  }
  return(df)
}

compute_shap_values_shapper <- function(model, train, validate, variables) {
  exp_rf <- DALEX::explain(model, data = train)
  #ive_rf <- shap(exp_rf, new_observation = validate[-c(15)])
  #ive_rf <- shap(exp_rf, new_observation = as.data.frame(train %>% select(-Hb)))
  if ("sex" %in% variables) {
    validate2 <- validate %>% select(-c(Hb))
  } else {
    validate2 <- validate %>% select(-c(Hb, sex))
  }
  validate2 <- as.data.frame(validate2)
  # It seems that I have iterate over the rows of validate2 to get all local attributions
  res <- map_dfr(1:nrow(validate2), function(i) as_tibble(shap(exp_rf, new_observation = validate2[i,])), .id="myid")
  res <- res %>% 
    select(Variable=`_vname_`, attribution=`_attribution_`, sign=`_sign_`)
  return(res)
}

predict_model.train <- function(x, newdata) { 
  res <- predict(x, as.data.frame(newdata), type = "raw") 
  as.integer(res == "Deferred")
}

get_model_specs.train <- function(x){
  feature_list = list()
  feature_list$labels <- labels(x$terms)
  m <- length(feature_list$labels)
  
  feature_list$classes <- attr(x$terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes=="factor"] <- NA # the model object doesn't contain factor levels info
  
  return(feature_list)
}

# compute_shap_values_shapr <- function(model, validate, variables, n=100, seed) {
#   message("In function compute_shap_values_shapr")
#   set.seed(seed)
#   
#   # if ("sex" %in% variables) {
#   #   validate2 <- validate %>% select(-c(Hb))
#   # } else {
#   #   validate2 <- validate %>% select(-c(Hb, sex))
#   # }
#   #validate2 <- as.data.frame(validate2)
#   #validate2 <- validate %>% mutate(previous_Hb_def = as.integer(previous_Hb_def))
#   validate2 <- validate  %>% slice_sample(n=n)
#   p0 <- mean(validate$Hb_deferral == "Deferred")
#   
#   result_code <- tryCatch(
#     error = function(cnd) {
#       msg <- paste("\nComputation of shap values failed:", cnd$message, 
#                    sep="\n")
#       warning(msg)
#       NULL
#     },
#     {
#       explainer <- shapr::shapr(validate2, model, n_combinations = 1000)
#       explanation <- shapr::explain(validate2, explainer, approach = "empirical", prediction_zero = p0)
#     }
#   )
#   if (is.null(result_code)) {
#     return(NULL)
#   }
#   #explanation$dt
#   n <- nrow(explanation$dt)
#   res1 <- as_tibble(explanation$dt) %>% select(-none) %>% mutate(id=1:n)
#   res2 <- as_tibble(explanation$x_test) %>% mutate(id=1:n)
#   res1 <- pivot_longer(res1, cols=!id) %>%
#     select(Variable=name, id, attribution=value)
#   res2 <- pivot_longer(res2, cols=!id) %>%
#     select(Variable=name, id, value=value)
#   res <- inner_join(res1, res2, by=c("Variable", "id")) %>%
#     group_by(Variable) %>%
#     mutate(value=scale(value)[,1]) %>%
#     ungroup()
#   return(res)
# }

# Caret's predict method only allows type to be "prob" or "raw.
# We will dig out the ksvm fit object from Caret's fit object and
# call its predict method directly.
my_predict <- function(fit, newdata, type="response") {
  stopifnot("train" %in% class(fit) && "ksvm" %in% class(fit$finalModel))
  newdata <- predict(fit$preProcess, newdata)  # Preprocess the data the same way Caret does
  newdata <- newdata %>% select(-any_of(c("sex", "Hb", "Hb_deferral"))) 
  res <- predict(fit$finalModel, newdata=newdata, type=type) # Call underlying object's predict method
  return(res)
}

use_decision_value_with_svm <- TRUE

# The nsim parameter seems to have linear effect on running time
compute_shap_values_fastshap <- function(model, validate, variables, n=1000, seed, nsim=100) {
  message("In function compute_shap_values_fastshap")
  set.seed(seed)
  
  n <- min(n, nrow(validate))
  
  validate2 <- validate  %>% slice_sample(n=n) %>% select(-any_of(c("Hb_deferral", "Hb")))

  if ("stanfit" %in% class(model)) {
    t <- as_tibble(rownames_to_column(as.data.frame(rstan::get_posterior_mean(model))))
    beta <- t %>% filter(str_detect(rowname, r"(^beta\[\d+\])")) %>% pull(`mean-all chains`) #select(rowname, mean=`mean-all chains`)
  }
  
  pfun_lmm <- function(object, newdata) {
    #message(colnames(newdata))
    #message(head(newdata$donb))
    #message(sprintf("In function pfun_lmm: rows=%i cols=%i", nrow(newdata), ncol(newdata)))
    result <- as.vector(beta %*% t(as.matrix(newdata %>% select(-donb))) + newdata %>% pull(donb))
    return(result)
  }
  
  pfun_randomForest <- function(object, newdata) {
    predict(object, newdata = newdata, type="prob")[,2]
  }
  
  pfun_ranger <- function(object, newdata) {
    predict(object, data = newdata, type="response")$predictions[,"Deferred"]
  }

  # Not used currently as this doesn't perform preprocessing on newdata
  pfun_ksvm <- function(object, newdata) {
    predict(object, newdata = newdata, type="probabilities")[,2]
  }

  # This uses decision values instead of probabilities
  pfun_ksvm_decision <- function(object, newdata) {
    my_predict(object, newdata = newdata, type="decision")[,1]
  }
  
  # This is Caret's wrapper model
  pfun_train <- function(object, newdata) {
    predict(object, newdata = newdata, type="prob")[,2]
  }
  
  
  if ("randomForest" %in% class(model)) {
    pfun <- pfun_randomForest
  } else if ("ksvm" %in% class(model)) {
    pfun <- pfun_ksvm
  } else if ("ranger" %in% class(model)) {
    pfun <- pfun_ranger
  } else if ("train" %in% class(model)) {
    if (use_decision_value_with_svm && "ksvm" %in% class(model$finalModel)) {
      pfun <- pfun_ksvm_decision
    } else pfun <- pfun_train
  } else if ("stanfit" %in% class(model)) {
    pfun <- pfun_lmm
  }
  
  #print(pfun)
  
  result_code <- tryCatch(
    error = function(cnd) {
      msg <- paste("\nComputation of shap values failed:", cnd$message, 
                   sep="\n")
      warning(msg)
      NULL
    },
    {
      rlang::with_options(lifecycle_verbosity = "quiet", {  # Prevent the deprecation message caused by the explain function
        if ("lm" %in% class(model)) {
          shap <- fastshap::explain(model,   # This is for the baseline logistic regression
                                    feature_names = "previous_Hb",
                                    newdata = as.data.frame(validate2 %>% select(previous_Hb)),
                                    exact = TRUE)
        } else if ("stanfit" %in% class(model)) {
          shap <- fastshap::explain(model, 
                                    X = as.data.frame(validate2), 
                                    #newdata = as.data.frame(validate), 
                                    feature_names = setdiff(colnames(validate2), "donb"),
                                    pred_wrapper = pfun, 
                                    nsim = nsim)
        } else {
          shap <- fastshap::explain(model, 
                                    X = as.data.frame(validate2),
                                    pred_wrapper = pfun, 
                                    nsim = nsim)
        }
      })
      shap <- as_tibble(shap)  # This drops the class "explain"
    }
    
  )
  if (is.null(result_code)) {
    return(NULL)
  }
  
  if (any(shap %>% mutate(c=if_any(everything(), is.na)) %>% pull(c)) || nrow(shap) == 2*n) {
    warning("Predict function failed in compute_shap_values_fastshap. You could try rerunning with a different seed")
    return(NULL)
  }
  n <- nrow(validate2)
  attributions <- shap %>% mutate(id=1:n)
  attributions <- pivot_longer(attributions, cols=!id) %>%
    select(Variable=name, id, attribution=value)
  features <- validate2 %>% mutate(id=1:n)
  if ("sex" %in% colnames(validate2)) {
    features <- features %>% mutate(sex = sex=="female")
  }
  features <- features %>% pivot_longer(cols=!id) %>%
    select(Variable=name, id, value=value)
  res <- inner_join(attributions, features, by=c("Variable", "id")) %>%
    group_by(Variable) %>%
    mutate(value=scale(value)[,1]) %>%
    ungroup()
  
  res <- res %>% select(-id) %>% slice_sample(prop = 1.0)  # permute the rows
  
  return(res)
}

plot_summary_shap_values <- function(df, variables_renamed) {
  message("In function plot_summary_shap_values")
  # The global attribution of a variable is the average of the absolute values of local attributions 
  res2 <- df %>% 
    mutate(attribution=abs(attribution)) %>% 
    group_by(Variable) %>% 
    summarise(attribution=mean(attribution))
  res2 <- left_join(res2, bind_rows(descript, donor_descript), by=c("Variable"="Variable")) %>% 
    select(Variable, Pretty, attribution)
  res2 <- prettify_variables(res2, variables_renamed)
  
  shap_plot_rf <- res2 %>% ggplot(aes(x=reorder(Pretty, attribution), y=attribution)) +
    geom_col(alpha=0.7) + 
    coord_flip() + 
    xlab("Variable") + ylab("Mean absolute attribution")
  return(shap_plot_rf)
}

plot_shap_values <- function(df, variables_renamed) {
  message("In function plot_shap_values")
  res2 <- df %>%   # Drop variables that have attribution of all observations equal to zero
    group_by(Variable) %>% 
    filter(!all(attribution==0.0)) %>%
    ungroup()
  res2 <- left_join(res2, bind_rows(descript, donor_descript), by=c("Variable"="Variable")) %>% 
    select(Variable, Pretty, attribution, value)
  res2 <- prettify_variables(res2, variables_renamed)

  shap_plot_rf <- res2 %>% 
    mutate(value = if_else(abs(value) <= 3, value, NA_real_)) %>% # mark outliers
    ggplot(aes(fct_rev(Pretty), attribution, color=value)) + 
    #ggplot(aes(Variable, attribution)) + 
    geom_hline(yintercept = 0) +
    ggforce::geom_sina() + 
    coord_flip() + 
    scale_color_gradient(low="blue", high="red", na.value="black") + 
    labs(x="Variable", y="Attribution", color="Variable value")
  # shap_plot_rf <- res2 %>% ggplot(aes(x=reorder(Pretty, attribution), y=attribution)) +
  #   geom_col(alpha=0.7) + 
  #   coord_flip() + 
  #   xlab("Variable") + ylab("Mean absolute attribution")
  return(shap_plot_rf)
}

# Constructor of the MyLogger class
# S3 class
new_logger <- function(prefix="", file="", silent=FALSE) {
  stopifnot(is.character(prefix))
  structure(list(prefix=prefix, file=file, silent=silent), class = "MyLogger")
}

print.MyLogger <- function(object, msg) {
  if (! object$silent) {
    cat(object$prefix, msg, file=object$file, append=TRUE)
  }
}

set_cores_options <- function(cores) {
  number_of_cores <- parallel::detectCores()
  if (!is.null(cores)) {
    number_of_cores <- min(cores, number_of_cores)
    options(mc.cores   = number_of_cores)   # This option is in package parallel
    options(boot.ncpus = number_of_cores)   # For bootstrapping
  } else {
    options(mc.cores   = number_of_cores)
    options(boot.ncpus = number_of_cores)
  }
  return(number_of_cores)
}
