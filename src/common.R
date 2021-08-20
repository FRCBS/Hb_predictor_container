# Variables that are in use
library(parallel)
library(doParallel)

descript <- tibble(Variable = c("donor", "Hb", "days_to_previous_fb", "age", "previous_Hb_def", 
                                "year", "warm_season", "consecutive_deferrals", "recent_donations",
                                "recent_deferrals", "hour", 
                                "previous_Hb", "Hb_first", "Hb_deferral", "sex"), 
                   Pretty = c("Donor ID", "Hemoglobin", "Days to previous full blood donation", "Age", "Previous Hb deferral", 
                              "Year", "Warm season", "Consecutive deferrals", "Recent donations", 
                              "Recent deferrals", "Hour", 
                              "Previous Hb", "First Hb", "Hb deferral", "Sex"),
                   Type = c("Factor", "numeric", "numeric (int)", "numeric", "boolean",
                            "numeric (int)", "boolean", "numeric (int)", "numeric (int)", "numeric (int)", "numeric",
                            "numeric", "numeric", "boolean", "Factor"),
                   Explanation = c("Donor identifier",
                                   "Amount of Hemoglobin",
                                   "Time (in days) between Hb measurement and previous full blood donation event",
                                   "Age of donor",
                                   "Indicates whether the donor was deferred from blood donation due to low hemoglobin at previous donation event",
                                   "Year of donation",
                                   "True if donation was given in April-September",
                                   "Number of times the donor has been deferred due to low hemoglobin since last succesful whole blood donation",
                                   "Number of donations in the last two years",
                                   "Number of deferrals due to low hemoglobin in the last two years",
                                   "Time of day when donation was given as hours (e.g. 13:45 = 13.75)",
                                   "Hb value at previous measurement (dynamic linear mixed model)",
                                   "Hb value at first donation of this donor (linear mixed model)",
                                   "Deferred based on low hemoglogin",
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

summary_plotter <- function(df, variable_descriptions, color) {
  g <- df %>%
    mutate(across(where(is.logical), as.integer)) %>%
    keep(is.numeric) %>%
#    keep(function(c) is.numeric(c) | is.factor(c)) %>%
    to_pretty(variable_descriptions) %>%
    gather() %>%
    mutate(key = factor(key, levels=variable_descriptions$Pretty))  %>% # Don't sort alphabetically
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(fill = color)
  return(g)
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

learn_hyperparameters <- function(df, method, search_grid, cores) {
  message("In function learn_hyperparameters")
  df <- df %>% filter(label=="train") %>% select(-label) # Drop donors that belong to the original validate set
  #file <- "../output/learned_hyperparameters.Rdata"
  
  #Initialise parallellisation
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  
  
  #Define cross validation
  fitControl <- caret::trainControl(## 4-fold CV
    method = "repeatedcv",
    number = 4, #how many is good?
    ## repeated ten times
    repeats = 1, #how many is good?
    classProbs = TRUE,
    summaryFunction = twoClassSummary
    #savePredictions = TRUE
  )
  
  #Train
  rrfFit_roc_hyper <- caret::train(Hb_deferral ~ ., data = df, 
                                   method = method, 
                                   trControl = fitControl, 
                                   verbose = FALSE, 
                                   ## Now specify the exact models 
                                   ## to evaluate:
                                   tuneGrid = search_grid,
                                   metric="ROC"
                                   #should we use Kappa or ROC?
                                   #                importance = "permutation"
                                   #https://stackoverflow.com/questions/18578861/variable-importance-using-the-caret-package-error-randomforest-algorithm
                                   #should we use , ’impurity’, ’impurity_corrected’, ’permutation’ ?
  )
  
  #save(rrfFit_roc_hyper, file=file)
  parallel::stopCluster(cl)
  return(as.list(rrfFit_roc_hyper$bestTune))
}

# This is currently used by the RF and SVM models.
# These should be dropped or incorporated into the main preprocessing.
# Note that this also contains Finnish specific donation intervals 62 and 92
additional_preprocess <- function(data, variables) {
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>% group_by(donor) %>%
    dplyr::filter(n()>1) %>%  #Take out the ones with only one event 
    slice_max(order_by = dateonly, n=1) %>%
    mutate(nb_donat = sum( nb_donat_progesa, nb_donat_outside ,na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(Hb_deferral=factor(ifelse(Hb_deferral, "Deferred", "Accepted"), levels = c("Accepted","Deferred")))
  message(sprintf("Dropped %i / %i donations (%i / %i donors) due to length of time series being one\n", 
                  old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2))
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>%
    filter( !nb_donat == '0') #Some spurious mistake in the donation record
  message(sprintf("Dropped %i / %i donations (%i / %i donors) due to nb_donat being '0'\n", 
                  old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2))
  
  old_count <- nrow(data); old_count2 <- ndonor(data)
  data <- data %>% #Take out people that return faster than they should as we do not really know what they mean
    filter(!(days_to_previous_fb < 62 & sex == "male"),
           !(days_to_previous_fb < 92 & sex == "female"))
  message(sprintf("Dropped %i / %i donations (%i / %i donors) due to days_to_previous_fb < 62 (men) or 92 (women)\n", 
                  old_count - nrow(data), old_count, old_count2 - ndonor(data), old_count2))
  
  
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

gather_results <- function(df, Id, Model, Pretty, Sex) {
  message("In gather_results function")
  #boot.n = 2000
  result <- list()
  result$df <- df
  result$confusion_matrix_plot <- create_confusion_matrix_plot(df$original_label, df$predicted_label)
  result$roc_plot <- create_roc_new(df$original_label, df$score) #, boot.n=boot.n)
  ret <- tryCatch(
    error =  function(cnd) {
      message(paste("in error handler, possibly the deferral score is constant", cnd))
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
  result$f1_ci <- get_f1_ci(df)#, boot.n=boot.n)
  result$f1_ci
  message("seko2")
  
  t <- tibble(Id=Id, Model=Model, Pretty=Pretty, Sex=Sex, 
              "MAE (g / L)"    = NA, "RMSE (g / L)"    = NA, 
              "MAE (mmol / L)" = NA, "RMSE (mmol / L)" = NA) 
  
  message("Here7")
  t <- bind_cols(c(t, result$roc_plot$roc_ci, result$pr_plot$pr_ci, result$f1_ci))
  result$summary <- t
  
  #result$f1 <- get_f1(tibble(obs = factor(ifelse(df$deferral == 1, "Deferred", "Accepted"), levels=c("Accepted", "Deferred")),
  #                    Deferred = df$scores))
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
  
  
  pboth <- data %>%
    filter(first_event == FALSE) %>%
    select(all_of(donation_description$Variable))
  
  pboth <- summary_plotter(pboth, donation_description, color)
  pboth
  
  
  temp_donor_specific <- at_least_one_deferral
  #tr <- tibble_row(Variable="one_deferral", Pretty="At least one deferral", Type="numeric (int)", Explanation="At least one deferral")
  if (!is.null(donor_specific_variables)) {
    temp_donor_specific <- donor_specific_variables %>% 
      inner_join(at_least_one_deferral, by="donor")
  }
  
  message(sprintf("Column names are: %s\n", paste(names(temp_donor_specific), collapse=" ")))
  
  pdata2 <- temp_donor_specific %>%
    filter(donor %in% temp_donors)
  pdata2 <- summary_plotter(pdata2, 
                            donor_description,# %>% add_row(tr), 
                            color)
  pdata2
  
  lengths <- time_series_length_plotter(data, color)
  return(list(pboth, pdata2, lengths))
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

compute_shap_values <- function(model, train, validate, variables) {
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

plot_summary_shap_values <- function(df, variables_renamed) {
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
