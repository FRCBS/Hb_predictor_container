#library(Rook)  
#library(data.table, quietly=TRUE)

# Start in src directory with
# Rscript docker-server-plumber.R

container_version="0.23"

message(paste0("Working directory is ", getwd(), "\n"))
#setwd("src")
source("new_preprocess.R")
source("sanquin_preprocess.R")
source("helper_functions.R")
source("common.R")
# plumber.R

library(readr)
library(rjson)




# default values for parameters
default_max_diff_date_first_donation <- 60
default_Hb_cutoff_male   <- 135
default_Hb_cutoff_female <- 125
default_Hb_input_unit <- "gperl"

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# Show metadata about an uploaded file
get_info <- function(x) {
  v = c("<pre>",
    paste("Filename:", x$filename),
    paste("Tempfile:", x$tempfile),
    paste("Content type:", x$content_type),
    paste("Content length:", as.character(file.info(x$tempfile)$size)),
    "</pre>")
  return(paste(v, collapse="\n"))
}

check_columns <- function(got, expected) {
  if (all(expected %in% got)) {
    return("")
  } else {
    msg <- sprintf("Expected columns %s, got columns %s, missing %s", 
                   paste(expected, collapse=" "), 
                   paste(got, collapse=" "), 
                   paste(setdiff(expected, got), collapse=" "))
    return(msg)
  }
}

create_summary_table <- function(summary_table) {
  cols <- c("Model"="Pretty", "Sex", "MAE (g / L)", "RMSE (g / L)", "MAE (mmol / L)", "RMSE (mmol / L)", 
            "AUROC" = "AUROC value", "AUPR" = "AUPR value", "F1" = "F1 value")
  summary_table_string <- kable(summary_table %>% select(!!!cols), format="html", digits=3, 
                                caption="Error and performance measures: mean absolute error (MAE), root mean squared error (RMSE),\
                           area under ROC curve (AUROC), and area under precision-recall curve (AUPR).",
                                align="llllll",
                                table.attr = "id='errors_table' class='table table-condensed'")
  return(summary_table_string)
}

model_df <- tribble(
  ~model, ~pretty, ~rmd,
  "dt", "decision tree",             "template.Rmd",
  "rf", "random forest",             "random_forest.Rmd",
  "svm",           "support vector machine",    "svm.Rmd",
  "lmm",           "linear mixed model",        "linear_models.Rmd",
  "dlmm",          "dynamic linear mixed model","linear_models.Rmd",
  "both",          "Linear mixed models",       "linear_models.Rmd",
)

FRCBS_hyperparameters <- tribble(
  ~Model,          ~Sex,     ~Value,
  "rf",            "male",   list(mtry=4, splitrule="hellinger", min.node.size=34),
  "rf",            "female", list(mtry=4, splitrule="hellinger", min.node.size=34),
  "rf",            "both",   list(mtry=4, splitrule="hellinger", min.node.size=34),
  "svm",           "male",   list(degree=3, scale=0.1, C=1),
  "svm",           "female", list(degree=3, scale=0.1, C=1),
  "svm",           "both",   list(degree=3, scale=0.1, C=1)
)

Sanquin_hyperparameters <- FRCBS_hyperparameters

#learn_hyperparameters <- tibble(Model=character(0), Sex=character(0), Value=list())
# If empty tibble, we cannot save it to json
empty_hyperparameters <- tibble(Model="dummy", Sex="dummy", Value=list(list()))  


#' Upload the datas and parameters
#' @post /hb-predictor2
#' @parser multi
#' @serializer json
hb_predictor2 <- function(req){
  cat(sprintf("Container version is %s\n", container_version))
  if (Sys.getenv("TZ") == "") {
    Sys.setenv("TZ"="Europe/Helsinki")
  }
  # How much memory is available/used
  system("free -h")
  
  # How many cores are available
  cat(sprintf("Number of available cores: %i\n", parallel::detectCores()))

  ################################################
  #  
  # Parse uploaded files and other form parameters
  #
  ################################################

  cat("Before multipart$parse\n")
  tic("Parsing form data")
  #saveRDS(req, file="~/test_multipart_form_data/request.rds")
  
  #post = Rook::Multipart$parse(req)
  
  #separator <- req$postBody[[1]]
  #readr::write_lines(req$postBody, "/tmp/poista.bin", sep="\r\n")
  cat("tassa\n")
  #  writeBin(req$bodyRaw, con="/tmp/raw_form_data.bin")
  # Copy binary data from connection req$.bodyData to file "/tmp/raw_form_data.bin"
  to <- file("/tmp/raw_form_data.bin", "w+b")
  cat("tassa2\n")
  while (TRUE) {
    #cat("tassa3\n")
    data <- readBin(req$.bodyData, what="raw", 1024)
    if (length(data) == 0) break
    writeBin(data, to)
  }
  #writeBin(req$.bodyData, con="/tmp/raw_form_data.bin")
  cat("here3")
  close(to)
  return(list())
}



# Do the actual processing
hb_predictor3 <- function(ws) {
  separator <- ""
  command <- sprintf("./parse /tmp/raw_form_data.bin \"%s\" /tmp/parsed.json", separator)
  system(command)
  post <- rjson::fromJSON(file="/tmp/parsed.json")
  unlink("/tmp/raw_form_data.bin")
  unlink("/tmp/parsed.json")
  toc()
  
  ws$send(rjson::toJSON(list(type="status", status="Reading parameters")))
  
  #saveRDS(post, file="~/test_multipart_form_data/post.rds")
  cat("After multipart$parse\n")
  str(post, nchar.max = 10000)

  ######################################
  #
  # Check correctness of form parameters
  #
  ######################################
  error_messages=c()
  if (!("donations_fileUpload" %in% names(post)))
    error_messages <- c(error_messages, "You did not upload the donations file!")
  if (!("donors_fileUpload" %in% names(post)))
    error_messages <- c(error_messages, "You did not upload the donors file!")
  if ("Hb_cutoff_male" %in% names(post) && as.numeric(post$Hb_cutoff_male) <= 0)
    error_messages <- c(error_messages, "The Hb cutoff must be a positive number")
  if ("Hb_cutoff_female" %in% names(post) && as.numeric(post$Hb_cutoff_female) <= 0)
    error_messages <- c(error_messages, "The Hb cutoff must be a positive number")
  if ("unit" %in% names(post) && ! post$unit %in% c("gperl", "gperdl", "mmolperl"))
    error_messages <- c(error_messages, "The Hb unit must be either gperl, gperdl, or mmolperl")
  if ("hyperparameters" %in% names(post) && ! post$hyperparameters %in% c("finnish", "dutch", "upload", "learn"))
    error_messages <- c(error_messages, "The hyperparameter option must be either finnish, dutch, upload, or learn")
  if ("mode" %in% names(post) && ! post$mode %in% c("initial", "final"))
    error_messages <- c(error_messages, "The experiment mode must be either initial or final")
  if ("sample_fraction" %in% names(post)) {
    sf <- as.numeric(post$sample_fraction)
    if (is.na(sf) || sf < 0.0 || (sf > 1.0 && !is.wholenumber(sf)))
      error_messages <- c(error_messages, "The sample fraction must be a real number between 0 and 1, or an integer larger than 1")
  }
  if ("max_diff_date_first_donation" %in% names(post) && as.integer(post$max_diff_date_first_donation) < 0)
    error_messages <- c(error_messages, "The max tolerance in DONOR_DATE_FIRST_DONATION must be a non-negative integer")
  if (! "donations_fileUpload" %in% names(post))
    error_messages <- c(error_messages, "Missing donations file")
  if (! "donors_fileUpload" %in% names(post))
    error_messages <- c(error_messages, "Missing donors file")
    
  
  donations_o = post$donations_fileUpload
  donors_o = post$donors_fileUpload
  
  if ("donor_specific_fileUpload" %in% names(post) && post$donor_specific_fileUpload$filename != "") {
    donor_specific_filename <- post$donor_specific_fileUpload$tempfile
  } else {
    donor_specific_filename <- NULL
  }
  
  input_format = post$input_format
  if (! input_format %in% c("FRCBS", "Sanquin", "Preprocessed"))
    error_messages <- c(error_messages, "The input format should be either FRCBS, Sanquin, or Preprocessed")

  if (length(error_messages) > 0)
    return(list(type="final", error_messages=error_messages))
  
  cat(sprintf("The input format is %s\n", input_format))
  
  use_only_first_ferritin <- "use-only-first-ferritin" %in% names(post)
  cat(sprintf("The parameter use_only_first_ferritin is %s\n", as.character(use_only_first_ferritin)))

  stratify_by_sex <- "stratify-by-sex" %in% names(post)
  cat(sprintf("The parameter stratify_by_sex is %s\n", as.character(stratify_by_sex)))
  
  max_diff_date_first_donation <- ifelse ("max_diff_date_first_donation" %in% names(post), 
                                          as.integer(post$max_diff_date_first_donation), default_max_diff_date_first_donation)
  cat(sprintf("The parameter max_diff_date_first_donation is %i\n", max_diff_date_first_donation))
  
  Hb_input_unit <- ifelse ("unit" %in% names(post), post$unit, default_Hb_input_unit)
  cat(sprintf("The parameter Hb_input_unit is %s\n", Hb_input_unit))
  
  hyperparameters <- ifelse ("hyperparameters" %in% names(post), post$hyperparameters, "learn")
  cat(sprintf("The parameter hyperparameters is %s\n", hyperparameters))
  
  
  ####################################################################
  #
  # Check that the donation and donor dataframes are in correct format
  #
  ####################################################################
  
  if (input_format != "Preprocessed") {
    str(donors_o, nchar.max = 10000)
    upload_info <- c(get_info(donations_o), get_info(donors_o))
    use_col_names <- input_format != "FRCBS"
    
    # Check donation dataframe
    donations <- read_delim(donations_o$tempfile, col_names=use_col_names, delim='|')
    if (input_format == "FRCBS") {
      if (ncol(donations) != 12) {
        error_messages <- c(error_messages, sprintf("Expected 12 columns in the donation file, got %i columns", ncol(donations)))
        return(list(type="final", error_messages=error_messages))
      }
    } else {
      msg <- check_columns(names(donations), c("KEY_DONOR", "KEY_DONAT_INDEX_DATE", "DONAT_PHLEB_START", "DONAT_STATUS", "KEY_DONAT_PHLEB", 
                                              "DONAT_VOL_DRAWN", "DONAT_RESULT_CODE"))
      if(str_length(msg) > 0) {
        error_messages <- c(error_messages, sprintf("donation dataframe: %s", msg))
        return(list(type="final", error_messages=error_messages))
      }
    }
    # Give intermediate results to the browser
    donation_info <- sprintf("<p>Donations: filename=%s, rows=%i, columns=%i</p>", donations_o$filename, nrow(donations), ncol(donations))
    ws$send(rjson::toJSON(list(type="info", result=donation_info)))
    
    # Check donor dataframe
    donors <- read_delim(donors_o$tempfile, col_names=use_col_names, delim='|')
    if (input_format == "FRCBS") {
      if (ncol(donors) != 26) {
        error_messages <- c(error_messages, sprintf("Expected 26 columns in the donor file, got %i columns", ncol(donors)))
        return(list(type="final", error_messages=error_messages))
      }
    } else {
      required_donor_variables <- c("KEY_DONOR", "KEY_DONOR_SEX", "KEY_DONOR_DOB", "DONOR_DATE_FIRST_DONATION")
      msg <- check_columns(names(donors), required_donor_variables)
      if(str_length(msg) > 0) {
        error_messages <- c(error_messages, sprintf("donor dataframe: %s", msg))
        return(list(type="final", error_messages=error_messages))
      } else {
        message("Check if we got additional variables")
        additional_variables <- setdiff(names(donors), required_donor_variables) %>%
          keep(function(name) is.numeric(donors[[name]]) || name == "DONOR_DATE_FIRST_DONATION")
        cat(sprintf("Got the following additional numeric donor specific variables: %s\n", paste(additional_variables, collapse=" ")))
      }
    }
    # Give intermediate results to the browser
    donor_info <- sprintf("<p>Donor: filename=%s, rows=%i, columns=%i</p>", donors_o$filename, nrow(donors), ncol(donors))
    ws$send(rjson::toJSON(list(type="info", result=donor_info)))
  } else {
    donation_info <- ""
    donor_info <- ""
  }
  
  # Create the input parameter list for the Rmd files that do the actual prediction    
  myparams <- list()
  

  # From now on the cutoffs are in units of g/L
  if ("Hb_cutoff_male" %in% names(post)) { 
    value <- as.numeric(post$Hb_cutoff_male)
    if (! is_hb_value_sane(value, Hb_input_unit)) {
      warning(sprintf("The male Hb cutoff value %f does not seem to match its unit\n", value))
    }
    myparams$Hb_cutoff_male <- convert_hb_unit(Hb_input_unit, "gperl", value)
  } else {
    myparams$Hb_cutoff_male <- default_Hb_cutoff_male
  }
  if ("Hb_cutoff_female" %in% names(post)) { 
    value <- as.numeric(post$Hb_cutoff_female)
    if (! is_hb_value_sane(value, Hb_input_unit)) {
      warning(sprintf("The female Hb cutoff value %f does not seem to match its unit\n", value))
    }
    myparams$Hb_cutoff_female <- convert_hb_unit(Hb_input_unit, "gperl", value)
  } else {
    myparams$Hb_cutoff_female <- default_Hb_cutoff_female
  }

  
  myparams$skip_train <- FALSE
  myparams$create_datasets_bool <- TRUE
  #    myparams$skip_train <- TRUE
  #    myparams$create_datasets_bool <- FALSE
  
  ################################
  #
  # Do the preprocessing
  #
  ################################
  if (input_format == "Preprocessed") {
    donation_specific_filename <- post$preprocessed_fileUpload$tempfile
    fulldata_preprocessed <- load_single(donation_specific_filename)
    preprocessed_info <- sprintf("<p>Preprocessed data: rows=%i, columns=%i</p>", nrow(fulldata_preprocessed), ncol(fulldata_preprocessed))
    ws$send(rjson::toJSON(list(type="info", result=preprocessed_info)))
    if (sf != 1.0) {
      fulldata_preprocessed <- stratified_sample(fulldata_preprocessed, stratify_by_sex, sf, donor_field = "donor", sex_field = "sex")
    }
  } else {
    # Do the preprocessing
    tic("Preprocessing data")
    ws$send(rjson::toJSON(list(type="status", status="Preprocessing")))
    if (input_format == "FRCBS") {
      donations <- read_donations(donations_o$tempfile)
      donors <- read_donors(donors_o$tempfile)
      donors <- split_set3(donors)  # label the donors to either train, validate, or test
      if (sf != 1.0) {
        donors <- stratified_sample(donors, stratify_by_sex, sf)
        donations <- semi_join(donations, donors, by="KEY_DONOR")
      }
      fulldata_preprocessed <- preprocess(donations, donors,
                                          myparams$Hb_cutoff_male, myparams$Hb_cutoff_female, Hb_input_unit)
    } else {  # Sanquin
      donations <- read_sanquin_donations(donations_o$tempfile)
      donors <- read_sanquin_donors(donors_o$tempfile)
      donors <- split_set3(donors)  # label the donors to either train, validate, or test
      if (sf != 1.0) {
        donors <- stratified_sample(donors, stratify_by_sex, sf)
        donations <- semi_join(donations, donors, by="KEY_DONOR")
      }
      fulldata_preprocessed <- sanquin_preprocess(donations, donors,
                                                  #donations_o$tempfile, donors_o$tempfile,
                                                  myparams$Hb_cutoff_male, myparams$Hb_cutoff_female, Hb_input_unit,
                                                  max_diff_date_first_donation)
      if ("FERRITIN_FIRST" %in% names(donors)) {
        cat("hep\n")

        donor_specific <- sanquin_preprocess_donor_specific(donors, fulldata_preprocessed, use_only_first_ferritin)
        
        donor_specific_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
        save(donor_specific, file = donor_specific_filename)
        cat(sprintf("Saved donor specific variables (%ix%i) to file %s\n", nrow(donor_specific), ncol(donor_specific), donor_specific_filename))
      } else {
        cat("No ferritin information found.\n")
      }
    }
    post$sample_fraction <- 1.0   # Do not repeat the sampling in the Rmd files
    preprocessed_info <- sprintf("<p>Preprocessed data: rows=%i, columns=%i</p>", nrow(fulldata_preprocessed), ncol(fulldata_preprocessed))
    ws$send(rjson::toJSON(list(type="info", result=preprocessed_info)))
    #donation_specific_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
    donation_specific_filename <- "../output/preprocessed.rdata"
    save(fulldata_preprocessed, file=donation_specific_filename)
    toc()
    message(sprintf("Saved preprocessed data to file %s\n", donation_specific_filename))
  }
  print(summary(fulldata_preprocessed))
  
  cat("Distribution of time series length\n")
  print(fulldata_preprocessed %>% count(donor, name="Length") %>% count(Length, name="Count"))
  
  if (stratify_by_sex) {
    male_donation_specific_filename   <- "../output/male_preprocessed.rdata"
    female_donation_specific_filename <- "../output/female_preprocessed.rdata"
    tmp <- fulldata_preprocessed %>% filter(sex=="male")
    save(tmp,   file=male_donation_specific_filename)
    tmp <- fulldata_preprocessed %>% filter(sex=="female")
    save(tmp, file=female_donation_specific_filename)
    rm(tmp)
  } else {
    male_donation_specific_filename <- NA_character_
    female_donation_specific_filename <- NA_character_
  }
  rm(fulldata_preprocessed)
  
  ################################
  #
  # Create parameter list for Rmds
  #
  ################################
  
  myparams$input_file <- donation_specific_filename
  if ("sample_fraction" %in% names(post))
    myparams$sample_fraction <- as.numeric(post$sample_fraction)
  if ("hlen" %in% names(post))       # Minimum length of time series
    myparams$hlen <- as.integer(post$hlen)
  if (!is.null(donor_specific_filename))
    myparams$donor_specific_file <- donor_specific_filename
  
  predictive_variables <- c()
  for (parameter_name in names(post)) {
    if (str_starts(parameter_name, "dv_")) predictive_variables <- append(predictive_variables, str_remove(parameter_name, "^dv_")) 
  }
  if (stratify_by_sex) {
    predictive_variables <- setdiff(predictive_variables, "sex")
  }
  print(predictive_variables)
  myparams$predictive_variables <- paste(predictive_variables, sep=",")
  
  #myparams$hyperparameters <- "../output/hyperparameters.rds"   # binary format
  myparams$hyperparameters <- "../output/hyperparameters.json"
  if (hyperparameters == "finnish") {
    write_hyperparameters(FRCBS_hyperparameters, myparams$hyperparameters)
  } else if (hyperparameters == "dutch") {
    write_hyperparameters(Sanquin_hyperparameters, myparams$hyperparameters)
  } else if (hyperparameters == "learn") {
    write_hyperparameters(empty_hyperparameters, myparams$hyperparameters)  # Empty dataframe
  } else if (hyperparameters == "upload") {
    cmd <- sprintf("cp %s %s", post$hyperparameter_fileUpload$tempfile, myparams$hyperparameters)
    system(cmd)
  }
  
  myparams$mode <- post$mode
  
  # This debugging information will be sent to the browser NOT DONE CURRENTLY! FIX THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  myparams_string <- paste( map_chr(names(myparams), function (name) sprintf("<li>%s=%s</li>", name, myparams[name])), 
                            collapse="\n")
  myparams_string <- sprintf("<p>Parameters are:</p>\n <ul>%s\n</ul>\n", myparams_string)
  cat(myparams_string)
  
  
  summary_tables <- list()
  effect_size_tables <- list()
  variable_importance_tables <- list()
  prediction_tables <- list()
  #details_df <- tibble(id=character(0), pretty=character(0), sex=character(0), html=character(0), pdf=character(0))
  details_dfs <- list()
  


  
  ###################
  #
  # Run models
  #
  ###################

  # This looks ugly because lmm and dlmm are done in the same Rmd.
  tmp <- intersect(c("lmm", "dlmm"), names(post))
  if (length(tmp) == 0) {
    linear_models <- NULL
  } else {
    linear_models <- case_when(
      length(tmp) == 1 ~ tmp[[1]],
      length(tmp) == 2 ~ "both")
  }
  
  models <- intersect(c("dt", "rf", "svm"), names(post))
  models <- c(models, linear_models)
  for (m in models) {
    cat(sprintf("Running model %s\n", m))
    myparams$model <- m
    is_linear_model <- m %in% c("lmm", "dlmm", "both")
    pretty <- model_df %>% filter(model==m) %>% pull(pretty)
    rmd <- model_df %>% filter(model==m) %>% pull(rmd)
    #sexess <- if (stratify_by_sex && m != "random-forest")  c("male", "female") else c("both")
    sexes <- if (stratify_by_sex)  c("male", "female") else c("both")
    for (sex in sexes) {
      cat(sprintf("Running sex %s\n", sex))
      ws$send(rjson::toJSON(list(type="status", status=sprintf("Running %s %s", ifelse(sex=="both", "", sex), pretty))))
      myparams["sex"] <- sex
      myparams$input_file <- case_when(sex=="both" ~ donation_specific_filename,
                                       sex == "male" ~ male_donation_specific_filename,
                                       sex == "female" ~ female_donation_specific_filename
      )
      
      temp_filename <- sprintf("/tmp/summary-%s-%s.csv", m, sex)
      myparams["summary_table_file"] <- temp_filename
      
      effect_size_filename <- ifelse(is_linear_model,
                                     sprintf("/tmp/effect-size-%s-%s.csv", m, sex),
                                     sprintf("/tmp/variable-importance-%s-%s.csv", m, sex))
      myparams["effect_size_table_file"] <- effect_size_filename
      
      prediction_filename <- sprintf("/tmp/prediction-%s-%s.csv", m, sex)
      myparams["prediction_table_file"] <- prediction_filename
      
      #     effect_size_filename <- sprintf("/tmp/effect-size-%s.csv", sex)
      #     myparams["effect_size_table_file"] <- effect_size_filename
      
      
      error_messages <- tryCatch(
        withCallingHandlers({
          rmarkdown::render(
            rmd,
            #'template.Rmd',
            output_file=rep(sprintf('results-%s-%s', m, sex), 2),   # One for each output format: html and pdf 
            output_format=c('html_document', 'pdf_document'),
            clean=FALSE,
            output_dir='../output',
            params = myparams)
          NULL
        }, warning = function(w) ws$send(rjson::toJSON(list(type="warning", 
                                                            warning_messages=c(sprintf("Warning in %s call \n", pretty),
                                                                               w$message))))),
        error = function(cnd) {
          error_messages <- c(sprintf("Error in %s %s call \n", sex, pretty), cnd$message)
          return(error_messages)
        }
      )
      if (!is.null(error_messages)) {
        cat(paste0(error_messages))
        cat("\n")
        ws$send(rjson::toJSON(list(type="error", error_messages=error_messages)))
        next  # break
      }
      
      message("x1")
      
      s <- read_csv(temp_filename)
      summary_tables[[paste(m, sex, sep="-")]] <- s
      ws$send(rjson::toJSON(list(type="summary", summary_table_string = create_summary_table(bind_rows(summary_tables)))))
      
      message("x2")

      p <- read_csv(prediction_filename)
      prediction_tables[[paste(m, sex, sep="-")]] <- p
      
      message("x3")
      
      t <-
        tibble(id=sprintf("detail-%s-%s", m, sex), 
                pretty=str_to_sentence(pretty),
                sex=sex,
                html=sprintf("output/results-%s-%s.html", m, sex),
                pdf=sprintf("output/results-%s-%s.pdf", m, sex))
      details_dfs[[length(details_dfs)+1]] <- t
      ws$send(rjson::toJSON(list(type="detail", details_df = purrr::transpose(t))))
      
      message("x4")
      
      if (is_linear_model) {
        effect_size_tables[[paste(m, sex, sep="-")]] <- read_csv(effect_size_filename)
      } else {
        variable_importance_tables[[paste(m, sex, sep="-")]] <- read_csv(effect_size_filename)
      }
    } # end for sexes
  } # end for models
  
  #unlink(donation_specific_filename)
  if (FALSE && !is.null(donor_specific_filename))
    unlink(donor_specific_filename)
  
  ####################
  #
  # Report the results
  #
  ####################
  
  summary_table <- bind_rows(summary_tables)
  write_excel_csv(summary_table, "../output/summary.csv")
  summary_table_string <- create_summary_table(summary_table)
  ws$send(rjson::toJSON(list(type="summary", summary_table_string = summary_table_string)))
  
  effect_size_table <- bind_rows(effect_size_tables)
  write_excel_csv(effect_size_table, "../output/effect-size.csv")
  
  variable_importance_table <- bind_rows(variable_importance_tables, .id="Id")
  write_excel_csv(variable_importance_table, "../output/variable-importance.csv")
  
  prediction_table <- bind_rows(prediction_tables)
  write_excel_csv(prediction_table, "../output/prediction.csv")
  
  ws$send(rjson::toJSON(list(type="status", status="Ready")))
  
  details_df <- bind_rows(details_dfs)
  return(list(type="final", summary_table=as.character(summary_table_string), details_df = purrr::transpose(details_df)))
  
}



#' Show the html page
#' @get /hb-predictor
#' @serializer html
hb_predictor <- function(req){
  response = sprintf('
  <html>
  
  <head>
  <meta charset="UTF-8"/>
  <title>Hemoglobin predictor</title>
  <link rel="stylesheet" href="static/bootstrap.min.css">
  <link rel="stylesheet" href="static/style.css">
  <script type="text/javascript" src="static/script.js"></script>
  </head>
  
  <body>
  <div id="version">Version %s</div> 
  <div id="container">
  
    <div id="logos">
    <img id="bs_logo" src="static/bloodservice_logo.png" />
    <img id="eba_logo" src="static/FundedbyEBA.jpg" />
    </div>
    
    <div id="content">
      <h1>Hemoglobin predictor</h1>
      <a href="hb-predictor">Go back to start</a>
      
      <div id="form-container">
        <form id="form" enctype="multipart/form-data" action="hb-predictor2" method="post">

        <fieldset id="input_format_fs">
          <legend>What is the input format?</legend>
          <label for="FRCBS">
            <input type="radio" value="FRCBS", id="FRCBS" name="input_format" />
            FRCBS
          </label>
          <label for="Sanquin">
            <input type="radio" value="Sanquin", id="Sanquin" name="input_format" checked/>
            Sanquin
          </label>
          <label for="Preprocessed">
            <input type="radio" value="Preprocessed", id="Preprocessed" name="input_format" />
            Preprocessed
          </label>
        </fieldset>
        
        <table id="input_table">
        <tr id="donations_row"><td>Upload donations file:</td> <td><input type=file name="donations_fileUpload"></td> </tr>
        <tr id="donors_row"><td>Upload donors file:</td>    <td><input type=file name="donors_fileUpload"></td> </tr>
        <tr id="donor_specific_row" style="display: none"><td>Upload donor specific file:</td>    <td><input type=file name="donor_specific_fileUpload"></td> </tr>
        <tr id="preprocessed_row" style="display: none"><td>Preprocessed file:</td>     <td><input type=file name="preprocessed_fileUpload"></td> </tr>
        <tr><td>Hb cutoff (male)</td>       <td><input id="Hb_cutoff_male" name="Hb_cutoff_male" value="%i" maxlength="5" size="5">
          <!--<span id="male_unit">g/L</span>--></td> </tr>
        <tr><td>Hb cutoff (female)</td>     <td><input id="Hb_cutoff_female" name="Hb_cutoff_female" value="%i" maxlength="5" size="5">
        <!--<span id="female_unit">g/L</span>--></td> </tr>
        <tr><td>Hb unit</td>                <td>
        <select id="unit" name="unit">
          <option value="gperl" label="g/L" selected>g/L</option>
          <option value="gperdl" label="g/dL">g/dL</option>
          <option value="mmolperl" label="mmol/L">mmol/L</option>
        </select>
        </td></tr>
        <tr><td>Minimum donations</td>      <td><input name="hlen" value="7" pattern="^[0-9]+$" maxlength="5" size="5"></td> </tr>
        <tr><td>Sample fraction/size</td>        <td><input name="sample_fraction" value="1.00" maxlength="5" size="5"></td> </tr>
        <tr id="max_diff_date_first_donation_row"><td>Max tolerance in DONOR_DATE_FIRST_DONATION</td>        <td><input name="max_diff_date_first_donation" value="%i" maxlength="5" size="5"></td> </tr>
        <tr id="use_only_first_ferritin_row"><td>Use only first ferritin value</td>
            <td>
            <input type="checkbox" value="on", id="use-only-first-ferritin" name="use-only-first-ferritin" />
            </td>
        </tr>
        <tr id="stratify_by_sex_row"><td>Stratify by sex</td>
            <td>
            <input type="checkbox" value="on", id="stratify-by-sex" name="stratify-by-sex" checked />
            </td>
        </tr>
        
        <tr><td>Hyperparameters</td>                <td>
        <select id="hyperparameters" name="hyperparameters">
          <option value="finnish" label="Finnish" selected>Finnish</option>
          <option value="dutch" label="Dutch">Dutch</option>
          <option value="upload" label="Upload">Upload</option>
          <option value="learn" label="Learn">Learn</option>
        </select>
        </td></tr>
        <tr id="hyperparameter_file_row" hidden>
          <td>Upload hyperparameter file:</td>    
          <td><input type=file name="hyperparameter_fileUpload"></td> 
        </tr>

        <tr><td>Mode</td>                <td>
        <select id="mode" name="mode">
          <option value="initial" label="Initial" selected>Initial</option>
          <option value="final" label="Final">Final</option>
        </select>
        </td></tr>
        
        </label>
        <!--<tr><td>Progress</td>               <td><progress id="progress" value="0" /></td></tr>-->
        </table>
        <!--
        <label for="use-only-first-ferritin">
            <input type="checkbox" value="on", id="use-only-first-ferritin" name="use-only-first-ferritin" />
            Use only first ferritin value
        </label>
        --> 
        
        <fieldset id="predictive-variables">
          <legend>Which predictive variables to use?</legend>
        </fieldset>
          
        <fieldset>
          <legend>Which prediction model to use?</legend>
          
          <label for="lmm">
            <input type="checkbox" value="on", id="lmm" name="lmm" />
            Linear mixed model
          </label>
          
          <label for="dlmm">
            <input type="checkbox" value="on", id="dlmm" name="dlmm" />
            Dynamic linear mixed model
          </label>

          <label for="decision-tree">
            <input type="checkbox" value="on", id="decision-tree" name="dt" />
            Mock decision tree
          </label>
          
          <label for="random-forest">
            <input type="checkbox" value="on", id="random-forest" name="rf" checked />
            Random forest
          </label>
          
          <label for="svm">
            <input type="checkbox" value="on", id="svm" name="svm"  />
            Support vector machine
          </label>
          
        </fieldset>
    
        <input id="submit" type="submit" value="Upload the files and start computing" name="submit_button" />
        </form>
      </div> <!-- id="form-container -->
      

      <div id="info-container" hidden>
        <h2>Progress info</h2>    
        <p>Computation started: <span id="start-time"></span></p>
        <p id="finish-time-container">Computation finished: <span id="finish-time"></span></p>
        <p>Elapsed time: <span id="time"></span></p>
        <p>Status: <span id="status"></span></p>
        <div id="spinner-container">
          <div class="lds-spinner" hidden ><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div></div>
        </div>
        <div id="error_messages"></div>
        <div id="warning_messages"></div>
        <div id="info"></div>
      </div>
      
      <div id="results-container" hidden>
        <h2>Results</h2>

        <h3>Summary</h3>
        <div id="table_container"></div>
        <table id="summary-table" class="table table-condensed">
        </table>
        <h3>Download the results</h3>
        <ul>
        <li> <a href="/output/summary.csv">Summary table</a> (CSV)</li>
        <li id="effect-size"> <a href="/output/effect-size.csv">Effect size table</a> (CSV)</li>
        <li id="variable-importance"> <a href="/output/variable-importance.csv">Variable importance table</a> (CSV)</li>
        <li id="download_hyperparameters"> <a href="/output/hyperparameters.json" target="_blank">Learned hyperparameters</a> (JSON)</li>
        <li id="preprocessed"> <a href="/output/preprocessed.rdata">Preprocessed data</a> (R binary)</li>
        <li id="train"> <a href="/output/train.csv">Train</a> </li>
        <li id="validate"> <a href="/output/validate.csv">Validate</a> </li>
        </ul>
        
        <h3>Detailed result pages</h3>
        <table id="detailed-results" class="table table-condensed">
        <tr> <th>Model</th> <th>Sex</th> <th>html</th> <th>pdf</th> </tr>
        </table>
      </div>
      
    </div>  
  
  </div>
  </body>
  </html>
  ', container_version, round(default_Hb_cutoff_male), round(default_Hb_cutoff_female), default_max_diff_date_first_donation)

  response
}

#' @assets ../output /output
list()

#' @assets ../static /static
list()
