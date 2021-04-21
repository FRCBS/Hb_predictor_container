#library(Rook)  
#library(data.table, quietly=TRUE)

# Start in src directory with
# Rscript docker-server-plumber.R

container_version="0.19"

message(paste0("Working directory is ", getwd(), "\n"))
#setwd("src")
source("new_preprocess.R")
source("sanquin_preprocess.R")
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
    msg <- sprintf("Expected columns %s, got columns %s, missing %s", paste(expected, collapse=" "), paste(got, collapse=" "), paste(setdiff(expected, got), collapse=" "))
  }
}



#' Read the input form and learn the models, and show the results.
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
  close(to)
  return(list())
}

hb_predictor3 <- function() {
  separator <- ""
  command <- sprintf("./parse /tmp/raw_form_data.bin \"%s\" /tmp/parsed.json", separator)
  system(command)
  post <- rjson::fromJSON(file="/tmp/parsed.json")
  unlink("/tmp/raw_form_data.bin")
  unlink("/tmp/parsed.json")
  toc()
  
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
    return(list(error_messages=error_messages))
  
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
        return(list(error_messages=error_messages))
      }
    } else {
      msg <- check_columns(names(donations), c("KEY_DONOR", "KEY_DONAT_INDEX_DATE", "DONAT_PHLEB_START", "DONAT_STATUS", "KEY_DONAT_PHLEB", 
                                              "DONAT_VOL_DRAWN", "DONAT_RESULT_CODE"))
      if(str_length(msg) > 0) {
        error_messages <- c(error_messages, sprintf("donation dataframe: %s", msg))
        return(list(error_messages=error_messages))
      }
    }
    donation_info <- sprintf("<p>Donations: filename=%s, rows=%i, columns=%i</p>", donations_o$filename, nrow(donations), ncol(donations))

    # Check donor dataframe
    donors <- read_delim(donors_o$tempfile, col_names=use_col_names, delim='|')
    if (input_format == "FRCBS") {
      if (ncol(donors) != 26) {
        error_messages <- c(error_messages, sprintf("Expected 26 columns in the donor file, got %i columns", ncol(donors)))
        return(list(error_messages=error_messages))
      }
    } else {
      msg <- check_columns(names(donors), c("KEY_DONOR", "KEY_DONOR_SEX", "KEY_DONOR_DOB", "DONOR_DATE_FIRST_DONATION"))
      if(str_length(msg) > 0) {
        error_messages <- c(error_messages, sprintf("donor dataframe: %s", msg))
        return(list(error_messages=error_messages))
      }
    }
    donor_info <- sprintf("<p>Donor: filename=%s, rows=%i, columns=%i</p>", donors_o$filename, nrow(donors), ncol(donors))
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
  } else {
    # Do the preprocessing
    tic("Preprocessing data")
    if (input_format == "FRCBS") {
      if (sf != 1.0)
        sample_raw_progesa(donations_o$tempfile, donors_o$tempfile, donations_o$tempfile, donors_o$tempfile, ndonor=sf)
      fulldata_preprocessed <- preprocess(donations_o$tempfile, donors_o$tempfile,
                                          myparams$Hb_cutoff_male, myparams$Hb_cutoff_female, Hb_input_unit)
    } else {  # Sanquin
      res <- sanquin_sample_raw_progesa(donations_o$tempfile, donors_o$tempfile, donations_o$tempfile, donors_o$tempfile, ndonor=sf)
      fulldata_preprocessed <- sanquin_preprocess(donations_o$tempfile, donors_o$tempfile,
                                                  myparams$Hb_cutoff_male, myparams$Hb_cutoff_female, Hb_input_unit,
                                                  max_diff_date_first_donation)
      if ("FERRITIN_FIRST" %in% names(res$donor)) {
        cat("hep\n")

        donor_specific <- sanquin_preprocess_donor_specific(res$donor, fulldata_preprocessed, use_only_first_ferritin)
        
        donor_specific_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
        save(donor_specific, file = donor_specific_filename)
        cat(sprintf("Saved donor specific variables (%ix%i) to file %s\n", nrow(donor_specific), ncol(donor_specific), donor_specific_filename))
      } else {
        cat("No ferritin information found.\n")
      }
    }
    post$sample_fraction <- 1.0   # Do not repeat the sampling in the Rmd files
    preprocessed_info <- sprintf("<p>Preprocessed data: rows=%i, columns=%i</p>", nrow(fulldata_preprocessed), ncol(fulldata_preprocessed))
    #donation_specific_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
    donation_specific_filename <- "../output/preprocessed.rdata"
    save(fulldata_preprocessed, file=donation_specific_filename)
    toc()
    message(sprintf("Saved preprocessed data to file %s\n", donation_specific_filename))
  }
  
  cat("Distribution of time series length\n")
  print(fulldata_preprocessed %>% count(donor, name="Length") %>% count(Length, name="Count"))
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
  print(predictive_variables)
  myparams$predictive_variables <- paste(predictive_variables, sep=",")
  
  # This debugging information will be sent to the browser
  myparams_string <- paste( map_chr(names(myparams), function (name) sprintf("<li>%s=%s</li>", name, myparams[name])), 
                            collapse="\n")
  myparams_string <- sprintf("<p>Parameters are:</p>\n <ul>%s\n</ul>\n", myparams_string)
  
  
  
  summary_tables <- list()
  effect_size_tables <- list()
  details_df <- tibble(id=character(0), pretty=character(0), gender=character(0), html=character(0), pdf=character(0))
  
  ####################
  #
  # Run linear models
  #
  ####################
  
  methods <- intersect(c("lmm", "dlmm"), names(post))
  if (length(methods) > 0) {
    m <- ifelse(length(methods) == 2, "both", methods[[1]])
    myparams$method <- case_when(m=="lmm" ~ "no-fix", m=="dlmm" ~ "icp-fix", TRUE ~ m)
    for (gender in c("male", "female")) {
      myparams["gender"] <- gender
      filename <- sprintf("/tmp/summary-%s.csv", gender)
      myparams["summary_table_file"] <- filename
      effect_size_filename <- sprintf("/tmp/effect-size-%s.csv", gender)
      myparams["effect_size_table_file"] <- effect_size_filename
      error_messages <- tryCatch(
        error = function(cnd) {
          error_messages <- c(sprintf("Error in %s with sex %s.\n", "linear mixed model", gender), cnd$message)
          return(error_messages)
        },
        {
          rmarkdown::render(
            'linear_models.Rmd',
            output_file=rep(sprintf('results-%s', gender), 2),   # One for each output format: html and pdf 
            #output_file=sprintf('results-%s', gender),   # One for each output format: html and pdf 
            output_format=c('html_document', 'pdf_document'),
            #output_format=list('html_document', pdf_document(dev="pdf_cairo")),
            #output_format=c('html_document'),
            clean=FALSE,
            output_dir='../output',
            params = myparams)
          NULL
        }
      )
      if (!is.null(error_messages)) {
        cat(paste0(error_messages))
        return(list(error_messages=error_messages))
      }      
      summary_tables[[gender]] <- read_csv(filename)
      effect_size_tables[[gender]] <- read_csv(effect_size_filename)
      
      #m <- myparams$method
      pretty <- case_when(m=="lmm" ~ "Linear mixed model", m=="dlmm" ~ "Dynamic linear mixed model", TRUE ~ "Linear mixed models")
      details_df <- details_df %>% 
        add_row(id=sprintf("detail-linear-models-%s", gender), 
                pretty=pretty,
                gender=gender,
                html=sprintf("output/results-%s.html", gender),
                pdf=sprintf("output/results-%s.pdf", gender))
    }
  }
  
  ###################
  #
  # Run random forest
  #
  ###################

  method_df <- tribble(
    ~method, ~pretty, ~rmd,
    "decision-tree", "decision tree", "template.Rmd",
    "random-forest", "random forest", "random_forest.Rmd",
  )  
  methods <- intersect(c("decision-tree", "random-forest"), names(post))
  for (m in methods) {
    cat(sprintf("Running method %s\n", m))
    myparams$method <- m
    pretty <- method_df %>% filter(method==m) %>% pull(pretty)
    rmd <- method_df %>% filter(method==m) %>% pull(rmd)
    genders <- if (stratify_by_sex && m != "random-forest")  c("male", "female") else c("both")
    for (gender in genders) {
      cat(sprintf("Running gender %s\n", gender))
      myparams["gender"] <- gender
      temp_filename <- sprintf("/tmp/summary-%s-%s.csv", m, gender)
      myparams["summary_table_file"] <- temp_filename
      temp_effect_size_filename <- sprintf("../output/variable-importance-%s.csv", m)
      myparams["effect_size_table_file"] <- temp_effect_size_filename
      error_messages <- tryCatch(
        error = function(cnd) {
          error_messages <- c(sprintf("Error in %s call.\n", pretty), cnd$message)
          return(error_messages)
        },
        {
          rmarkdown::render(
            rmd,
            #'template.Rmd',
            output_file=rep(sprintf('results-%s-%s', m, gender), 2),   # One for each output format: html and pdf 
            output_format=c('html_document', 'pdf_document'),
            clean=FALSE,
            output_dir='../output',
            params = myparams)
          NULL
        }
      )
      if (!is.null(error_messages)) {
        cat(paste0(error_messages))
        return(list(error_messages=error_messages))
      }
      summary_tables[[paste(m, gender, sep="-")]] <- read_csv(temp_filename)
      
      details_df <- details_df %>% 
        add_row(id=sprintf("detail-%s-%s", m, gender), 
                pretty=str_to_sentence(pretty),
                gender=gender,
                html=sprintf("output/results-%s-%s.html", m, gender),
                pdf=sprintf("output/results-%s-%s.pdf", m, gender))
      
      # should I read here the effect size table?
      #effect_size_tables[[paste(m, gender, sep="-")]] <- read_csv(temp_effect_size_filename)
    }
  }
  
  #unlink(donation_specific_filename)
  if (!is.null(donor_specific_filename))
    unlink(donor_specific_filename)
  
  ####################
  #
  # Report the results
  #
  ####################
  
  summary_table <- bind_rows(summary_tables)
  effect_size_table <- bind_rows(effect_size_tables)
  write_csv(summary_table, "../output/summary.csv")
  write_csv(effect_size_table, "../output/effect-size.csv")
  cols <- c("Model", "Gender", "MAE (g / L)", "RMSE (g / L)", "MAE (mmol / L)", "RMSE (mmol / L)", "AUROC" = "AUROC value", "AUPR" = "AUPR value", "F1" = "F1 value")
  summary_table_string <- kable(summary_table %>% select(!!!cols), format="html", digits=3, 
                         caption="Error and performance measures: mean absolute error (MAE), root mean squared error (RMSE),\
                           area under ROC curve (AUROC), and area under precision-recall curve (AUPR).",
                         align="llllll",
                         table.attr = "id='errors_table' class='table table-condensed'")
  


  
  #result2 <- c(upload_info, s1, s2, s3, myparams_string, time_start_s, time_end_s, total_time)
  result2 <- c(donation_info, donor_info, preprocessed_info)
  result2 <- paste(result2, collapse="\n")
  
  
  return(list(result=result2, summary_table=as.character(summary_table_string), details_df = purrr::transpose(details_df)))
  
}



#' Show the html page
#' @get /hb-predictor
#' @serializer html
hb_predictor <- function(req){
  response = sprintf('
  <html>
  
  <head>
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
            <input type="checkbox" value="on", id="stratify-by-sex" name="stratify-by-sex" />
            </td>
        </tr>
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
            <input type="checkbox" value="on", id="decision-tree" name="decision-tree" />
            Mock decision tree
          </label>
          
          <label for="random-forest">
            <input type="checkbox" value="on", id="random-forest" name="random-forest" checked />
            Random forest
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
        <div id="spinner-container">
          <div class="lds-spinner" hidden ><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div><div></div></div>
        </div>
        <div id="error_messages"></div>
        <div id="info"></div>
      </div>
      
      <div id="results-container" hidden>
        <h2>Results</h2>
        <!--
        <p><a href="output/results-male.html">Result page for males</a></p>
        <p><a href="output/results-female.html">Result page for females</a></p>
        <p><a href="output/results-male.pdf">Results for males in pdf</a></p>
        <p><a href="output/results-female.pdf">Result for females in pdf</a></p>
        --> 
        <h3>Summary</h3>
        <div id="table_container"></div>
        <p>Load the summary table in CSV form from <a href="/output/summary.csv">here.</a></p>
        <p id="effect-size">Load the effect size table in CSV form from <a href="/output/effect-size.csv">here.</a></p>
        <p id="variable-importance">Load the variable importance table in CSV form from <a href="/output/variable-importance.csv">here.</a></p>
        <p id="preprocessed">Load the preprocessed data from <a href="/output/preprocessed.rdata">here.</a></p>
        
        <h3>Detailed result pages</h3>
        <table id="detailed-results" class="table table-condensed">
        <tr> <th>Model</th> <th>Gender</th> <th>html</th> <th>pdf</th> </tr>
        <!--
        <tr id="detail-lmm-male"> <td>LMM (male)</td> <td>male</td> <td><a href="output/results-male.html" target="_blank" >html</a></td> <td><a href="output/results-male.pdf" target="_blank" >pdf</a></td> </tr>
        <tr id="detail-lmm-female"> <td>LMM (female)</td> <td>female</td> <td><a href="output/results-female.html" target="_blank" >html</a></td> <td><a href="output/results-female.pdf" target="_blank" >pdf</a></td> </tr>
        <tr id="detail-rf"> <td>Random forest</td> <td>both</td> <td><a href="output/results-random-forest-both.html" target="_blank" >html</a></td> <td><a href="output/results-random-forest-both.pdf" target="_blank" >pdf</a></td> </tr>
        <tr id="detail-dt"> <td>Decision tree</td> <td>both</td> <td><a href="output/results-decision-tree-both.html" target="_blank" >html</a></td> <td><a href="output/results-decision-tree-both.pdf" target="_blank" >pdf</a></td> </tr>
        -->
        </table>
      </div>
      
    </div>  
  
  </div>
  </body>
  </html>
  ', container_version, default_Hb_cutoff_male, default_Hb_cutoff_female, default_max_diff_date_first_donation)

  response
}

#' @assets ../output /output
list()

#' @assets ../static /static
list()
