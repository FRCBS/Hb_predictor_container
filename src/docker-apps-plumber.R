#library(Rook)  
#library(data.table, quietly=TRUE)

# Start in src directory with
# Rscript docker-server-plumber.R

container_version="0.16"

message(paste0("Working directory is ", getwd(), "\n"))
#setwd("src")
source("new_preprocess.R")
source("sanquin_preprocess.R")
# plumber.R

library(readr)
library(rjson)

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
function(req, parametrit){
  cat(sprintf("Container version is %s\n", container_version))
  if (Sys.getenv("TZ") == "") {
    Sys.setenv("TZ"="Europe/Helsinki")
  }
  #cat("At the start\n")
  cat("Before multipart$parse\n")
  tic("Parsing form data")
  #saveRDS(req, file="~/test_multipart_form_data/request.rds")
  
  #post = Rook::Multipart$parse(req)
  
  # Parse uploaded files.
  #separator <- req$postBody[[1]]
  #readr::write_lines(req$postBody, "/tmp/poista.bin", sep="\r\n")
  separator <- ""
  writeBin(req$bodyRaw, con="/tmp/poista.bin")
  command <- sprintf("./parse /tmp/poista.bin \"%s\" /tmp/poista.json", separator)
  system(command)
  post <- rjson::fromJSON(file="/tmp/poista.json")
  unlink("/tmp/poista.bin")
  unlink("/tmp/poista.json")
  toc()
  
  #saveRDS(post, file="~/test_multipart_form_data/post.rds")
  cat("After multipart$parse\n")
  str(post, nchar.max = 10000)


  
  error_messages=c()
  if (!("donations_fileUpload" %in% names(post)))
    error_messages <- c(error_messages, "You did not upload the donations file!")
  if (!("donors_fileUpload" %in% names(post)))
    error_messages <- c(error_messages, "You did not upload the donors file!")
  if ("Hb_cutoff_male" %in% names(post) && as.integer(post$Hb_cutoff_male) <= 0)
    error_messages <- c(error_messages, "The Hb cutoff must be a positive integer")
  if ("Hb_cutoff_female" %in% names(post) && as.integer(post$Hb_cutoff_female) <= 0)
    error_messages <- c(error_messages, "The Hb cutoff must be a positive integer")
  if ("sample_fraction" %in% names(post)) {
    sf <- as.numeric(post$sample_fraction)
    if (is.na(sf) || sf < 0.0 || (sf > 1.0 && !is.wholenumber(sf)))
      error_messages <- c(error_messages, "The sample fraction must be a real number between 0 and 1, or an integer larger than 1")
  }
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
    donation_info <- sprintf("<p>Donations: filename=%s, rows=%i, columns=%i</p>", donations_o$filename, nrow(donations), ncol(donations))
    donor_info <- sprintf("<p>Donor: filename=%s, rows=%i, columns=%i</p>", donors_o$filename, nrow(donors), ncol(donors))
  } else {
    donation_info <- ""
    donor_info <- ""
  }
  
  # Create the input parameter list for the Rmd files that do the actual prediction    
  myparams <- list()
  myparams$Hb_cutoff_male   <- ifelse ("Hb_cutoff_male"   %in% names(post), as.integer(post$Hb_cutoff_male),   135)
  myparams$Hb_cutoff_female <- ifelse ("Hb_cutoff_female" %in% names(post), as.integer(post$Hb_cutoff_female), 125)
  
  # if (is.na(myparams$Hb_cutoff_male) || myparams$Hb_cutoff_male < 0) {
  #   return("Incorrect value for Hb_cutoff_male\n")
  # }
  myparams$skip_train <- FALSE
  myparams$create_datasets_bool <- TRUE
  #    myparams$skip_train <- TRUE
  #    myparams$create_datasets_bool <- FALSE
  
  if (input_format == "Preprocessed") {
    data_filename <- post$preprocessed_fileUpload$tempfile
    fulldata_preprocessed <- load_single(data_filename)
    preprocessed_info <- sprintf("<p>Preprocessed data: rows=%i, columns=%i</p>", nrow(fulldata_preprocessed), ncol(fulldata_preprocessed))
    rm(fulldata_preprocessed)
  } else {
    # Do the preprocessing
    tic("Preprocessing data")
    if (input_format == "FRCBS") {
      if (sf != 1.0)
        sample_raw_progesa(donations_o$tempfile, donors_o$tempfile, donations_o$tempfile, donors_o$tempfile, ndonor=sf)
      fulldata_preprocessed <- preprocess(donations_o$tempfile, donors_o$tempfile,
                                          myparams$Hb_cutoff_male, myparams$Hb_cutoff_female)
    } else {  # Sanquin
      res <- sanquin_sample_raw_progesa(donations_o$tempfile, donors_o$tempfile, donations_o$tempfile, donors_o$tempfile, ndonor=sf)
      fulldata_preprocessed <- sanquin_preprocess(donations_o$tempfile, donors_o$tempfile,
                                                  myparams$Hb_cutoff_male, myparams$Hb_cutoff_female)
      if (all(c("FERRITIN_FIRST", "FERRITIN_LAST", "FERRITIN_LAST_DATE") %in% names(res$donor))) {
        cat("hep\n")
        donor_specific <- res$donor %>% select(donor = KEY_DONOR, FERRITIN_FIRST, FERRITIN_LAST, FERRITIN_LAST_DATE)
        cat("hep2\n")
        old_count <- nrow(donor_specific)
        donor_specific <- donor_specific %>% 
          filter(!is.na(FERRITIN_FIRST), !is.na(FERRITIN_LAST), !is.na(FERRITIN_LAST_DATE)) %>%
          mutate(FERRITIN_LAST_DATE=lubridate::as_date(FERRITIN_LAST_DATE))
        cat(sprintf("Dropped %i / %i donors due to FERRITIN_FIRST/LAST/LAST_DATE being NA\n", 
                    old_count - nrow(donor_specific), old_count))
        
        # Select only donors whose last ferritin is not from the last donation
        last_donations <- fulldata_preprocessed %>% group_by(donor) %>% slice_max(order_by=dateonly) %>% ungroup() %>% select(donor, dateonly)
        old_count <- nrow(donor_specific)
        donor_specific <- donor_specific %>% anti_join(last_donations, by=c("donor"="donor", "FERRITIN_LAST_DATE"="dateonly")) # %>% select(-FERRITIN_LAST_DATE)
        cat(sprintf("Dropped %i / %i donors due to FERRITIN_LAST_DATE being equal to last donation date\n", 
                    old_count - nrow(donor_specific), old_count))
        
        old_count <- nrow(donor_specific)
        donor_specific <- donor_specific %>% semi_join(fulldata_preprocessed, by="donor") # make sure these were not preprocessed away
        cat(sprintf("Dropped %i / %i donors due to joining with preprocessed data\n", 
                    old_count - nrow(donor_specific), old_count))
        
        donor_specific_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
        save(donor_specific, file = donor_specific_filename)
        cat(sprintf("Saved donor specific variables (%ix%i) to file %s\n", nrow(donor_specific), ncol(donor_specific), donor_specific_filename))
      } else {
        cat("No ferritin information found.\n")
      }
    }
    post$sample_fraction <- 1.0   # Do not repeat the sampling in the Rmd files
    preprocessed_info <- sprintf("<p>Preprocessed data: rows=%i, columns=%i</p>", nrow(fulldata_preprocessed), ncol(fulldata_preprocessed))
    #data_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
    data_filename <- "../output/preprocessed.rdata"
    save(fulldata_preprocessed, file=data_filename)
    toc()
    message(sprintf("Saved preprocessed data to file %s\n", data_filename))
  }
  
  myparams$input_file <- data_filename
  if ("sample_fraction" %in% names(post))
    myparams$sample_fraction <- as.numeric(post$sample_fraction)
  if ("hlen" %in% names(post))
    myparams$hlen <- as.integer(post$hlen)
  if (!is.null(donor_specific_filename))
    myparams$donor_specific_file <- donor_specific_filename
  
  myparams_string <- paste( map_chr(names(myparams), function (name) sprintf("<li>%s=%s</li>", name, myparams[name])), 
                            collapse="\n")
  myparams_string <- sprintf("<p>Parameters are:</p>\n <ul>%s\n</ul>\n", myparams_string)
  
  summary_tables <- list()
  effect_size_tables <- list()
  
  # How much memory is available/used
  system("free -h")
  
  # How many cores are available
  cat(sprintf("Number of available cores: %i\n", parallel::detectCores()))
  
  # Run linear models
  methods <- intersect(c("no-fix", "icp-fix"), names(post))
  if (length(methods) > 0) {
    myparams$method <- ifelse (length(methods) == 2, "both", methods[[1]])
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
    }
  }
  
  # Run random forest etc
  methods <- intersect(c("decision-tree", "random-forest"), names(post))
  if (length(methods) > 0) {
    myparams$method <- ifelse (length(methods) == 2, "both", methods[[1]])
    gender <- "both"
    myparams["gender"] <- gender
    filename <- sprintf("/tmp/summary-%s.csv", "ml")
    myparams["summary_table_file"] <- filename
    effect_size_filename <- sprintf("../output/variable-importance.csv")
    myparams["effect_size_table_file"] <- effect_size_filename
    error_messages <- tryCatch(
      error = function(cnd) {
        error_messages <- c("Error in random forest call.\n", cnd$message)
        return(error_messages)
      },
      {
        rmarkdown::render(
          'random_forest.Rmd',
          #'template.Rmd',
          output_file=rep(sprintf('results-%s', gender), 2),   # One for each output format: html and pdf 
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
    summary_tables[["ml"]] <- read_csv(filename)
  }
  #unlink(data_filename)
  if (!is.null(donor_specific_filename))
    unlink(donor_specific_filename)
  
  summary_table <- bind_rows(summary_tables)
  effect_size_table <- bind_rows(effect_size_tables)
  write_csv(summary_table, "../output/summary.csv")
  write_csv(effect_size_table, "../output/effect-size.csv")
  cols <- c("Model", "MAE (g / L)", "RMSE (g / L)", "MAE (mmol / L)", "RMSE (mmol / L)", "AUROC" = "AUROC value", "AUPR" = "AUPR value", "F1" = "F1 value")
  summary_table_string <- kable(summary_table %>% select(!!!cols), format="html", digits=3, 
                         caption="Error and performance measures: mean absolute error (MAE), root mean squared error (RMSE),\
                           area under ROC curve (AUROC), and area under precision-recall curve (AUPR).",
                         align="llllll",
                         table.attr = "id='errors_table' class='table table-condensed'")
  


  
  #result2 <- c(upload_info, s1, s2, s3, myparams_string, time_start_s, time_end_s, total_time)
  result2 <- c(donation_info, donor_info, preprocessed_info)
  result2 <- paste(result2, collapse="\n")
  
  
  return(list(result=result2, summary_table=as.character(summary_table_string)))
  
}



#' Show the html page
#' @get /hb-predictor
#' @serializer html
function(req){
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
        <tr><td>Hb cutoff (male)</td>       <td><input name="Hb_cutoff_male" value="135" maxlength="5" size="5"><span id="male_unit">g/L</span></td> </tr>
        <tr><td>Hb cutoff (female)</td>     <td><input name="Hb_cutoff_female" value="125" maxlength="5" size="5"><span id="female_unit">g/L</span></td> </tr>
        <tr><td>Minimum donations</td>      <td><input name="hlen" value="7" pattern="^[0-9]+$" maxlength="5" size="5"></td> </tr>
        <tr><td>Sample fraction/size</td>        <td><input name="sample_fraction" value="1.00" maxlength="5" size="5"></td> </tr>
        <!--<tr><td>Progress</td>               <td><progress id="progress" value="0" /></td></tr>-->
        </table>
      
        <fieldset>
          <legend>Which prediction model to use?</legend>
          <label for="no-fix">
            <input type="checkbox" value="on", id="no-fix" name="no-fix" />
            Linear mixed model
          </label>
          <label for="icp-fix">
            <input type="checkbox" value="on", id="icp-fix" name="icp-fix" />
            Dynamic linear mixed model
          </label>
          <!--
          <label for="decision-tree">
            <input type="checkbox" value="on", id="decision-tree" name="decision-tree" checked/>
            Decision tree
          </label>
          -->
          <label for="random-forest">
            <input type="checkbox" value="on", id="random-forest" name="random-forest" checked/>
            Random forest
          </label>
        </fieldset>
    
        <input id="submit" type="submit" value="Upload the files and start computing" name="submit_button">
      </div>
      

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
        <tr> <th>Model</th> <th>html</th> <th>pdf</th> </tr>
        <tr id="detail-lmm-male"> <td>LMM (male)</td> <td><a href="output/results-male.html" target="_blank" >html</a></td> <td><a href="output/results-male.pdf" target="_blank" >pdf</a></td> </tr>
        <tr id="detail-lmm-female"> <td>LMM (female)</td> <td><a href="output/results-female.html" target="_blank" >html</a></td> <td><a href="output/results-female.pdf" target="_blank" >pdf</a></td> </tr>
        <tr id="detail-rf"> <td>Random forest</td> <td><a href="output/results-both.html" target="_blank" >html</a></td> <td><a href="output/results-both.pdf" target="_blank" >pdf</a></td> </tr>
        </table>
      </div>
      
    </div>  
  
  </div>
  </body>
  </html>
  ', container_version)

  response
}

#' @assets ../output /output
list()

#' @assets ../static /static
list()
