#library(Rook)  
#library(data.table, quietly=TRUE)

setwd("src")
source("new_preprocess.R")
# plumber.R



get_info <- function(x) {
  v = c("<pre>",
    paste("Filename:", x$filename),
    paste("Tempfile:", x$tempfile),
    paste("Content type:", x$content_type),
    paste("Content length:", as.character(file.info(x$tempfile)$size)),
    "</pre>")
  return(paste(v, collapse="\n"))
}

#' Make html page
#aaa #' @param firstname If provided, filter the data to only this species (e.g. 'setosa')
#' @get /hb-predictor2
#' @post /hb-predictor2
#' @json
function(req, fileUpload){
  #cat("At the start\n")
  cat("Before multipart$parse")
  post = Rook::Multipart$parse(req)
  cat("After multipart$parse")
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
    if (is.na(sf) || sf < 0.0 || sf > 1.0)
      error_messages <- c(error_messages, "The sample fraction must be a real number between 0 and 1")
  }
  
  if (length(error_messages) > 0)
    return(list(error_messages=error_messages))
  
  if ("donations_fileUpload" %in% names(post) &&
      "donors_fileUpload"    %in% names(post)){
    donations_o = post$donations_fileUpload
    donors_o = post$donors_fileUpload
    
    #l=list()
    time_start <- now()
    
    str(donors_o, nchar.max = 10000)
    upload_info <- c(get_info(donations_o), get_info(donors_o))
    donations <- read_delim(donations_o$tempfile, col_names=FALSE, delim='|')
    s1 <- sprintf("<p>Donations: filename=%s, rows=%i, columns=%i</p>", donations_o$filename, nrow(donations), ncol(donations))
    donors <- read_delim(donors_o$tempfile, col_names=FALSE, delim='|')
    s2 <- sprintf("<p>Donor: filename=%s, rows=%i, columns=%i</p>", donors_o$filename, nrow(donors), ncol(donors))
    
    myparams <- list()
    myparams$Hb_cutoff_male <- ifelse ("Hb_cutoff_male" %in% names(post), as.integer(post$Hb_cutoff_male), 135)
    myparams$Hb_cutoff_female <- ifelse ("Hb_cutoff_female" %in% names(post), as.integer(post$Hb_cutoff_female), 125)
    
    if (is.na(myparams$Hb_cutoff_male) || myparams$Hb_cutoff_male < 0) {
      return("Incorrect value for Hb_cutoff_male\n")
    }
    myparams$skip_train <- FALSE
    myparams$create_datasets_bool <- TRUE
#    myparams$skip_train <- TRUE
#    myparams$create_datasets_bool <- FALSE
    fulldata_preprocessed <- preprocess(donations_o$tempfile, donors_o$tempfile,
                                        myparams$Hb_cutoff_male, myparams$Hb_cutoff_female)
    s3 <- sprintf("<p>Preprocessed data: rows=%i, columns=%i</p>", nrow(fulldata_preprocessed), ncol(fulldata_preprocessed))
    #data_filename <- "/tmp/temp.rdata"
    data_filename <- tempfile(pattern = "preprocessed_data_", fileext = ".rdata")
    save(fulldata_preprocessed, file=data_filename)
    myparams$input_file <- data_filename
    if ("sample_fraction" %in% names(post)) {
      myparams$sample_fraction <- as.numeric(post$sample_fraction)
    }
    if ("hlen" %in% names(post)) {
      myparams$hlen <- as.integer(post$hlen)
    }
    if ("method" %in% names(post)) {
      myparams$method <- post$method
    }
    
    myparams_string <- paste( map_chr(names(myparams), function (name) sprintf("<li>%s=%s</li>", name, myparams[name])), 
                             collapse="\n")
    myparams_string <- sprintf("<p>Parameters are:</p>\n <ul>%s\n</ul>\n", myparams_string)
    
    error_dfs <- list()
    for (gender in c("male", "female")) {
    #for (gender in c("male")) {
      myparams["gender"] <- gender
      filename <- sprintf("/tmp/errors-%s.csv", gender)
      myparams["errors_table_file"] <- filename
      rmarkdown::render(#'jarkko_new_rmd.Rmd',
                        'jarkko_subset_analyses.Rmd',
                        output_file=rep(sprintf('results-%s', gender), 2),   # One for each output format: html and pdf 
                        #output_file=sprintf('results-%s', gender),   # One for each output format: html and pdf 
                        output_format=c('html_document', 'pdf_document'),
                        #output_format=c('html_document'),
                        clean=FALSE,
                        output_dir='../output',
                        params = myparams)
      error_dfs[[gender]] <- read_csv(filename)
    }
    unlink(data_filename)
  
    errors_both <- rbind(error_dfs[["male"]], error_dfs[["female"]])
    #errors_both <- rbind(error_dfs[["male"]])
    errors_string <- kable(errors_both, format="html", digits=3, 
                           caption="Error and performance measures: mean absolute error (MAE), root mean squared error (RMSE), and area under ROC curve (AUC).",
                           align="llllll",
                             table.attr = "id='errors_table' class='table table-condensed'")

    time_end <- now()
    time_start_s <- sprintf("<p>Computation started: %s</p>\n", as.character(time_start))
    time_end_s <- sprintf("<p>Computation ended: %s</p>\n", as.character(time_end))
    total_time <- sprintf("<p>Computation took %.2f seconds</p>\n", as.double(as.duration(time_end - time_start)))

    
    iframe_male <- sprintf("<iframe src='output/results-male.html' width='800' height='800'></iframe>")
    iframe_female <- sprintf("<iframe src='output/results-female.html' width='800' height='800'></iframe>")

    #result2 <- c(upload_info, s1, s2, s3, myparams_string, time_start_s, time_end_s, total_time)
    result2 <- c(s1, s2, s3)
    result2 <- paste(result2, collapse="\n")

  
    return(list(result=result2, errors=as.character(errors_string)))
  } else {
    return("Error: no uploaded files!")
  }
}

#' Make html page
#' @get /hb-predictor
#' @html
function(req){
  response = '
  <html>
  
  <head>
  <link rel="stylesheet" href="static/bootstrap.min.css">
  <link rel="stylesheet" href="static/style.css">
  <script type="text/javascript" src="static/script.js"></script>
  </head>
  
  <body>
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
        
        <table id="input_table">
        <tr><td>Upload donations file:</td> <td><input type=file name="donations_fileUpload"></td> </tr>
        <tr><td>Upload donors file:</td>    <td><input type=file name="donors_fileUpload"></td> </tr>
        <tr><td>Hb cutoff (male)</td>       <td><input name="Hb_cutoff_male" value="135" maxlength="5" size="5"><span id="male_unit">g/L</span></td> </tr>
        <tr><td>Hb cutoff (female)</td>     <td><input name="Hb_cutoff_female" value="125" maxlength="5" size="5"><span id="female_unit">g/L</span></td> </tr>
        <tr><td>Minimum donations</td>      <td><input name="hlen" value="1" pattern="^[0-9]+$" maxlength="5" size="5"></td> </tr>
        <tr><td>Sample fraction</td>        <td><input name="sample_fraction" value="1.00" maxlength="5" size="5"></td> </tr>
        <!--<tr><td>Progress</td>               <td><progress id="progress" value="0" /></td></tr>-->
        </table>
      
        <fieldset>
          <legend>Which prediction model to use?</legend>
          <label for="no-fix">
            <input type="radio" value="no-fix", id="no-fix" name="method"/>
            Linear mixed model
          </label>
          <label for="icp-fix">
            <input type="radio" value="icp-fix", id="icp-fix" name="method"/>
            Dynamic linear mixed model
          </label>
          <label for="both">
            <input type="radio" value="both", id="both" name="method" checked/>
            Both
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

        <h3>Detailed result pages</h3>
        <table id="detailed-results" class="table table-condensed">
        <tr> <th>Gender</th> <th>html</th> <th>pdf</th> </tr>
        <tr> <td>Male</td> <td><a href="output/results-male.html" target="_blank" >html</a></td> <td><a href="output/results-male.pdf" target="_blank" >pdf</a></td> </tr>
        <tr> <td>Female</td> <td><a href="output/results-female.html" target="_blank" >html</a></td> <td><a href="output/results-female.pdf" target="_blank" >pdf</a></td> </tr>
        </table>
      </div>
      
    </div>  
  
  </div>
  </body>
  </html>
  '

  response
}

#' @assets ../output /output
list()

#' @assets ../static /static
list()