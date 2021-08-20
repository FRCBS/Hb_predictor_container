context("server")

library(httr)       # client
library(websocket)  # client
library(stringr)
library(purrr)

#source("../../src/new_preprocess.R", chdir=TRUE)

url <- "http://localhost:8080/hb-predictor"
url2 <- "http://localhost:8080/hb-predictor2"
websocket_url <- "ws://127.0.0.1:8080/"

runner <- function(form_parameters) {
  errors <- list()
  finished <- FALSE
  r <- GET(url)
  #print(r)
  expect_equal(r$status_code, 200)
  
  # Upload parameters
  res <- POST(url2, body = form_parameters)
  expect_equal(res$status_code, 200)
  
  
  ws <- websocket::WebSocket$new(websocket_url)
  
  ws$onMessage(function(event) {
    cat("Client received message:", event$data, "\n")
    data <- rjson::fromJSON(event$data)
    if (data$type == "final") {
      finished <<- TRUE   # change the variable outside the local environment
    } else if (data$type == "error") {
      errors <<- c(errors, data$error_messages)
    }
  })
  
  ws$onOpen(function(event) {
    cat("Connection opened\n")
    ws$send("start")
  })
  
  
  while (TRUE) {
    if (finished) break
    later::run_now(1L)   # Safely wait for one second
  }
  
  # Close client
  ws$close()
  #print(errors)
  expect_equal(length(errors), 0)
  #cat("here\n")
  #NULL
}

make_parameters <- function(v) {
  v <- sprintf("dv_%s", v)
  names(v) <- v
  map(v, function(x) "on")
}

make_parameters2 <- function(v) {
  #v <- sprintf("dv_%s", v)
  names(v) <- v
  map(v, function(x) "on")
}

test_that("web server is running", {
  form_parameters <- list(input_format = "Sanquin", 
                          donations_file_upload = upload_file("../../generated_example_donations_sanquin.data"),
                          donors_file_upload = upload_file("../../generated_example_donors_sanquin.data"),
                          Hb_cutoff_male = 13.5,
                          Hb_cutoff_female = 12.5,
                          unit = "gperdl",
                          hlen = 7,
                          sample_fraction = 1.0,
                          "stratify-by-sex" = "on",
                          hyperparameters = "finnish",
                          mode = "final"
                          #rf = "on"
                          # dv_previous_Hb = "on",
                          # dv_warm_season = "on",
                          # dv_year = "on",
                          # dv_age = "on"
                          
  )
  variables <- str_split("previous_Hb warm_season year age", " ")[[1]]
  models <- str_split("rf svm", " ")[[1]]
  form_parameters <- c(form_parameters, make_parameters(variables), make_parameters2(models))
  runner(form_parameters)
})
