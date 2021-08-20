context("preprocess")

source("../../src/sanquin_preprocess.R", chdir=TRUE)

library(tidyverse)

test_that("preprocessing works", {
  expect_equal(2 * 2, 4)
})

test_that("preprocessing works2", {
  # Don't show the output
  output <- 
    capture_output(data <- sanquin_preprocess("generated_example_donations_sanquin.data", "generated_example_donors_sanquin.data",
                                             135, 125, "gperdl", FALSE, 60))
  #load("correct_sanquin_data.rdata")
  correct_data <- readRDS("correct_sanquin_data.rds")
  expect_equal(nrow(data), 5565)
  expect_equal(ncol(data), 21)
  expect_equal(length(unique(data$donor)), 200)
  col_names <- c("don_id", "donor", "Hb", "dateonly", "previous_Hb_def",
                 "days_to_previous_fb", "donat_phleb", "sex", "age", "Hb_deferral",
                 "nb_donat_progesa", "nb_donat_outside",
    "first_event", "previous_Hb", "year", "warm_season", "Hb_first",
    "hour", "consecutive_deferrals", "recent_donations", "recent_deferrals")
  expect_equal(colnames(data), col_names)
  col_types <- c(
      don_id = "factor",
      donor = "factor",
      Hb = "numeric",
      dateonly = "Date",
      previous_Hb_def = "logical",
      days_to_previous_fb = "integer",
      donat_phleb = "factor",
      sex = "factor",
      age = "integer",
      Hb_deferral = "logical",
      nb_donat_progesa = "integer",
      nb_donat_outside = "integer",
      first_event = "logical",
      previous_Hb = "numeric",
      year = "integer",
      warm_season = "numeric",
      Hb_first = "numeric",
      hour = "numeric",
      consecutive_deferrals = "integer",
      recent_donations = "integer",
      recent_deferrals = "integer")
  for (col_name in col_names) {
    expect_equal(class(data[[!!col_name]]), !!col_types[[col_name]]) 
  }
  for (col_name in col_names) {
    #expect_true(all_equal(data[[!!col_name]], correct_data[[!!col_name]], convert=TRUE)) 
#      if (col_name %in% c("warm_season", "previous_Hb_def")) {
#          expect_equal(data[[!!col_name]], as.logical(correct_data[[!!col_name]]))
#      } else if (col_name %in% c("donor", "don_id")) {
#          expect_equal(as.character(data[[!!col_name]]), correct_data[[!!col_name]])
#      } else {
          expect_equal(data[[!!col_name]], correct_data[[!!col_name]])
#      }
  }
})

test_that("Incongruent donors in two files", {
  donors1 <- tribble(
    ~KEY_DONOR, ~KEY_DONOR_SEX, ~KEY_DONOR_DOB, ~DONOR_DATE_FIRST_DONATION,
    "1",        "M",         "20000101",     "20200101"
  )  
  donors2 <- tribble(
    ~KEY_DONOR, ~KEY_DONOR_SEX, ~KEY_DONOR_DOB, ~DONOR_DATE_FIRST_DONATION,
    "2",        "F",         "20000101",     "20200101"
  )  
  donations1 <- tribble(
    ~KEY_DONOR, ~KEY_DONAT_INDEX_DATE, ~DONAT_PHLEB_START, ~DONAT_STATUS, ~KEY_DONAT_PHLEB, ~DONAT_VOL_DRAWN, ~DONAT_RESULT_CODE,
    "1",        "20200101",            "0900",             "-",           "K",              450,              140
  )
  donations2 <- tribble(
    ~KEY_DONOR, ~KEY_DONAT_INDEX_DATE, ~DONAT_PHLEB_START, ~DONAT_STATUS, ~KEY_DONAT_PHLEB, ~DONAT_VOL_DRAWN, ~DONAT_RESULT_CODE,
    "2",        "20200101",            "0900",             "-",           "K",              450,              145
  )
  output <-
    capture_output(
      res <- sanquin_preprocess(donations1, donors1, 135, 125, "gperl", FALSE, 60)
    )
  expect_equal(nrow(res), 1)

  # Extra donation  
  output <-
    capture_output(
      res <- sanquin_preprocess(bind_rows(donations1, donations2), donors1, 135, 125, "gperl", FALSE, 60)
    )
  expect_equal(nrow(res), 1)

  # Extra donor
  output <-
    capture_output(
      res <- sanquin_preprocess(donations1, bind_rows(donors1, donors2), 135, 125, "gperl", FALSE, 60)
    )
  expect_equal(nrow(res), 1)

  # No common donors in the two tables
  expect_error(
    suppressWarnings(
  output <-
    capture_output(
      res <- sanquin_preprocess(donations1, donors2, 135, 125, "gperl", FALSE, 60)
    )
    )
  )
  #expect_equal(nrow(res), 1)
  
}
)
