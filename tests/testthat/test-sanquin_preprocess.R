context("preprocess")

source("../../src/sanquin_preprocess.R", chdir=TRUE)

test_that("preprocessing works", {
  expect_equal(2 * 2, 4)
})

test_that("preprocessing works2", {
  data <- sanquin_preprocess("generated_example_donations_sanquin.data", "generated_example_donors_sanquin.data",
                             135, 125, "gperdl", 60)
  load("correct_sanquin_data.rdata")
  expect_equal(nrow(data), 5565)
  expect_equal(ncol(data), 21)
  expect_equal(length(unique(data$donor)), 200)
  col_names <- c("don_id", "donor", "Hb", "dateonly", "previous_Hb_def",
                 "days_to_previous_fb", "donat_phleb", "gender", "age", "Hb_deferral",
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
      gender = "factor",
      age = "integer",
      Hb_deferral = "logical",
      nb_donat_progesa = "integer",
      nb_donat_outside = "integer",
      first_event = "logical",
      previous_Hb = "numeric",
      year = "integer",
      warm_season = "logical",
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