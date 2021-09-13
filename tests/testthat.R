library(testthat)
#library(Hb_predictor_container)

args <- commandArgs(TRUE)

#test_check("Hb_predictor_container")
if (length(args) == 0) {
  test_dir("tests/testthat/")
} else {
  for (file in args) {
    test_file(file)
  }  
}