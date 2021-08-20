context("enrich_deferrals_rf")

# Run tests by executing "Rscript tests/testthat.R" from the top-level "Hb_predictor_container" directory

source("../../src/enrich_deferrals_rf.R", chdir=TRUE)

library(tibble)
library(lubridate)

test_that("trim_time_series", {

  df <- tibble(donor=character(), dateonly=date(), Hb_deferral=logical())
  expect_equal(trim_time_series(df), df)
  
  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), TRUE,
    "a", ymd("2012-01-19"), TRUE
  )
  expect_equal(trim_time_series(df), df)

  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), FALSE,
    "a", ymd("2012-01-19"), FALSE,
    "b", ymd("2012-01-18"), FALSE,
    "b", ymd("2012-01-19"), TRUE,
    "b", ymd("2012-01-20"), FALSE
  )
  res <- trim_time_series(df)
  expect_equal(res, df[1:4,])

  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "b", ymd("2012-01-18"), FALSE,
    "b", ymd("2012-01-19"), TRUE,
    "b", ymd("2012-01-20"), FALSE,
    "a", ymd("2012-01-18"), FALSE,
    "a", ymd("2012-01-19"), FALSE
  )
  res <- trim_time_series(df)
  expect_equal(res %>% arrange(donor, dateonly), df[c(1, 2, 4, 5),] %>% arrange(donor, dateonly))
  
  # Dropped donors with only one donation
  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), FALSE
  )
  res <- trim_time_series(df)
  expect_equal(nrow(res), 0)
  
  # Dropped donors who have their only deferral as the first event
  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), TRUE,
    "a", ymd("2012-01-19"), FALSE
  )
  res <- trim_time_series(df)
  expect_equal(nrow(res), 0)

    # Only deferred donors in data
  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), FALSE,
    "a", ymd("2012-01-19"), TRUE
  )
  expect_equal(trim_time_series(df), df)

  # Only non-deferred donors in data
  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), FALSE,
    "a", ymd("2012-01-19"), FALSE
  )
  expect_equal(trim_time_series(df), df)

})

test_that("get_deferrals", {
  df <- tibble(donor=character(), dateonly=date(), Hb_deferral=logical())
  res <- expect_warning(get_deferrals(df), "no non-missing arguments to max; returning -Inf")
  expect_equal(nrow(res$sometime_deferred), 0)
  expect_equal(nrow(res$never_deferred), 0)
})

test_that("balance_classes", {
  df <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), TRUE,
    "b", ymd("2012-01-19"), FALSE
  )
  #df <- df %>% mutate(donor = as.factor(donor))
  
  res <- balance_classes(df, 0.5)
  expect_equal(res, df %>% mutate(donor = as.factor(donor)))

  res <- balance_classes(df, 1.0)
  expect_equal(res, df[1,] %>% mutate(donor = as.factor(donor)))

  res <- balance_classes(df, 0.0)
  expect_equal(res, df[2,] %>% mutate(donor = as.factor(donor)))
  
  df2 <- tribble(
    ~donor, ~dateonly, ~Hb_deferral,
    "a", ymd("2012-01-18"), TRUE,
    "b", ymd("2012-01-18"), TRUE,
    "c", ymd("2012-01-18"), TRUE,
    "d", ymd("2012-01-18"), FALSE
  )
  res <- balance_classes(df2, 0.5)
  expect_equal(nrow(res), 2)
})
