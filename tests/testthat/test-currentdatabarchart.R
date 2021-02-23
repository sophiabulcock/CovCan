

context("current_data_barchart")



library(testthat)

test_that(
  "current_data_barchart",
  {
    expect_true(!is.null(current_data_barchart('both', 'Canada', 'Alberta','Manitoba')))
  })



# -


