context("Change between days")


library(testthat)

test_that(
  "change between days",
  {
expect_true(!is.null(covid_changes_between_days('2020-11-05', '2020-11-07', 'British Columbia', 'Alberta')))
})






