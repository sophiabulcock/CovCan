
# Test function for daily_count function
# Return False if returned value is null


context("daily_count")




test_that(
  "daily_count",
  {
    expect_true(!is.null(daily_count()))
  })


