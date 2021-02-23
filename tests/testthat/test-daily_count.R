
# Test function for daily_count function
# Return False if returned value is null



context(" daily_count function")



test_that(
  " daily_count function",
  {
    expect_true(!is.null(daily_count()))
  })


