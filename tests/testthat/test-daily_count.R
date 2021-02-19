
# Test function for daily_count function
# Return False if returned value is null



context(" daily_count function")
# source('R/betweendaychanges.r')
#
# source('R/daycovidchanges.r')
#
# source('R/timeseriesplot.r')
#
# source('R/currentdatabar.r')

# source('R/daily_count.r')



test_that(
  " daily_count function",
  {
    expect_true(!is.null(daily_count()))
  })


