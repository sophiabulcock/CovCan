#
# source('R/betweendaychanges.r')
#
# source('R/daycovidchanges.r')
#
# source('R/timeseriesplot.r')
#
# source('R/currentdatabar.r')
#
# source('R/daily_count.r')




context("day_covid_changes")



library(testthat)

test_that(
  "day_covid_changes",
  {
    expect_true(!is.null(day_covid_changes('2020-05-29','British Columbia', 'Nova Scotia')))
  })



