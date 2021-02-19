# ---
# jupyter:
#   jupytext:
#     text_representation:
# source('R/betweendaychanges.r')
#
# source('R/daycovidchanges.r')

# source('R/timeseriesplot.r')

# source('R/currentdatabar.r')
#
# source('R/daily_count.r')

context("time_series_plot")




# +




test_that(
  " test for time sries plot ",
  {
    expect_true(!is.null(time_series_plot('infected','2020-07-01', '2020-08-02','Manitoba')))

  })



# -




