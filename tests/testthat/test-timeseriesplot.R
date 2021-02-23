

context("time_series_plot")




# +




test_that(
  " test for time sries plot ",
  {
    expect_true(!is.null(time_series_plot('infected','2020-07-01', '2020-08-02','Manitoba')))

  })



# -




