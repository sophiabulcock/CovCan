
# source('R/covid_mapca.r')
# source('R/get_data.r')



context("test for invalid apikeys")

library(testthat)


test_that(
  " test for invalid aipkeys",
  {
    expect_equal(Getdata_syncing('asdf'),"Client error: (400) Bad Request")
    expect_equal(Get_data('asdf') ,"Client error: (400) Bad Request")
    expect_equal(covid_mapca('Ifect'),"please enter either infectedCount or deceasedCount")


  })


