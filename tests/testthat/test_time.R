context("Test time functions")

test_that("make_duration() returns correct data.", {

  ## Prepare test data
  strings <- "09:30:00"
  test_data <- make_duration(strings)

  ## Type check
  expect_is(test_data, "Duration")
  ## Value check
  expect_equal(test_data, lubridate::dhours(9) + lubridate::dminutes(30))

  ## Error check
  expect_error(make_duration(1))
  expect_error(make_duration("09:3000"))
})

test_that("make_dt_from_msec() returns correct data.", {

  ## Prepare test data
  msec <- c(1506378448618, 1506378448619)
  test_data <- make_dt_from_msec(msec, tz = "America/New_York")

  ## Type check
  expect_is(test_data, "POSIXct")
  ## Value check
  expect_equal(test_data,
               c(lubridate::ymd_hms("2017-09-25 18:27:28.618", tz = "America/New_York"),
                 lubridate::ymd_hms("2017-09-25 18:27:28.619", tz = "America/New_York")))

  ## Error check
  expect_error(make_duration("hoge"))
  expect_error(make_duration(1506378448618, 1))
})
