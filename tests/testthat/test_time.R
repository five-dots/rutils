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
