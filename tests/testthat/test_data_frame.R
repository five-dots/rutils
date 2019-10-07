context("Test data.frame functions")

test_that("remove_outliers() returns correct data.", {
  test_data <- data.frame(x = 1:100)
  filtered <- remove_outliers(test_data, x, 0.01)

  ## Type check
  expect_is(filtered, "data.frame")
  expect_is(filtered$x, "integer")

  ## Dimention check
  expect_equal(nrow(filtered), 98)

  ## Error check
  expect_error(remove_outliers(NULL, x, 0.01))
  expect_error(remove_outliers(data.frame(), x, 0.01))
  expect_error(remove_outliers(data, x, 0.51))
  expect_error(remove_outliers(data, x, -0.01))
})
