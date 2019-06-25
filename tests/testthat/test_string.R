context("Test string functions")

test_that("unescape_html() returns correct data.", {

  ## Prepare test data
  strings <- c("S&amp;P500", "isn&apos;t")
  test_data <- unescape_html(strings)

  ## Type check
  expect_is(test_data, "character")
  ## Dimention check
  expect_equal(length(test_data), 2)
  ## Value check
  expect_equal(test_data, c("S&P500", "isn't"))

  ## Error check
  expect_error(unescape_html(1))
  expect_error(unescape_html(character()))
})
