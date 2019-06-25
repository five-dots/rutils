context("Test plot functions")

test_that("gg_theme() returns correct data.", {

  ## Prepare test data
  test_data <- gg_theme()
  class(test_data)

  ## Type check
  expect_is(test_data, c("theme", "gg"))

  ## Error check
  expect_error(gg_theme("hoge"))
})
