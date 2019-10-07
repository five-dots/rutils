context("Test factor functions")

test_that("as_numeric_factor() returns correct data.", {

  months  <- c(1:12)
  factors <- factor(4:6, levels = months)
  tested  <- as_numeric_factor(factors)

  ## Type check
  expect_is(tested, "numeric")

  ## Dimention check
  expect_equal(tested, 4:6)
})
