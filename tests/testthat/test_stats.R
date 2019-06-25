context("Test stats functions")

test_that("lm_pvalue() returns correct data.", {

  ## Prepare test data
  set.seed(123)
  data <- data.frame(x = rnorm(100), y = rnorm(100))
  fit_lm <- lm(y ~ x, data)
  pvalue <- lm_pvalue(fit_lm)

  ## Value check
  expect_equal(pvalue, 0.6245623, tolerance=1e-7)

  ## Error check
  expect_error(lm_pvalue(10))
})
