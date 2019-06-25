context("Test file functions")

test_that("read_last_file() returns correct data.", {

  ## Prepare test data
  temp_dir <- tempdir()
  csv_path <- paste0(temp_dir, "/test.csv")
  zip_path <- paste0(temp_dir, "/test.zip")
  rds_path <- paste0(temp_dir, "/test.rds")
  tmp_data <- data.frame(x = 1:10, y = 11:20)

  write.csv(tmp_data, csv_path, row.names = FALSE)
  utils::zip(zip_path, csv_path, flags = "-q")
  saveRDS(tmp_data, rds_path)

  test_data <- list(
    any = read_last_file(temp_dir, file_regex = NULL),
    csv = read_last_file(temp_dir, file_regex = ".csv$"),
    zip = read_last_file(temp_dir, file_regex = ".zip$"),
    rds = read_last_file(temp_dir, file_regex = ".rds$")
  )

  ## Run tests for all formats
  for (d in test_data) {
    ## Type check
    expect_is(d, "data.frame")
    ## Dimention check
    expect_equal(nrow(d), 10)
    expect_equal(ncol(d), 2)
  }

  ## Error check
  expect_error(read_last_file("/hoge"))
  expect_error(read_last_file(1))
  expect_error(read_last_file(temp_dir, file_regex = 1))
  expect_error(read_last_file(temp_dir, file_regex = ".hoge"))
})

test_that("get_date_from_file() returns correct data.", {
  ## Type check
  expect_equal(get_date_from_file(c("_2018-12-31.csv",
                                    "_2019-01-01.csv")),
               c(lubridate::ymd("2018-12-31"),
                 lubridate::ymd("2019-01-01")))
  expect_true(is.na(get_date_from_file("hoge_20181231.csv")))

  ## Error check
  expect_error(get_date_from_file(1))
})
