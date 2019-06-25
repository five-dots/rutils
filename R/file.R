#' Read the last file in the directory by the file's extension
#'
#' @param dir Directory
#' @param file_regex File regex to search
#'
#' @return data.frame
#' @export
read_last_file <- function(dir, file_regex = NULL) {
  stopifnot(is.character(dir),
           (is.character(file_regex) | is.null(file_regex)),
            length(dir) == 1,
            length(file_regex) <= 1,
            ## Stop if no dir
            fs::dir_exists(dir))

  files <- fs::dir_ls(dir, regexp = file_regex, type = "file")
  ## Stop if no file
  stopifnot(length(file) > 0)

  ## Use last file
  file <- utils::tail(files, 1)

  ## Zip
  if (stringr::str_detect(file, ".zip$")) {
    cmd <- glue::glue("unzip -p {file}")
    data.table::fread(cmd = cmd, data.table = FALSE)

  ## CSV
  }  else if (stringr::str_detect(file, ".csv$")) {
    data.table::fread(file, data.table = FALSE)

  ## RDS
  }  else if (stringr::str_detect(file, ".rds$")) {
    readRDS(file)

  } else {
    stop("No applicable method found for the file.")
  }
}

#' Get date from file name
#'
#' @param path Charactoer vector of file paths
#'
#' @return Date vector
#' @export
get_date_from_file <- function(path) {
  stopifnot(is.character(path), length(path) > 0)
  regex <- "[0-9]{4}-[0-9]{2}-[0-9]{2}.[a-z,1-9]+$"
  purrr::map(path, ~ {
    stringr::str_extract(.x, regex) %>%
      stringr::str_split("\\.") %>%
      purrr::pluck(1, 1)
  }) %>%
    unlist() %>%
    lubridate::ymd()
}
