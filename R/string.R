#' Unescape HTML strings
#'
#' @param strings character vector
#'
#' @return character vector
#' @export
unescape_html <- function(strings) {
  stopifnot(is.character(strings), length(strings) > 0)

  ## https://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
  purrr::map_chr(strings, ~ {
    xml2::xml_text(xml2::read_html(glue::glue("<x>{.x}</x>")))
  })
}

exec_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file <- stringr::str_subset(args, "^--file=.*.R$") %>%
    stringr::str_remove("^--file=")

  if (length(file) == 0) return(NULL)
  file
}

#' Get a string for logging
#'
#' @param message character scalar
#'
#' @return character scalar
#' @export
log_str <- function(message) {
  stopifnot(is.character(message), length(message) == 1)
  date_time <- format(Sys.time(), "%Y/%m/%d (%a) %H:%M:%S")

  message <- stringr::str_c(message, "\n")
  stringr::str_c(date_time, exec_file(), message, sep = " ")
}
