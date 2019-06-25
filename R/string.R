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
