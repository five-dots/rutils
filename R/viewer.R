#' View object types
#'
#' @param obj Object
#'
#' @return list of character vector
#' @export
view_types <- function(obj) {
  list(
    typeof = typeof(obj),
    mode   = mode(obj),
    class  = class(obj),
    otype  = pryr::otype(obj),
    ftype  = tryCatch(pryr::ftype(obj), condition = function(c) NA),
    sexp   = pryr::sexp_type(obj)
  )
}

#' View object
#'
#' @param obj Object
#'
#' @return list of character vector (except str output)
#' @export
view_data <- function(obj) {
  cat("\n### str() output ###\n\n")
  utils::str(obj)

  cat("\n\n### head() output ###\n\n")
  print(utils::head(obj))

  if (is.data.frame(obj)) {
    if (nrow(obj) > 6) {
      cat("\n\n### tail() output ###\n\n")
      print(utils::tail(obj))
    }
  } else {
    if (length(obj) > 6) {
      cat("\n\n### tail() output ###\n\n")
      print(utils::tail(obj))
    }
  }
}
