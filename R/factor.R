#' Convert factor to numeric
#'
#' @param x factor vector
#'
#' @return numeric vector
#' @export
as_numeric_factor <- function(x) as.numeric(levels(x))[x]
