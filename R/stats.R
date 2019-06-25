#' Extract p-value from lm fit object
#'
#' @param fit lm fit object
#'
#' @return numeric scalar
#' @export
lm_pvalue <- function(fit) {
  stopifnot(class(fit) == "lm")

  f <- summary(fit)$fstatistic
  p <- stats::pf(f[1], f[2], f[3], lower.tail = FALSE)
  attributes(p) <- NULL
  p
}
