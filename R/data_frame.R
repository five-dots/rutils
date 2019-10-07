#' Remove outliers from data.frame
#'
#' @param data data.frame
#' @param x column name to filter
#' @param outlier_percent Upper and lower bound percent to remove
#'
#' @return data.frame
#' @export
remove_outliers <- function(data, x, outlier_percent = NULL) {
  stopifnot(is.data.frame(data), nrow(data) > 0)
  stopifnot(0 < outlier_percent, outlier_percent < 0.5)

  x <- dplyr::enquo(x)
  data %>%
    dplyr::mutate(rank = dplyr::percent_rank(!!x)) %>%
    dplyr::filter(.data$rank >= outlier_percent &
                  .data$rank <= (1 - outlier_percent)) %>%
    dplyr::select(-rank)
}

