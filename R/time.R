#' Make lubridate duration object from HH:MM:SS string
#'
#' @param hhmmss character scalar of HH:MM:SS format
#'
#' @return lubridate's duration object
#' @export
make_duration <- function(hhmmss) {
  stopifnot(is.character(hhmmss),
            length(hhmmss) == 1,
            ## "HH:MM:SS" format
            stringr::str_count(hhmmss, ":") == 2)

  parts <- stringr::str_split(hhmmss, ":")[[1]] %>% as.numeric()
  lubridate::dhours(parts[1]) +
    lubridate::dminutes(parts[2]) +
    lubridate::dseconds(parts[3])
}

#' Make lubridate duration object from HH:MM:SS string
#'
#' @param msec Elapsed milliseconds from epoch
#' @param tz Timezone
#'
#' @return POSIXct with millisecond accuracy
#' @export
make_dt_from_msec <- function(msec, tz = "America/New_York") {
  stopifnot(is.numeric(msec), length(msec) > 0,
            is.character(tz), length(tz) == 1)
  as.POSIXct(msec/1000, origin = "1970-01-01", tz = tz) + 0.0005
}
