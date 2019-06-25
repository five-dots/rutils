#' Custom ggplot2 theme
#'
#' @param base_size Base font size
#'
#' @return ggplot theme
#' @export
gg_theme <- function(base_size = 11) {
  stopifnot(is.numeric(base_size),
            length(base_size) == 1)

  half.line <- base_size / 2
  ggplot2::theme_grey() + ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1), hjust = 0, vjust = 1, face = "bold.italic",
      margin = ggplot2::margin(b = half.line * 1)),
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(0.8), hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half.line * 0.8)),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
    #legend.position = c(0.95, 0.1),
    #legend.text = ggplot2::element_text(size = 6),
    #legend.title = ggplot2::element_blank(),
    #legend.margin = ggplot2::margin(0, 5, 4, 4)
  )
}
