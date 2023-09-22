#' aRt theme
#'
#' Custom ggplot2 theme for aRt objects
#'
#' @export
#'

theme_aRt <- ggplot2::theme_void() + # nolint
  ggplot2::theme(
    legend.position = "none",
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  )
