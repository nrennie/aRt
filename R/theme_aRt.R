#' aRt theme
#'
#' Custom ggplot2 theme for aRt objects
#'
#' @noRd

theme_aRt <- function(bg_col, padding = 0) {
  ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      legend.position = "none",
      plot.margin = ggplot2::unit(
        c(padding, padding, padding, padding), "cm"
      )
    )
}
