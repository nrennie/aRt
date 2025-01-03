#' aRt theme
#'
#' Custom ggplot2 theme for aRt objects
#'
#' @noRd

theme_aRt <- function(bg_col, padding = 0) { # nolint
  ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      legend.position = "none",
      plot.margin = ggplot2::unit(
        c(padding, padding, padding, padding), "cm"
      ),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "cm")
    )
}
