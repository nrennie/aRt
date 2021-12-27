#' Shell
#'
#' This function generates a layered generative art ggplot object sine and cosine waves and lines.
#'
#' @param n Number of spirals. Default 4.
#' @param alpha Transparency of lines. Default 1.
#' @param main_col Colour scheme of art. Default "black".
#' @param bg_col Background colour. Default "white".
#' @return A ggplot object.
#' @export


shell <- function(n = 4, alpha = 1, main_col = "black", bg_col = "white") {
  theta <- seq(0, (n + 0.5) * pi, 0.01)
  r <- 0.5 + 0.5 * theta
  df <- data.frame(x = r * cos(theta), y = r * sin(theta))
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = .data$x, y = .data$y),
                       colour = ggplot2::alpha(main_col, alpha)) +
    ggplot2::coord_fixed(xlim = c(- (max(abs(df))), (max(abs(df)) + 1)),
                         ylim = c(- (max(abs(df))), (max(abs(df)) + 1))) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col))
  p
}
