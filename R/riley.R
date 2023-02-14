#' Riley
#'
#' This function generates a coloured generative art ggplot object from intersecting lines.
#' Inspired by Balm by Bridget Riley.
#'
#' @param n_x Number of circles in x-direction. Default 9.
#' @param n_y Number of circles in y-direction. Default 9.
#' @param offset Numeric giving offset of y-axis. Default 3.
#' @param main_col Main colour. Default "black".
#' @param bg_col Background colour. Default "white".
#' @return A ggplot object.
#' @export

riley <- function(n_x = 9,
                  n_y = 9,
                  offset = 3,
                  main_col = "black",
                  bg_col = "white") {
  x <- -n_x:n_x
  y <- -n_y:n_y + offset
  df <- expand.grid(x = x, y = y)
  plot_data <- df |>
    dplyr::mutate(y = .data$y + c(rep(c(0.5, 0), times = n_y), 0.5)) |>
    dplyr::filter(y != min(.data$y))
  p <- ggplot2::ggplot(plot_data) +
    ggforce::geom_ellipse(mapping = ggplot2::aes(
      x0 = .data$x,
      y0 = .data$y,
      b = 0.35 / (((abs(.data$y) + 1))^(1 / 3)),
      a = 0.35,
      alpha = abs(.data$y),
      angle = 0),
      fill = main_col,
      colour = "transparent") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col))
  cowplot::ggdraw() +
    cowplot::draw_plot(p) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col))
}
