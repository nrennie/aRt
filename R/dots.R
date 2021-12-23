#' Dots
#'
#' This function generates a coloured generative art ggplot object using polar coordinates.
#'
#' @param n_x Number of rotational points. Default 50.
#' @param n_y Number of outwards points. Default 100.
#' @param jitter_size_width Size of jitter width. Default 0.5.
#' @param jitter_size_height Size of jitter height. Default 0.5.
#' @param col_palette Colour palette from rcartocolor. Default "Purp".
#' @param bg_col Background colour. Default "#63589f".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'
#'

dots <- function(n_x = 50, n_y = 100, jitter_size_width = 0.5, jitter_size_height = 0.5, col_palette = "Purp", bg_col = "#63589f", s = 1234) {
  x <- rep(1:n_x, times = n_y)
  y <- rep(1:n_y, each = n_x)
  plot_data <- data.frame(x = x, y = y)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, colour = y)) +
    ggplot2::geom_jitter(size = 0.5, width = jitter_size_width, height = jitter_size_height) +
    rcartocolor::scale_colour_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
    ggplot2::coord_polar() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          plot.title = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_blank(),
          legend.position = "none",
          plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"), # top, right, bottom, left
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())
  p
}
