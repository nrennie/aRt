#' Rectangles
#'
#' This function generates a generative art ggplot object featuring multiple coloured rectangles.
#'
#' @param n Number of rectangles. Default 100.
#' @param max_height Maximum height of rectangle. Default 7.
#' @param max_width Maximum width of rectangle. Default 5.
#' @param size Line width of rectangles. Default 2.
#' @param main_col Colour of non-highlighted rectangles. Default "lightgrey".
#' @param col_palette Vector of colours. Default "Bold" colour palette from rcartocolor. Must have 12 colours.
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 123.
#' @return A ggplot object
#' @export

rectangles <- function(n = 100,
                       max_height = 7,
                       max_width = 5,
                       size = 2,
                       main_col = "lightgrey",
                       col_palette = rcartocolor::carto_pal(n = 12, "Bold"),
                       bg_col = "white",
                       s = 123) {
  if (max_height <= 2 || max_width <= 2) {
    stop("height and width should be > 2")
  }
  # generate data
  set.seed(s)
  plot_data <- data.frame(x = c(),
                          y = c(),
                          group = c(),
                          col = c())
  for (i in 1:n) {
    k <- generate_rectangle(x = stats::runif(1, 0, 30),
                     y = stats::runif(1, 0, 30),
                     height = stats::runif(1, 2, max_height),
                     width = stats::runif(1, 2, max_width),
                     group = i)
    plot_data <- rbind(plot_data, k)
  }
  # set colours
  pal <- c(col_palette, main_col)
  names(pal) <- 1:13
  # plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_polygon(ggplot2::aes(group = .data$group,
                                       colour = .data$col),
                                       fill = "transparent",
                                       size = size) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::xlim((min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))) +
    ggplot2::ylim((min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))) +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col))
  p
}
