#' Abacus
#'
#' This function generates a generative art ggplot
#' object using points and lines.
#'
#' @param nx Number of lines in x direction. Default 30.
#' @param ny Number of points per line. Default 100.
#' @param max_size Maximum size of points. Default 2.
#' @param main_col Vector of colours (or single colour). Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Random seed. Default 123.
#' @return A ggplot object.
#' @export
#'

abacus <- function(nx = 30,
                   ny = 100,
                   max_size = 2,
                   main_col = "black",
                   bg_col = "white",
                   s = 123) {
  set.seed(s)
  plot_data <- tibble::tibble(x = rep(1:nx, ny),
                              y = stats::runif(nx * ny, 0, 10),
                              size = stats::rexp(nx * ny))

  p <- ggplot2::ggplot(data = plot_data,
                       mapping = ggplot2::aes(x = .data$x,
                                              y = .data$y)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$x),
                       alpha = 0.1,
                       colour = main_col,
                       size = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = .data$size),
                        pch = 21,
                        fill = main_col,
                        colour = main_col,
                        alpha = 0.3) +
    ggplot2::scale_size(range = c(0.3, max_size)) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.title = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_blank(),
                   legend.position = "none",
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()
    )
  return(p)
}
