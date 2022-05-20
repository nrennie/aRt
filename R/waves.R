#' Waves
#'
#' This function generates a generative art ggplot object
#' from sine and cosine waves.
#'
#' @param a sine wave parameter. Default 23.
#' @param b Cosine wave parameter. Default 6.
#' @param main_col Vector of colours (or single colour) for lines. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 2021.
#' @return A ggplot object.
#' @export

waves <- function(a = 23,
                  b = 6,
                  main_col = "black",
                  bg_col = "white",
                  s = 2021) {
  set.seed(s)
  x <- seq(0, 50 * pi, 0.01)
  y <- sample(1:8, size = 1) * sin(a * x) +
    sample(1:8, size = 1) * cos(b * x)
  df <- data.frame(x = x, y = y)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_path(ggplot2::aes(colour = .data$y)) +
    ggplot2::scale_colour_gradientn(colours = rev(main_col)) +
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
          panel.grid.minor = ggplot2::element_blank()
    )
  p
}
