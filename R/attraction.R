#' Attraction
#'
#' This function generates a generative art ggplot
#' object using strange attractors.
#'
#' @param n Number of points. Default 5000.
#' @param a Parameter. Default -3.
#' @param b Parameter. Default 1.
#' @param c Parameter. Default 0.5.
#' @param d Parameter. Default -1.
#' @param main_col Vector of colours (or single colour). Default `"black"`.
#' @param bg_col Background colour. Default `"white"`.
#' @return A ggplot object.
#' @examples
#' attraction(n = 100)
#' @export

attraction <- function(n = 5000,
                       a = -3,
                       b = 1,
                       c = 0.5,
                       d = -1,
                       main_col = "black",
                       bg_col = "white") {
  x <- numeric(length = n)
  y <- numeric(length = n)
  x[1] <- 0
  y[1] <- 0
  for (i in 2:n) {
    x[i] <- sin(a * y[i - 1]) * sin(b * x[i - 1]) + c * (cos(a * x[i - 1])^2)
    y[i] <- (sin(b * x[i - 1]))^2 + d * cos(b * y[i - 1]) * cos(a * x[i - 1])
  }
  plot_data <- data.frame(t = 1:n, x = x, y = y)
  plot_data
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(colour = t),
      shape = 20,
      alpha = 0.2
    ) +
    ggplot2::scale_colour_gradientn(colours = main_col) +
    theme_aRt(bg_col)
  return(p)
}
