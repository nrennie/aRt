#' Draw Ellipse in Circle
#'
#' This function generates data for a circle filled with ellipses of the same vertical radius
#'
#' @param x0 x-cordinate of circle centre. Default 0.
#' @param y0 y-cordinate of circle centre. Default 0.
#' @param r vertical radius of circle. Default 5.
#' @param n number of points to generate. Default 1000.
#' @return a tibble
#' @noRd
#'

draw_ellipse_in_circle <- function(x0 = 0,
                                   y0 = 0,
                                   r = 5,
                                   n = 1000) {
  plot_data <- data.frame(x = c(), y = c(), group = c())
  a <- seq(0, 1, 0.05)
  for (i in seq_len(length(a))) {
    k <- draw_ellipse_circle(x0 = x0, y0 = y0, r = r, a = a[i], n = n, group = i)
    plot_data <- rbind(plot_data, k)
  }
  plot_data
}
