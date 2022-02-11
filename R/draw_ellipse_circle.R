#' Draw Ellipse Circle
#'
#' This function generates data for an ellipse or circle.
#'
#' @param x0 x-cordinate of circle centre. Default 0.
#' @param y0 y-cordinate of circle centre. Default 0.
#' @param r vertical radius of circle. Default 5.
#' @param a ratio of horizontal to vertical radii.
#' @param n number of points to generate. Default 1000.
#' @param group group to identify part of same circle. Default 1.
#' @return a tibble
#' @noRd
#'

draw_ellipse_circle <- function(x0 = 0,
                                y0 = 0,
                                r = 5, a = 1,
                                n = 1000,
                                group = 1) {
  theta <- seq(0, 2 * pi, length.out = n)
  tibble::tibble(x = a * r * cos(theta) + x0,
                 y = r * sin(theta) + y0,
                 group = rep(group, n))
}
