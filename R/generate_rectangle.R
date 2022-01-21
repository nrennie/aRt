#' Generate rectangle
#'
#' This function generates a rectangle which can be plotted with geom_polygon.
#'
#' @param x x-cordinate of bottom left corner
#' @param y y-cordinate of bottom left corner
#' @param height height of rectangle
#' @param width width of rectangle
#' @param group group to identify part of same rectangle. Default 1.
#' @return a data frame
#' @noRd
#'

generate_rectangle <- function(x, y, height, width, group = 1) {
  data.frame(x = c(x, x + width, x + width, x),
             y = c(y, y, y + height, y + height),
             group = rep(group, 4),
             col = as.character(rep(sample(1:13, size = 1, prob = c(rep(0.01, 12), 0.78)), 4)))
}
