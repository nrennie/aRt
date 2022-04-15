#' Sunbursts panel
#'
#' This function generates a grid of sunbursts pieces
#'
#' @param n Granularity. Default 100.
#' @param ncol Number of column panels. Default 4.
#' @param nrow Number of row panels. Default 4.
#' @param x_means Vector of any number of means for the x-coordinate. Default c(0, 10, 5).
#' @param y_means Vector of any number of means for the y-coordinate. Default c(0, 7, 8).
#' @param xy_var Numeric varaince of x and y points. Default 5.
#' @param low Colour of background. Default "#4e0550".
#' @param high Colour of sunburst points. Default "#facdfc".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'

sunbursts_panel <- function(n = 100,
                            ncol = 4,
                            nrow = 4,
                            x_means = c(0, 10, 5),
                            y_means = c(0, 7, 8),
                            xy_var = 5,
                            low = "#4e0550",
                            high = "#facdfc",
                            s = 1234) {
  set.seed(s)
  ss <- sample(5:100, size = ncol * nrow)
  p <- lapply(ss, function(i) sunbursts(n = i,
                                        x_means = x_means,
                                        y_means = y_means,
                                        xy_var = xy_var,
                                        low = low,
                                        high = high,
                                        s = i))
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = ncol, nrow = nrow) &
    ggplot2::theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

}
