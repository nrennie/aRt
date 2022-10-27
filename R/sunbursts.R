#' Sunbursts
#'
#' This function generates a generative art ggplot object from 2d density plots
#'
#' @param n Granularity. Default 100.
#' @param x_means Vector of any number of means for the x-coordinate. Default c(0, 10, 5).
#' @param y_means Vector of any number of means for the y-coordinate. Default c(0, 7, 8).
#' @param xy_var Numeric varaince of x and y points. Default 5.
#' @param low Colour of background. Default "#074050".
#' @param high Colour of sunburst points. Default "#d3f2a3".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

sunbursts <- function(n = 100,
                      x_means = c(0, 10, 5),
                      y_means = c(0, 7, 8),
                      xy_var = 5,
                      low = "#074050",
                      high = "#d3f2a3",
                      s = 1234) {
  set.seed(s)
  # generate data
  df <- purrr::map2(.x = x_means,
              .y = y_means,
              .f = ~data.frame(x = rnorm(n, .x, xy_var), y = rnorm(n, .y, xy_var)))
  plot_data <- dplyr::bind_rows(df)
  # plot
  p <- ggplot2::ggplot(data = plot_data,
                  mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::stat_density_2d(mapping = ggplot2::aes(fill = ..density..),
                             geom = "raster", contour = FALSE, na.rm = TRUE) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                limits = c(min(c(plot_data$x, plot_data$y)),
                                           (max(c(plot_data$x, plot_data$y))))) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = c(min(c(plot_data$x, plot_data$y)),
                                           (max(c(plot_data$x, plot_data$y))))) +
    ggplot2::scale_fill_gradient(low = low, high = high) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
                   plot.background = ggplot2::element_rect(fill = low, colour = low),
                   panel.background = ggplot2::element_rect(fill = low, colour = low))
  p
}

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
  p <- lapply(ss, function(i) sunbursts(n = i, x_means = x_means, y_means = y_means, xy_var = xy_var, low = low, high = high, s = i))
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = ncol, nrow = nrow) &
    ggplot2::theme(plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
                   plot.background = ggplot2::element_rect(fill = low, colour = low),
                   panel.background = ggplot2::element_rect(fill = low, colour = low))

}
