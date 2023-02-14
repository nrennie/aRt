#' Generate black hole points
#'
#' @param r_maxi Radius of i^{th} internal circle
#' @param n Number of points per circle
#' @param lim Numeric specifying size of grid
#' @noRd

black_hole_points <- function(r_maxi, n, lim) {
  r <- 1 / sqrt(stats::runif(n)) * r_maxi
  theta <- stats::runif(n, 0, 2 * pi)
  plot_data <- tibble::tibble(x = cos(theta) * r,
                              y = sin(theta) * r)  |>
    dplyr::filter(.data$x <= lim,
                  .data$x >= -1 * lim,
                  .data$y <= lim,
                  .data$y >= -1 * lim) |>
    dplyr::mutate(dists = sqrt((.data$x^2 + .data$y^2)) - r_maxi)
  return(plot_data)
}

#' Black Hole
#'
#' This function generates a generative art ggplot
#' object using points.
#'
#' @param r_max Vector of radii for the internal circle. Default c(50, 150, 250, 350).
#' @param n Number of points per circle. Default 10000.
#' @param lim Numeric specifying size of grid. Default 400.
#' @param main_cols Vector of colours (or single colour). Default rcartocolor::carto_pal(n = 7, name = "SunsetDark").
#' @param bg_col Background colour. Default "black".
#' @param size Size of points. Default 0.01.
#' @param a Transparency of points. Default 0.5.
#' @param s Random seed. Default 1234.
#' @return A ggplot object.
#' @export
#'

black_hole <- function(r_max = c(50, 150, 250, 350),
                       n = 10000,
                       lim = 300,
                       main_cols = rcartocolor::carto_pal(n = 7, name = "SunsetDark"),
                       bg_col = "black",
                       size = 0.01,
                       a = 0.5,
                       s = 1234) {
  set.seed(s)
  plot_data <- purrr::map(.x = r_max, .f = ~black_hole_points(.x, n, lim))
  plot_data <- dplyr::bind_rows(plot_data)
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = plot_data,
                        mapping = ggplot2::aes(x = .data$x,
                                               y = .data$y,
                                               colour = .data$dists),
               size = size) +
    ggplot2::coord_fixed() +
    ggplot2::scale_colour_gradientn(colours = ggplot2::alpha(main_cols,
                                                             alpha = a)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(colour = bg_col,
                                                            fill = bg_col),
                   plot.background = ggplot2::element_rect(colour = bg_col,
                                                           fill = bg_col))
  return(p)
}
