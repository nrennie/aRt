#' Fading
#'
#' This function generates a coloured generative art ggplot object using
#' voronoi tiles.
#'
#' @param n_layers Number of layers. Default 6.
#' @param n_points Number of points per layer area. Default 10.
#' @param col_palette Vector of colours. Default "SunsetDark" colour palette
#' from rcartocolor.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

fading <- function(n_layers = 6,
                   n_points = 10,
                   col_palette = rcartocolor::carto_pal(n = 7, "SunsetDark"),
                   s = 1234) {
  n_points <- n_points * (n_layers:1)
  x_widths <- 2 * (n_layers:1)
  x_lower <- cumsum(2 * (n_layers:1))
  x_upper <- x_lower + x_widths
  y_widths <- 4 * (n_layers:1)
  y_lower <- cumsum(4 * (n_layers:1))
  y_upper <- y_lower + y_widths
  y <- unlist(lapply(1:n_layers, function(i) round(stats::runif(n_points[i], y_lower[i], y_upper[i]), 1)))
  x <- round(-0.5 * y + unlist(lapply(1:n_layers, function(i) round(stats::runif(n_points[i], x_lower[i], x_upper[i]), 1))), 1)
  z <- y + stats::rnorm(length(x), 0, 0.5)
  df <- tibble::tibble(x = x, y = y, z = z)
  df <- dplyr::filter(df, !is.na(y))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = -1L)) +
    ggforce::geom_voronoi_tile(ggplot2::aes(fill = z)) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_fill_gradientn(colours = rev(col_palette)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
      plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"), # top, right, bottom, left
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  suppressWarnings(print(p))
}
