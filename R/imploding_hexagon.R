#' Implode function
#' From ggfx vignettes
#' @param x A `raster` or `nativeRaster` object
#' @param implode_factor Image implode factor. Default 0.5.
#' @noRd
implode <- function(x, implode_factor = 0.5) {
  vp <- magick::image_read(ggfx::get_viewport_area(x))
  vp <- magick::image_implode(vp, factor = implode_factor)
  ggfx::set_viewport_area(x, grDevices::as.raster(vp, native = TRUE))
}

#' Imploding Hexagon
#'
#' This function generates a generative art ggplot
#' object using spatial hexagons and Magick.
#'
#' @param n Number of points. Default 25000.
#' @param size Size of points. Default 0.1.
#' @param bg_col Background colour. Default "grey10".
#' @param col_palette Colour palette. Default
#' `grDevices::grey.colors(n = 10, start = 0.1, end = 0.7)`.
#' @param random Boolean. Should colours be arranged randomly or not?
#' Default `TRUE`.
#' @param implode_factor Image implode factor. Default 0.5.
#' @param s Random seed. Default 123.
#' @return A ggplot object.
#' @examples
#' imploding_hexagon()
#' @export
imploding_hexagon <- function(n = 25000,
                              size = 0.05,
                              bg_col = "grey10",
                              col_palette = grDevices::grey.colors(
                                n = 10,
                                start = 0.1,
                                end = 0.9
                              ),
                              random = TRUE,
                              implode_factor = 0.5,
                              s = 1234) {
  set.seed(s)
  # draw a hexagon
  r <- 1
  theta <- seq(-pi / 2, (2 * pi) - pi / 2, length.out = 7)
  x <- r * c(cos(theta), cos(theta[1]))
  y <- r * c(sin(theta), sin(theta[1]))
  corners <- list(matrix(c(x, y), byrow = FALSE, ncol = 2))
  corners_sf <- sf::st_polygon(corners)

  # sample lots of points from in it
  points_sample <- sf::st_sample(corners_sf, size = n)
  points_sf <- sf::st_sf(points_sample)
  points_sf$x <- sf::st_coordinates(points_sf)[, 1]
  points_sf$y <- sf::st_coordinates(points_sf)[, 2]

  # colour the points
  if (random) {
    points_sf$col <- sample(col_palette, size = n, replace = TRUE)
  } else {
    points_sf <- points_sf |>
      dplyr::mutate(dist_0 = sqrt(.data$x^2 + .data$y^2)) |>
      dplyr::arrange(dplyr::desc(.data$dist_0))
    points_sf$col <- grDevices::colorRampPalette(col_palette)(n)
  }

  # limits
  upper_lim <- ceiling(max(abs(c(points_sf$x, points_sf$y))))
  lower_lim <- -1 * upper_lim

  # plot the points
  g <- ggplot2::ggplot(data = points_sf, mapping = ggplot2::aes(x = x, y = y)) +
    ggfx::with_custom(
      geom_point(
        size = size,
        mapping = ggplot2::aes(colour = col)
      ),
      filter = implode,
      implode_factor = implode_factor
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(limits = c(lower_lim, upper_lim)) +
    ggplot2::scale_y_continuous(limits = c(lower_lim, upper_lim)) +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col)
  return(g)
}
