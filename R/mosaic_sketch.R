#' Rough tiles
#'
#' This function generates voronoi tiles in a hand-sketched look.
#'
#' @param n Number of points to generate tiles from. Default 100.
#' @param fill_cols Vector of colours to fill tiles with, Default c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E").
#' @param line_col Colour of lines between tiles, Default "white".
#' @param bg_col Background colour. Default "white".
#' @param line_size Thickness of lines between tiles. Default 1.
#' @param x_means Vector of any number of means for the x-coordinate. Default c(0, 10, 5).
#' @param y_means Vector of any number of means for the y-coordinate. Default c(0, 7, 8).
#' @param xy_var Numeric varaince of x and y points. Default 2.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @noRd

rough_tiles <- function(n = 10,
                        fill_cols = c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E"),
                        line_col = "white",
                        bg_col = "white",
                        line_size = 2,
                        x_means = c(0, 10, 5),
                        y_means = c(0, 7, 8),
                        xy_var = 2,
                        s = 1234) {
  set.seed(s)
  df <- purrr::map2(.x = x_means,
                    .y = y_means,
                    .f = ~data.frame(x = rnorm(n, .x, xy_var), y = rnorm(n, .y, xy_var)))
  plot_data <- dplyr::bind_rows(df)
  num_cols <- length(fill_cols)
  plot_data$fill_col <- factor(sample(1:num_cols,
                                      size = nrow(plot_data),
                                      replace = TRUE))
  p <- ggplot2::ggplot(data = plot_data,
                       mapping = ggplot2::aes(x = .data$x,
                                              y = .data$y,
                                              fill = .data$fill_col)) +
    ggvoronoi::geom_voronoi() +
    ggvoronoi::stat_voronoi(geom = "path",
                            colour = line_col,
                            linewidth = line_size) +
    ggplot2::scale_fill_manual(values = fill_cols)
  return(p)

}


#' Mosiac Sketch
#'
#' This function generates a generative art ggplot object from voronoi tiles in a hand-sketched look.
#'
#' @param n Number of points to generate tiles from. Default 100.
#' @param fill_cols Vector of colours to fill tiles with, Default c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E").
#' @param line_col Colour of lines between tiles, Default "white".
#' @param bg_col Background colour. Default "white".
#' @param line_size Thickness of lines between tiles. Default 1.
#' @param x_means Vector of any number of means for the x-coordinate. Default c(0, 10, 5).
#' @param y_means Vector of any number of means for the y-coordinate. Default c(0, 7, 8).
#' @param xy_var Numeric varaince of x and y points. Default 2.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

mosaic_sketch <- function(n = 10,
                          fill_cols = c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E"),
                          line_col = "white",
                          bg_col = "white",
                          line_size = 2,
                          x_means = c(0, 10, 5),
                          y_means = c(0, 7, 8),
                          xy_var = 2,
                          s = 1234) {
  p <- rough_tiles(n = n,
                   fill_cols = fill_cols,
                   line_col = line_col,
                   bg_col = bg_col,
                   line_size = line_size,
                   x_means = x_means,
                   y_means = y_means,
                   xy_var = xy_var,
                   s = s)
  k <- ggplot2::ggplot_build(p)
  g <- ggplot2::ggplot() +
    ggpattern::geom_polygon_pattern(data = tibble::as_tibble(k$data[[1]]),
                                    ggplot2::aes(x = .data$x,
                                                 y = .data$y,
                                                 group = .data$group,
                                                 pattern_colour = I(.data$fill),
                                                 pattern_angle = .data$fill,
                                                 pattern_fill = I(.data$fill)),
                                    pattern = "crosshatch",
                                    fill = bg_col,
                                    pattern_size = 0.001,
                                    pattern_spacing = 0.01,
                                    pattern_density = 0.3) +
    ggpattern::scale_pattern_angle_discrete(range = c(0, 90)) +
    ggplot2::geom_path(data = tibble::as_tibble(k$data[[2]]),
                       ggplot2::aes(x = .data$x,
                                    y = .data$y,
                                    group = .data$group,
                                    linewidth = I(.data$linewidth),
                                    colour = I(.data$colour))) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), unit = "cm"),
                   panel.background = ggplot2::element_rect(fill = bg_col,
                                                            colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col,
                                                           colour = bg_col))
  return(g)
}
