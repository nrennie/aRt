#' Mosiac
#'
#' This function generates a generative art ggplot object from voronoi tiles.
#'
#' @param n Number of points to generate tiles from. Default 100.
#' @param fill_cols Vector of colours to fill tiles with, Default
#' `c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E")`.
#' @param line_col Colour of lines between tiles, Default "white".
#' @param bg_col Background colour. Default "white".
#' @param line_size Thickness of lines between tiles. Default 1.
#' @param x_means Vector of any number of means for the x-coordinate.
#' Default `c(0, 10, 5)`.
#' @param y_means Vector of any number of means for the y-coordinate.
#' Default `c(0, 7, 8)`.
#' @param xy_var Numeric variance of x and y points. Default 2.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

mosaic <- function(n = 100,
                   fill_cols = c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E"),
                   line_col = "white",
                   bg_col = "white",
                   line_size = 1,
                   x_means = c(0, 10, 5),
                   y_means = c(0, 7, 8),
                   xy_var = 2,
                   s = 1234) {
  set.seed(s)
  df <- purrr::map2(
    .x = x_means,
    .y = y_means,
    .f = ~ data.frame(x = rnorm(n, .x, xy_var), y = rnorm(n, .y, xy_var))
  )
  plot_data <- dplyr::bind_rows(df)
  num_cols <- length(fill_cols)
  plot_data$fill_col <- factor(sample(1:num_cols,
    size = nrow(plot_data),
    replace = TRUE
  ))
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = .data$fill_col
    )
  ) +
    ggvoronoi::geom_voronoi() +
    ggvoronoi::stat_voronoi(
      geom = "path",
      colour = line_col,
      size = line_size
    ) +
    ggplot2::scale_fill_manual(values = fill_cols) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(
        min(c(plot_data$x, plot_data$y)),
        (max(c(plot_data$x, plot_data$y)))
      )
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = c(
        min(c(plot_data$x, plot_data$y)),
        (max(c(plot_data$x, plot_data$y)))
      )
    ) +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), unit = "cm"),
      panel.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      ),
      plot.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      )
    )
  return(suppressWarnings(print(p)))
}
