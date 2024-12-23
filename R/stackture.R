#' Stack
#'
#' This function generates an sf object.
#'
#' @param x0 x-coordinate of middle of rectangle.
#' @param y0 y-coordinate of middle of rectangle.
#' @param min_height minimum height.
#' @param max_height maximum height.
#' @param min_width minimum width.
#' @param max_width maximum width.
#' @return An sf object.

stack <- function(x0, y0,
                  min_height, max_height,
                  min_width, max_width) {
  width <- stats::runif(1, min_width, max_width)
  height <- stats::runif(1, min_height, max_height)
  x_vals <- c(x0 - (width / 2), x0 + (width / 2), x0 + (width / 2), x0 - (width / 2), x0 - (width / 2))
  y_vals <- c(y0 - (height / 2), y0 - (height / 2), y0 + (height / 2), y0 + (height / 2), y0 - (height / 2))
  square_m <- matrix(c(x_vals, y_vals), byrow = FALSE, ncol = 2)
  square_sf <- sf::st_polygon(list(square_m))
  sf_polygon <- sf::st_sf(geometry = sf::st_sfc(square_sf))
  return(sf_polygon)
}



#' Stackture
#'
#' This function generates a coloured generative art ggplot object using
#' overlapping semi-transparent circles.
#'
#' @param n_x Number of columns in grid. Default 8.
#' @param n_y Number of rows in grid. Default 8.
#' @param min_height minimum height.
#' @param max_height maximum height.
#' @param min_width minimum width.
#' @param max_width maximum width.
#' @param interpolate Boolean indicating if colours should be interpolated. Default `TRUE`.
#' @param col_palette Vector of colours. Default `c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7")`.
#' @param bg_col Background colour. Default `"#004B67"`.
#' @param alpha Transparency. Default 1.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' stackture()
#' @export

stackture <- function(n_x = 8,
                      n_y = 8,
                      min_height = 1, max_height = 1.5,
                      min_width = 1, max_width = 1.5,
                      interpolate = TRUE,
                      col_palette = c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7"),
                      bg_col = "#004B67",
                      alpha = 1,
                      s = 1234) {
  all_data <- withr::with_seed(
    seed = s,
    code = {
      plot_grid <- expand.grid(x = 1:n_x, y = 1:n_y)
      all_data <- purrr::map2(
        .x = plot_grid$x,
        .y = plot_grid$y,
        .f = ~ stack(
          x0 = .x,
          y0 = .y,
          min_height = min_height,
          max_height = max_height,
          min_width = min_width,
          max_width = max_width
        )
      ) |>
        dplyr::bind_rows()
      all_data <- dplyr::slice_sample(all_data, n = nrow(all_data))
      if (interpolate) {
        all_data$col <- sample(grDevices::colorRampPalette(col_palette)(n_x * n_y))
      } else {
        all_data$col <- sample(col_palette, size = n_x * n_y, replace = TRUE)
      }
      all_data
    }
  )
  p <- ggplot2::ggplot(data = all_data) +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(fill = .data$col),
      colour = bg_col,
      alpha = alpha
    ) +
    ggplot2::scale_fill_identity() +
    theme_aRt(bg_col)
  return(p)
}
