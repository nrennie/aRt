#' Generate four polygons combined in square
#'
#' @param x_nudge x-coordinate of bottom left of square. Default 0.
#' @param y_nudge y-coordinate of bottom left of square. Default 0.
#' @param col_palette Vector of colours. Must be at least length 4.
#' @noRd

split_single <- function(x_nudge = 0,
                         y_nudge = 0,
                         x_corners = c(0, 1, 1, 0),
                         y_corners = c(0, 0, 1, 1),
                         col_palette) {
  # checks
  if (length(x_corners) != 4) {
    stop("x_corner must be of length 4")
  }
  if (length(y_corners) != 4) {
    stop("y_corner must be of length 4")
  }
  # midpoint
  mid <- stats::runif(2, 0.1, 0.9)
  # make polygons
  plot_data <- tibble::tribble(
    ~x, ~y, ~grp,
    # bottom
    x_corners[1], y_corners[1], "1",
    mid[1], mid[2], "1",
    x_corners[2], y_corners[2], "1",
    x_corners[1], y_corners[1], "1",
    # left
    x_corners[1], y_corners[1], "2",
    mid[1], mid[2], "2",
    x_corners[4], y_corners[4], "2",
    x_corners[1], y_corners[1], "2",
    # top
    x_corners[4], y_corners[4], "3",
    mid[1], mid[2], "3",
    x_corners[3], y_corners[3], "3",
    x_corners[4], y_corners[4], "3",
    # right
    x_corners[3], y_corners[3], "4",
    mid[1], mid[2], "4",
    x_corners[2], y_corners[2], "4",
    x_corners[3], y_corners[3], "4"
  )
  # adjust x and y
  plot_data$x <- plot_data$x + x_nudge
  plot_data$y <- plot_data$y + y_nudge
  # select colours
  plot_data$fill <- as.character(
    rep(sample(col_palette, size = 4), each = 4)
  )
  return(plot_data)
}

#' Split grid
#'
#' This function generates a generative art ggplot
#' object using polygons
#'
#' @param n_x Number of columns. Default 4.
#' @param n_y Number of rows. Default 4.
#' @param col_palette Vector of colours. Must be at least length 4.
#' Default `c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C")`
#' @param grid_col Colour of grid lines. Default "white".
#' @param grid_width Linewidth of grid lines. Default 1.
#' @param s Random seed. Default 1234.
#' @return A ggplot object.
#' @export

split_grid <- function(n_x = 4,
                       n_y = 4,
                       col_palette = c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C"),
                       grid_col = "white",
                       grid_width = 1,
                       s = 1234) {
  if (length(col_palette) < 4) {
    stop("col_palette must have at least 4 colours")
  }
  set.seed(s)
  plot_grid <- expand.grid(
    x = seq_len(n_x),
    y = seq_len(n_y)
  )
  all_data <- purrr::map2(
    .x = plot_grid$x,
    .y = plot_grid$y,
    .f = ~ split_single(
      x_nudge = .x,
      y_nudge = .y,
      col_palette = col_palette
    )
  )
  names(all_data) <- seq_len(n_x * n_y)
  plot_data <- dplyr::bind_rows(all_data, .id = "grp2") |>
    tidyr::unite("grp", "grp", "grp2", sep = "-") |>
    tibble::as_tibble()
  ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y, fill = .data$fill, group = .data$grp
      )
    ) +
    ggplot2::geom_tile(
      data = plot_grid,
      mapping = ggplot2::aes(
        x = .data$x + 0.5, y = .data$y + 0.5
      ),
      colour = grid_col,
      linewidth = grid_width,
      fill = "transparent"
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
