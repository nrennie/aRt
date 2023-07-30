#' Split jitter
#'
#' This function generates a generative art ggplot
#' object using polygons
#'
#' @param n_x Number of columns. Default 4.
#' @param n_y Number of rows. Default 4.
#' @param deg_jitter Degree of jitter. Default 0.4.
#' @param col_palette Vector of colours. Must be at least length 4.
#' Default `c("#416322", "#4e7628", "#5a892f", "#679c35", "#74af3b", "#80c044", "#8dc657")`
#' @param line_col Colour of lines. Default "transparent".
#' @param linewidth Width of lines. Default 1.
#' @param bg_col Background colour. Default "#679c35".
#' @param s Random seed. Default 1234.
#' @return A ggplot object.
#' @export

split_jitter <- function(n_x = 5,
                         n_y = 5,
                         deg_jitter = 0.4,
                         col_palette = c(
                           "#416322", "#4e7628", "#5a892f", "#679c35",
                           "#74af3b", "#80c044", "#8dc657"
                         ),
                         line_col = "transparent",
                         linewidth = 1,
                         bg_col = "#679c35",
                         s = 1234) {
  # make polygons
  if (n_x < 1 || n_y < 1) {
    stop("Number of rows and columns must be at least 1")
  }
  if (deg_jitter < 0 || deg_jitter > 0.5) {
    stop("deg_jitter must be between 0 and 0.5")
  }
  set.seed(s)
  x <- rep(1:(n_x + 1), times = n_y + 1) + stats::runif((n_x + 1) * (n_y + 1), 0, deg_jitter)
  y <- rep(1:(n_y + 1), each = n_x + 1) + stats::runif((n_x + 1) * (n_y + 1), 0, deg_jitter)
  poly_data <- tibble::tibble(x = x, y = y) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      x_id = rep(1:(n_x + 1), times = n_y + 1),
      y_id = rep(1:(n_y + 1), each = n_x + 1)
    )
  x1 <- matrix(NA, ncol = n_x, nrow = n_y)
  x2 <- matrix(NA, ncol = n_x, nrow = n_y)
  x3 <- matrix(NA, ncol = n_x, nrow = n_y)
  x4 <- matrix(NA, ncol = n_x, nrow = n_y)
  y1 <- matrix(NA, ncol = n_x, nrow = n_y)
  y2 <- matrix(NA, ncol = n_x, nrow = n_y)
  y3 <- matrix(NA, ncol = n_x, nrow = n_y)
  y4 <- matrix(NA, ncol = n_x, nrow = n_y)
  group <- matrix(NA_character_, ncol = n_x, nrow = n_y)
  for (i in 1:n_x) {
    for (j in 1:n_y) {
      x1[j, i] <- dplyr::filter(poly_data, .data$x_id == i, .data$y_id == j) |> dplyr::pull(x)
      x2[j, i] <- dplyr::filter(poly_data, .data$x_id == i + 1, .data$y_id == j) |> dplyr::pull(x)
      x3[j, i] <- dplyr::filter(poly_data, .data$x_id == i + 1, .data$y_id == j + 1) |> dplyr::pull(x)
      x4[j, i] <- dplyr::filter(poly_data, .data$x_id == i, .data$y_id == j + 1) |> dplyr::pull(x)

      y1[j, i] <- dplyr::filter(poly_data, .data$x_id == i, .data$y_id == j) |> dplyr::pull(y)
      y2[j, i] <- dplyr::filter(poly_data, .data$x_id == i + 1, .data$y_id == j) |> dplyr::pull(y)
      y3[j, i] <- dplyr::filter(poly_data, .data$x_id == i + 1, .data$y_id == j + 1) |> dplyr::pull(y)
      y4[j, i] <- dplyr::filter(poly_data, .data$x_id == i, .data$y_id == j + 1) |> dplyr::pull(y)

      i_val <- ifelse(i <= 9, paste0(i), paste0(0, i))
      j_val <- ifelse(j <= 9, paste0(j), paste0(0, j))
      group[j, i] <- paste0(i_val, "-", j_val)
    }
  }
  plot_data <- tibble::tibble(
    x = c(as.vector(x1), as.vector(x2), as.vector(x3), as.vector(x4)),
    y = c(as.vector(y1), as.vector(y2), as.vector(y3), as.vector(y4)),
    group = c(as.vector(group), as.vector(group), as.vector(group), as.vector(group))
  )
  polygons <- unique(plot_data$group)
  final_plot_data <- tibble::tibble(
    x = c(), y = c(), grp = c(), fill = c()
  )
  for (i in seq_len(length(polygons))) {
    p <- polygons[i]
    p_data <- dplyr::filter(plot_data, group == p)
    x_corners <- p_data$x
    y_corners <- p_data$y
    # convert to polygon
    sf_poly <- sf::st_polygon(
      list(cbind(
        c(x_corners, x_corners[1]),
        c(y_corners, y_corners[1])
      ))
    )
    # sample from middle
    mid <- sf::st_sample(sf_poly, 1) |>
      sf::st_coordinates() |>
      as.vector()
    # make polygons
    new_plot_data <- tibble::tribble(
      ~x, ~y, ~grp,
      # bottom
      x_corners[1], y_corners[1], paste0(p, "-", "1"),
      mid[1], mid[2], paste0(p, "-", "1"),
      x_corners[2], y_corners[2], paste0(p, "-", "1"),
      x_corners[1], y_corners[1], paste0(p, "-", "1"),
      # left
      x_corners[1], y_corners[1], paste0(p, "-", "2"),
      mid[1], mid[2], paste0(p, "-", "2"),
      x_corners[4], y_corners[4], paste0(p, "-", "2"),
      x_corners[1], y_corners[1], paste0(p, "-", "2"),
      # top
      x_corners[4], y_corners[4], paste0(p, "-", "3"),
      mid[1], mid[2], paste0(p, "-", "3"),
      x_corners[3], y_corners[3], paste0(p, "-", "3"),
      x_corners[4], y_corners[4], paste0(p, "-", "3"),
      # right
      x_corners[3], y_corners[3], paste0(p, "-", "4"),
      mid[1], mid[2], paste0(p, "-", "4"),
      x_corners[2], y_corners[2], paste0(p, "-", "4"),
      x_corners[3], y_corners[3], paste0(p, "-", "4")
    )
    # select colours
    new_plot_data$fill <- as.character(
      rep(sample(col_palette, size = 4), each = 4)
    )
    # join new data
    final_plot_data <- rbind(final_plot_data, new_plot_data)
  }
  # map over all polygon
  p <- ggplot2::ggplot(data = final_plot_data) +
    ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y, fill = .data$fill, group = .data$grp
      ),
      colour = line_col,
      linewidth = linewidth
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col)
    )
  return(p)
}
