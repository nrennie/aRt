#' Wander
#'
#' This function generates a generative art ggplot
#' object using random walks
#'
#' @param n_lines Number of lines. Default 100.
#' @param n_points Number of points. Default 350.
#' @param r_outer Radius of outer circle. Default 8.
#' @param r_inner Radius of inner circle. Default 3.
#' @param line_var Variance of random walk noise. Default 0.01.
#' @param deg_jitter Degree of jitter for multiple lines. Default 0.1.
#' @param linewidth Width of lines. Default 0.1.
#' @param bg_col Background colour. Default "#462255".
#' @param col_palette Vector of colours. Default `c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C")`
#' @param n_cols Number of colours to create. Default 20.
#' @param s Random seed. Default 123.
#' @return A ggplot object.
#' @export

wander <- function(n_lines = 100,
                   n_points = 350,
                   r_outer = 8,
                   r_inner = 3,
                   line_var = 0.01,
                   deg_jitter = 0.1,
                   linewidth = 0.1,
                   bg_col = "#462255",
                   col_palette = c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C"),
                   n_cols = 20,
                   s = 123) {
  # define boundaries
  set.seed(s)
  theta <- seq(0, 2 * pi, length.out = n_lines)
  x_inner <- r_inner * cos(theta)
  y_inner <- r_inner * sin(theta)
  x_outer <- r_outer * cos(theta)
  y_outer <- r_outer * sin(theta)
  all_cols <- grDevices::colorRampPalette(col_palette)(n_cols)
  # generate lines
  line_data <- purrr::map(
    .x = seq_len(length(theta)),
    .f = ~ data.frame(
      x0 = seq(x_inner[.x], x_outer[.x], length.out = n_points),
      y0 = seq(y_inner[.x], y_outer[.x], length.out = n_points),
      grp = .x,
      col = sample(all_cols, size = 1)
    )
  ) |>
    dplyr::bind_rows()
  # generate noise
  line_data$noise_x <- replicate(
    length(theta),
    cumsum(c(0, stats::rnorm(n = n_points - 1, mean = 0, sd = sqrt(line_var))))
  ) |>
    as.vector()
  line_data$noise_y <- replicate(
    length(theta),
    cumsum(c(0, stats::rnorm(n = n_points - 1, mean = 0, sd = sqrt(line_var))))
  ) |>
    as.vector()

  # add noise
  plot_data <- line_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      x = .data$x0 + .data$noise_x,
      y = .data$y0 + .data$noise_y
    )
  lims <- ceiling(max(abs(c(plot_data$x, plot_data$y))))

  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$grp,
        colour = .data$col
      ),
      alpha = 0.7,
      linewidth = linewidth
    ) +
    ggplot2::geom_path(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$grp,
        colour = .data$col
      ),
      position = ggplot2::position_jitter(
        width = deg_jitter,
        height = deg_jitter
      ),
      alpha = 0.5,
      linewidth = linewidth
    ) +
    ggplot2::geom_path(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$grp,
        colour = .data$col
      ),
      position = ggplot2::position_jitter(
        width = deg_jitter * 2,
        height = deg_jitter * 2
      ),
      alpha = 0.3,
      linewidth = linewidth
    ) +
    ggplot2::scale_x_continuous(limits = c(-lims, lims)) +
    ggplot2::scale_y_continuous(limits = c(-lims, lims)) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
  return(p)
}
