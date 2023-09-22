#' Riso Circles
#'
#' This function generates a coloured generative art ggplot object using
#' overlapping semi-transparent circles.
#'
#' @param n_x Number of columns in grid. Default 4.
#' @param n_y Number of rows in grid. Default 4.
#' @param n_circles Number of circles per grid square. Default 2.
#' @param r Radius of circles. Default 0.5.
#' @param jitter_x Jitter in x direction for circle positions. Default 0.25.
#' @param jitter_y Jitter in y direction for circle positions. Default 0.3.
#' @param alpha Transparency of circles. Default 0.5.
#' @param col_palette Vector of colours. Default `c("#6497b1", "#6a359c", "#FFB04F", "#679c35", "#cd1076")`.
#' @param cicrle_bg Background colour of circles. Default "#fafafa".
#' @param bg_col Background colour. Default "gray10".
#' @param interpolate Boolean indicating if colours should be interpolated. Default TRUE.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

riso_circles <- function(n_x = 4,
                         n_y = 4,
                         n_circles = 2,
                         r = 0.5,
                         jitter_x = 0.25,
                         jitter_y = 0.3,
                         alpha = 0.5,
                         col_palette = c("#6497b1", "#6a359c", "#FFB04F", "#679c35", "#cd1076"),
                         circle_bg = "#fafafa",
                         bg_col = "gray10",
                         interpolate = TRUE,
                         s = 1234) {
  set.seed(s)
  # create data
  plot_data <- data.frame(
    x0 = stats::runif(n_x * n_y * n_circles, -1 * jitter_x, jitter_x),
    y0 = stats::runif(n_x * n_y * n_circles, -1 * jitter_y, jitter_y),
    r = rep(r, n_x * n_y * n_circles),
    grp = as.character(rep(seq_len(n_x * n_y), each = n_circles))
  )
  if (interpolate) {
    plot_data$cols <- sample(grDevices::colorRampPalette(col_palette)(n_x * n_y * n_circles))
  } else {
    plot_data$cols <- sample(col_palette, size = n_x * n_y * n_circles, replace = TRUE)
  }
  # plot
  g <- ggplot2::ggplot(data = plot_data) +
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
      fill = circle_bg,
      colour = "transparent"
    ) +
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r, fill = cols),
      colour = "transparent",
      alpha = alpha
    ) +
    ggplot2::facet_wrap(~grp, ncol = n_x, nrow = n_y) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      strip.text = ggplot2::element_blank()
    )
  return(g)
}
