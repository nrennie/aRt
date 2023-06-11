#' Criss-cross
#'
#' This function generates a generative art ggplot
#' object using intersecting lines.
#'
#' @param n Number of lines per corner. Default 25.
#' @param bg_col Background colour. Default "white".
#' @param line_col Vector of colours (or single colour). Default "grey50".
#' @param linewidth Width of lines. Default 0.1.
#' @param outline_col Outline colour. Default "black".
#' @param outline_width Linewidth of outline. Default 1.5.
#' @param s Random seed. Default 1234.
#' @return A ggplot object.
#' @export

criss_cross <- function(n = 25,
                        bg_col = "white",
                        line_col = "grey50",
                        linewidth = 0.1,
                        outline_col = "black",
                        outline_width = 1.5,
                        s = 1234) {
  set.seed(s)
  top_left <-
    data.frame(
      x = rep(0, 2 * n),
      y = rep(1, 2 * n),
      xend = c(stats::runif(n, 0.5, 1), rep(1, n)),
      yend = c(rep(0, n), stats::runif(n, 0, 0.5)),
      grp = seq_len(2 * n),
      line_col = sample(line_col, size = 2 * n, replace = TRUE)
    )
  bottom_left <-
    data.frame(
      x = rep(0, 2 * n),
      y = rep(0, 2 * n),
      xend = c(stats::runif(n, 0.5, 1), rep(1, n)),
      yend = c(rep(1, n), stats::runif(n, 0.5, 1)),
      grp = seq_len(2 * n),
      line_col = sample(line_col, size = 2 * n, replace = TRUE)
    )
  top_right <-
    data.frame(
      x = rep(1, 2 * n),
      y = rep(1, 2 * n),
      xend = c(stats::runif(n, 0, 0.5), rep(0, n)),
      yend = c(rep(0, n), stats::runif(n, 0, 0.5)),
      grp = seq_len(2 * n),
      line_col = sample(line_col, size = 2 * n, replace = TRUE)
    )
  bottom_right <-
    data.frame(
      x = rep(1, 2 * n),
      y = rep(0, 2 * n),
      xend = c(stats::runif(n, 0, 0.5), rep(0, n)),
      yend = c(rep(1, n), stats::runif(n, 0.5, 1)),
      grp = seq_len(2 * n),
      line_col = sample(line_col, size = 2 * n, replace = TRUE)
    )
  square_data <- data.frame(
    x = c(0, 1, 1, 0, 0),
    y = c(0, 0, 1, 1, 0)
  )
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = top_left,
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend,
        group = .data$grp,
        colour = .data$line_col
      ),
      linewidth = linewidth
    ) +
    ggplot2::geom_segment(
      data = bottom_left,
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend,
        group = .data$grp,
        colour = .data$line_col
      ),
      linewidth = linewidth
    ) +
    ggplot2::geom_segment(
      data = top_right,
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend,
        group = .data$grp,
        colour = .data$line_col
      ),
      linewidth = linewidth
    ) +
    ggplot2::geom_segment(
      data = bottom_right,
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend,
        group = .data$grp,
        colour = .data$line_col
      ),
      linewidth = linewidth
    ) +
    ggplot2::geom_polygon(
      data = square_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y
      ),
      fill = "transparent",
      colour = outline_col,
      linewidth = outline_width
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        colour = bg_col,
        fill = bg_col
      ),
      panel.background = ggplot2::element_rect(
        colour = bg_col,
        fill = bg_col
      ),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}
