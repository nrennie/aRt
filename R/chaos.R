#' Chaos
#'
#' This function generates a generative art ggplot object using a
#' lines, points, and circles.
#'
#' @param n_lines Number of lines. Default 75.
#' @param n_points Number of points. Default 10.
#' @param n_circles Number of circles. Default 20.
#' @param line_col Line colour. Default "grey70".
#' @param point_col Point colour. Default "black".
#' @param circle_col Circle fill colour. Default "white".
#' @param circle_line_col Circle line colour. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param min_circle Minimum circle radius. Default 0.01.
#' @param max_circle Maximum circle radius. Default 0.1.
#' @param linewidth Linewidth of lines and circles. Default 0.2.
#' @param alpha Transparency of circles. Default 0.5.
#' @param size Size of points. Default 0.3.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' chaos()
#' @export

chaos <- function(n_lines = 75,
                  n_points = 10,
                  n_circles = 20,
                  line_col = "grey70",
                  point_col = "black",
                  circle_col = "white",
                  circle_line_col = "black",
                  bg_col = "white",
                  min_circle = 0.01,
                  max_circle = 0.1,
                  linewidth = 0.2,
                  alpha = 0.5,
                  size = 0.3,
                  s = 1234) {
  line_data <- withr::with_seed(
    seed = s,
    code = {
      line_data <- tibble::tibble(
        x1 = stats::runif(n_lines),
        y1 = stats::runif(n_lines),
        x2 = stats::runif(n_lines),
        y2 = stats::runif(n_lines),
        grp = seq_len(n_lines)
      )
    }
  )
  point_data <- withr::with_seed(
    seed = s,
    code = {
      point_data <- tibble::tibble(
        x = stats::runif(n_points),
        y = stats::runif(n_points)
      )
      point_data
    }
  )
  circle_data <- withr::with_seed(
    seed = s,
    code = {
      circle_data <- tibble::tibble(
        x0 = stats::runif(n_circles),
        y0 = stats::runif(n_circles),
        r = stats::runif(n_circles, min_circle, max_circle)
      )
      circle_data
    }
  )
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = line_data,
      mapping = ggplot2::aes(
        x = .data$x1,
        y = .data$y1,
        xend = .data$x2,
        yend = .data$y2,
        group = .data$grp
      ),
      linewidth = linewidth,
      colour = line_col
    ) +
    ggplot2::geom_point(
      data = point_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y
      ),
      size = size,
      colour = point_col
    ) +
    ggforce::geom_circle(
      data = circle_data,
      mapping = ggplot2::aes(
        x0 = .data$x0,
        y0 = .data$y0,
        r = .data$r
      ),
      fill = circle_col,
      alpha = alpha,
      linewidth = linewidth,
      colour = circle_line_col
    ) +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col, 5)
  return(p)
}
