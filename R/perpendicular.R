#' Generate line
#'
#' This function generates a vertical or horizontal line.
#'
#' @param x x-cordinate of middle of line
#' @param y y-cordinate of middle of line
#' @param length length of line
#' @param angle horizontal or vertical line. One of c("v", "h").
#' @param group group to identify part of same rectangle. Default 1.
#' @return a data frame
#' @noRd

generate_line <- function(x, y, length, angle, group = 1) {
  if (!(angle %in% c("v", "h"))) {
    stop("angle must be one of c('v', 'h').")
  }
  if (angle == "h") {
    output <- data.frame(
      x = x - 0.5 * length,
      xend = x + 0.5 * length,
      y = y,
      yend = y,
      group = group
    )
  }
  if (angle == "v") {
    output <- data.frame(
      x = x,
      xend = x,
      y = y - 0.5 * length,
      yend = y + 0.5 * length,
      group = group
    )
  }
  return(output)
}

#' Perpendicular
#'
#' This function generates a generative art ggplot object featuring multiple coloured perpendicular lines.
#'
#' @param n Number of rectangles. Default 100.
#' @param max_length Maximum length of line. Default 7.
#' @param linewidth Line width of line. Default 0.5.
#' @param main_col Colour of lines. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 123.
#' @return A ggplot object
#' @examples
#' perpendicular()
#' @export

perpendicular <- function(n = 100,
                          max_length = 7,
                          linewidth = 0.5,
                          main_col = "black",
                          bg_col = "white",
                          s = 123) {
  if (max_length <= 2) {
    stop("max_length should be > 2")
  }
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      set.seed(s)
      plot_data <- data.frame(
        x = c(),
        xend = c(),
        y = c(),
        yend = c(),
        group = c()
      )
      for (i in 1:n) {
        k <- generate_line(
          x = stats::runif(1, 0, 30),
          y = stats::runif(1, 0, 30),
          length = stats::runif(1, 2, max_length),
          angle = sample(c("v", "h"), size = 1),
          group = i
        )
        plot_data <- rbind(plot_data, k)
      }
      plot_data
    }
  )
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    )
  ) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(group = .data$group),
      linewidth = linewidth,
      colour = main_col
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::xlim(
      (min(c(plot_data$x, plot_data$y, plot_data$xend, plot_data$yend))),
      (max(c(plot_data$x, plot_data$y, plot_data$xend, plot_data$yend)))
    ) +
    ggplot2::ylim(
      (min(c(plot_data$x, plot_data$y, plot_data$xend, plot_data$yend))),
      (max(c(plot_data$x, plot_data$y, plot_data$xend, plot_data$yend)))
    ) +
    theme_aRt(bg_col)
  return(p)
}
