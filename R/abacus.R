#' Abacus
#'
#' This function generates a generative art ggplot
#' object using points and lines.
#'
#' @param nx Number of lines in x direction. Default 30.
#' @param ny Number of points per line. Default 100.
#' @param max_size Maximum size of points. Default 2.
#' @param main_col Vector of colours (or single colour). Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Random seed. Default 123.
#' @return A ggplot object.
#' @examples
#' abacus()
#' @export

abacus <- function(nx = 30,
                   ny = 100,
                   max_size = 2,
                   main_col = "black",
                   bg_col = "white",
                   s = 123) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      tibble::tibble(
        x = rep(1:nx, ny),
        y = stats::runif(nx * ny, 0, 10),
        size = stats::rexp(nx * ny)
      )
    }
  )

  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y
    )
  ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(group = .data$x),
      alpha = 0.1,
      colour = main_col,
      size = 0.3
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(size = .data$size),
      pch = 21,
      fill = main_col,
      colour = main_col,
      alpha = 0.3
    ) +
    ggplot2::scale_size(range = c(0.3, max_size)) +
    theme_aRt(bg_col)
  return(p)
}
