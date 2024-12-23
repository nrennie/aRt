#' Waves
#'
#' This function generates a generative art ggplot object
#' from sine and cosine waves.
#'
#' @param a sine wave parameter. Default 23.
#' @param b Cosine wave parameter. Default 6.
#' @param linewidth Width of lines. Default 0.5.
#' @param main_col Vector of colours (or single colour) for lines. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 2021.
#' @return A ggplot object.
#' @examples
#' waves()
#' @export

waves <- function(a = 23,
                  b = 6,
                  linewidth = 0.5,
                  main_col = "black",
                  bg_col = "white",
                  s = 2021) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      x <- seq(0, 50 * pi, 0.01)
      y <- sample(1:8, size = 1) * sin(a * x) +
        sample(1:8, size = 1) * cos(b * x)
      plot_data <- data.frame(x = x, y = y)
      plot_data
    }
  )

  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    ggplot2::geom_path(ggplot2::aes(colour = .data$y), linewidth = linewidth) +
    ggplot2::scale_colour_gradientn(colours = rev(main_col)) +
    ggplot2::coord_polar() +
    theme_aRt(bg_col)
  return(p)
}
