#' Dots
#'
#' This function generates a coloured generative art ggplot object using polar coordinates.
#'
#' @param n_x Number of rotational points. Default 50.
#' @param n_y Number of outwards points. Default 100.
#' @param jitter_size_width Size of jitter width. Default 0.5.
#' @param jitter_size_height Size of jitter height. Default 0.5.
#' @param col_palette Vector of colours. Default `"Purp"` colour palette from rcartocolor.
#' @param bg_col Background colour. Default `"#63589f"`.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' dots()
#' @export

dots <- function(n_x = 50,
                 n_y = 100,
                 jitter_size_width = 0.5,
                 jitter_size_height = 0.5,
                 col_palette = rcartocolor::carto_pal(n = 7, "Purp"),
                 bg_col = "#63589f",
                 s = 1234) {
  x <- rep(1:n_x, times = n_y)
  y <- rep(1:n_y, each = n_x)
  plot_data <- data.frame(x = x, y = y)
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      colour = .data$y
    )
  ) +
    ggplot2::geom_jitter(
      size = 0.5,
      width = jitter_size_width,
      height = jitter_size_height
    ) +
    ggplot2::scale_colour_gradientn(colours = rev(col_palette)) +
    ggplot2::coord_polar() +
    theme_aRt(bg_col)
  return(p)
}
