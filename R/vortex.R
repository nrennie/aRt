#' Vortex
#'
#' This function generates a greyscale or rainbow coloured generative art ggplot object shaped like a vortex.
#'
#' @param n Number of points. Default 25.
#' @param start_val Starting position for polar coordinates. Default 90.
#' @param col_palette Colour palette. Default `c("#700460", "#a02c5d", "#ec0f47", "#ee6b3b", "#fbbf54", "#abd96d", "#15c286", "#087353", "#045459", "#4b0082")`.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' vortex()
#' @export

vortex <- function(n = 25,
                   start_val = 90,
                   col_palette = c("#700460", "#a02c5d", "#ec0f47", "#ee6b3b", "#fbbf54", "#abd96d", "#15c286", "#087353", "#045459", "#4b0082"),
                   bg_col = "black",
                   s = 1234) {
  # generate data
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      m <- n * length(col_palette)
      plot_data <- data.frame(
        id = seq(1, m), value = sample(seq(0, m), m, replace = TRUE),
        type = factor(rep(c(seq_along(col_palette)), each = (m / length(col_palette))))
      )
      plot_data
    }
  )

  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$id,
        y = .data$value,
        group = .data$type,
        colour = .data$type
      )
    ) +
    ggplot2::scale_color_manual(values = col_palette) +
    ggplot2::coord_polar(start = start_val, theta = "y") +
    theme_aRt(bg_col)
  return(p)
}
