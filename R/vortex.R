#' Vortex
#'
#' This function generates a greyscale or rainbow coloured generative art ggplot object shaped like a vortex.
#'
#' @param n Number of points. Default 25.
#' @param start_val Starting position for polar coordinates. Default 90.
#' @param col_scheme Colour scheme of art. One of c("mono", "rainbow). Default 0.1.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' vortex()
#' @export

vortex <- function(n = 25,
                   start_val = 90,
                   col_scheme = "mono",
                   bg_col = "black",
                   s = 1234) {
  # colour schemes
  if (col_scheme == "mono") {
    cols <- rev(c("gray100", "gray95", "gray90", "gray85", "gray80", "gray70", "gray60", "gray50", "gray15", "gray0"))
  } else if (col_scheme == "rainbow") {
    cols <- c("#700460", "#a02c5d", "#ec0f47", "#ee6b3b", "#fbbf54", "#abd96d", "#15c286", "#087353", "#045459", "#4b0082")
  }
  # generate data
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      m <- n * 10
      plot_data <- data.frame(
        id = seq(1, m), value = sample(seq(0, m), m, replace = TRUE),
        type = factor(rep(c(1:10), each = (m / 10)))
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
    ggplot2::scale_color_manual("", values = cols) +
    ggplot2::coord_polar(start = start_val, theta = "y") +
    theme_aRt(bg_col)
  return(p)
}
