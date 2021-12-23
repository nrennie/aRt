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
    cols <- c("#700460", "#a02c5d", "#ec0f47", "#ee6b3b", "#fbbf54", "#abd96d", "#15c286", "#087353", "#045459", "4b0082")
  }
  # generate data
  m <- n * 10
  df1 <- data.frame(id = seq(1, m), value = sample(seq(0, m), m, replace = TRUE), type = factor(rep(c(1:10), each = (m / 10))))
  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df1, mapping = ggplot2::aes(x = .data$id, y = .data$value, group = .data$type, colour = .data$type)) +
    ggplot2::scale_color_manual("", values = cols) +
    ggplot2::coord_polar(start = start_val, theta = "y") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.title = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_blank(),
                   legend.position = "none",
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"), # top, right, bottom, left
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()
    )
  return(p)
}
