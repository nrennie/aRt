#' Heart
#'
#' This function generates a greyscale or rainbow coloured generative art ggplot object in the shape of a heart.
#'
#' @param n Number of lines per colour. Default 25.
#' @param col_scheme Colour scheme of art. One of c("mono", "rainbow). Default 0.1.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export


heart <- function(n = 25, col_scheme = "mono", bg_col = "black", s = 1234) {
  set.seed(s)
  # colour schemes
  if (col_scheme == "mono") {
    cols <- rev(c("gray100", "gray95", "gray90", "gray85", "gray80", "gray70", "gray60", "gray50", "gray15", "gray0"))
  } else if (col_scheme == "rainbow") {
    cols <- c("#700460", "#a02c5d", "#ec0f47", "#ee6b3b", "#fbbf54", "#abd96d", "#15c286", "#087353", "#045459", "4b0082")
  }
  # define times
  t1 <- seq(-0.5 * pi, 1.5 * pi, length = n * 10)
  # find points
  df1 <- data.frame(x = 16 * (sin(t1))^3, y = 13 * cos(t1) - 5 * cos(2 * t1) - 2 * cos(3 * t1) - cos(4 * t1))
  # randomise
  df1_random <- df1[sample(seq_len(nrow(df1)), size = nrow(df1), replace = FALSE), ]
  df1_random$col_choice <- rep(cols, times = n)
  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = df1_random, mapping = ggplot2::aes(x = .data$x, y = .data$y, colour = I(.data$col_choice))) +
    ggplot2::xlim(-17, 17) +
    ggplot2::ylim(-19, 15) +
    ggplot2::coord_fixed() +
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
  p
}
