#' Circular
#'
#' This function generates an abstract circular generative art ggplot object.
#'
#' @param n Number of steps from inside to outside. Default 100.
#' @param main_col Colour of lines. Default black.
#' @param bg_col Background colour. Default white.
#' @param s Seed value. Default 56.
#' @return A ggplot object.
#' @export

circular <- function(n = 100,
                     main_col = "black",
                     bg_col = "white",
                     s = 56) {
  if (n < 1) {
    stop("n must be an integer greater than 1")
  }
  output_mat <- matrix(NA, nrow = 360, ncol = n)
  for (i in 1:360) {
    output_mat[i, ] <- rw_circular(n = n, p = 0.5, lower_limit = 10, mid_limit = 90, upper_limit = 100, seed = i * s)
  }
  colnames(output_mat) <- 1:n
  plot_data <- tibble::tibble(val = 1:360, tibble::as_tibble(output_mat))
  plot_data <- tidyr::pivot_longer(plot_data, cols = 2:(n + 1))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$val, y = .data$name, group = .data$val)) +
    ggplot2::geom_line(colour = main_col) +
    ggplot2::coord_polar(start = 0) +
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
