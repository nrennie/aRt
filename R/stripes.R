#' Stripes
#'
#' This function generates a generative art ggplot object featuring rows of stripes.
#'
#' @param perc Percentage of data points to be sorted. Default 0.5.
#' @param n Number of rows. Default 3.
#' @param col_palette Colour palette from rcartocolor.
#' @param alpha Transparancy of fill
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @export

stripes <- function(perc = 0.5,
                    n = 3,
                    col_palette = "TealGrn",
                    alpha = 1,
                    s = 1234) {
  if (perc < 0 | perc > 1) {
    stop("perc not between 0 and 1")
  }
  if (n < 1) {
    stop("n must be a positive integer")
  }
  set.seed(s)
  plot_df <- matrix(NA, ncol = 1000, nrow = n)
  for (i in seq_len(n)) {
    k <- stats::runif(1000)
    vals <- sample(x = 1:1000, size = perc * 1000, replace = FALSE)
    k[sort(vals)] <- sort(k[vals])
    plot_df[i, ] <- k
  }
  colnames(plot_df) <- seq_len(ncol(plot_df))
  rownames(plot_df) <- seq_len(nrow(plot_df))
  plot_data <- tibble::tibble(times = seq_len(nrow(plot_df)), tibble::as_tibble(plot_df))
  plot_data <- tidyr::pivot_longer(plot_data, cols = 2:(ncol(plot_df) + 1))
  p <- ggplot2::ggplot(data = plot_data,
                       ggplot2::aes(x = .data$times, y = as.numeric(.data$name), fill = .data$value)) +
    ggplot2::geom_tile(alpha = alpha) +
    ggplot2::coord_flip(expand = FALSE) +
    rcartocolor::scale_fill_carto_c(palette = col_palette) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"),
                   plot.background = ggplot2::element_rect(fill = "transparent"),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
                   legend.position = "none",
                   legend.key = ggplot2::element_blank())
  return(p)
}
