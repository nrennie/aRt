#' Static
#'
#' This function generates a greyscale generative art ggplot object.
#'
#' @param perc Percentage of data points to be non-NA. Default 0.1.
#' @param n Number of squares. Default 500.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

static <- function(perc = 0.1,
                   n = 500,
                   s = 1234) {
  if (perc < 0 || perc > 1) {
    stop("perc not between 0 and 1")
  }
  if (n < 1) {
    stop("n must be a positive integer")
  }
  set.seed(s)
  plot_df <- matrix(NA, ncol = n, nrow = n)
  colnames(plot_df) <- seq_len(ncol(plot_df))
  rownames(plot_df) <- seq_len(nrow(plot_df))
  plot_data <- tibble::tibble(times = seq_len(nrow(plot_df)), tibble::as_tibble(plot_df))
  plot_data <- tidyr::pivot_longer(plot_data, cols = 2:(ncol(plot_df) + 1))
  plot_data$value[sample(
    x = 1:(ncol(plot_df) * nrow(plot_df)),
    size = round(perc * ncol(plot_df) * nrow(plot_df)),
    replace = FALSE
  )] <- stats::runif(round(perc * ncol(plot_df) * nrow(plot_df)))
  p <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$times, y = .data$name, fill = .data$value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_fill_gradient(low = "gray27", high = "gray95", na.value = "gray27") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
      legend.position = "none",
      legend.key = ggplot2::element_blank()
    )
  return(p)
}
