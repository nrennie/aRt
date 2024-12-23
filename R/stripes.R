#' Stripes
#'
#' This function generates a generative art ggplot object featuring rows of stripes.
#'
#' @param perc Percentage of data points to be sorted. Default 0.5.
#' @param n Number of rows. Default 3.
#' @param col_palette Vector of colours. Default `c("#8EA604", "#F5BB00", "#EC9F05", "#D76A03", "#BF3100")`.
#' @param alpha Transparency of fill. Default 1.
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @examples
#' stripes()
#' @export

stripes <- function(perc = 0.5,
                    n = 3,
                    col_palette = c("#8EA604", "#F5BB00", "#EC9F05", "#D76A03", "#BF3100"),
                    alpha = 1,
                    s = 1234) {
  if (perc < 0 || perc > 1) {
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
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$times,
      y = as.numeric(.data$name),
      fill = .data$value
    )
  ) +
    ggplot2::geom_tile(alpha = alpha) +
    ggplot2::coord_flip(expand = FALSE) +
    ggplot2::scale_fill_gradientn(colours = col_palette) +
    theme_aRt("transparent", -0.5)
  return(p)
}
