#' Smudge
#'
#' This function generates a coloured generative art ggplot object from
#' contours.
#'
#' @param n Number of grid boxes. Default 25.
#' @param binwidth Binwidth for colours. Default 0.01.
#' @param col_palette Vector of colours. Default
#' `PrettyCols::prettycols("TangerineBlues")`.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' smudge()
#' @export

smudge <- function(n = 25,
                   binwidth = 0.01,
                   col_palette = PrettyCols::prettycols("TangerineBlues"),
                   s = 1234) {
  set.seed(s)
  new_palette <- grDevices::colorRampPalette(col_palette)(1 / binwidth)
  plot_data <- expand.grid(
    x = seq(1, n, 1),
    y = seq(1, n, 1)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(z = stats::runif(n^2))
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      z = .data$z
    )
  ) +
    ggplot2::geom_contour_filled(binwidth = binwidth) +
    ggplot2::scale_fill_manual(values = new_palette) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme_aRt("transparent")
  return(p)
}
