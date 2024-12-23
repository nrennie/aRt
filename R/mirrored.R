#' Mirrored
#'
#' This function generates a coloured generative art ggplot object from rectangles.
#'
#' @param n Number of boxes per quadrant. Default 15.
#' @param w Weighting towards first colour of palette. Minimum of 2. Default 4.
#' @param col_palette Vector of colours. Default `PrettyCols::prettycols("PurpleTangerines")`.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' mirrored()
#' @export

mirrored <- function(n = 15,
                     w = 4,
                     col_palette = PrettyCols::prettycols("PurpleTangerines"),
                     s = 1234) {
  w <- floor(w)
  if (w < 2) {
    stop("w must be at least 2.")
  }
  set.seed(s)
  tr <- expand.grid(x = 0:n, y = 0:n)
  n_col <- length(col_palette)
  tr$cols <- sample(seq_along(col_palette),
    size = (n + 1)^2,
    replace = TRUE,
    prob = c((w / (n_col + (w - 1))), rep(1 / (n_col + (w - 1)), n_col - 1))
  )
  tl <- tr |>
    dplyr::mutate(x = -1 * .data$x)
  br <- tr |>
    dplyr::mutate(y = -1 * .data$y)
  bl <- br |>
    dplyr::mutate(x = -1 * .data$x)
  plot_data <- rbind(tr, tl, br, bl) |>
    dplyr::distinct(.data$x, .data$y, .keep_all = TRUE) |>
    dplyr::mutate(
      xmin = .data$x - stats::runif(4 * ((n^2) + n) + 1, 0.3, 0.7),
      xmax = .data$x + stats::runif(4 * ((n^2) + n) + 1, 0.3, 0.7),
      ymin = .data$y - stats::runif(4 * ((n^2) + n) + 1, 0.3, 0.7),
      ymax = .data$y + stats::runif(4 * ((n^2) + n) + 1, 0.3, 0.7)
    )

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = plot_data,
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$cols
      ),
      alpha = 0.8,
      colour = "transparent"
    ) +
    ggplot2::scale_fill_gradientn(colours = col_palette) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme_aRt(col_palette[1])
  return(p)
}
