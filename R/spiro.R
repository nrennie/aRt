#' Spiro
#'
#' This function generates a generative art ggplot object in a spirograph style.
#'
#' @param n_x Number of spirals per row. Default 10.
#' @param n_y Number of spirals per column. Default 10.
#' @param d Diameter. Default 10.
#' @param R Outer radius. Default 4.
#' @param r Inner radius. Default 1.
#' @param linewidth Width on lines. Default 0.5.
#' @param col_palette Vector of colours. Default "white".
#' @param bg_col Background colour. Default "grey20".
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @examples
#' spiro()
#' @export

spiro <- function(n_x = 10,
                  n_y = 10,
                  d = 10,
                  R = 4, # nolint
                  r = 1,
                  linewidth = 0.5,
                  col_palette = "white",
                  bg_col = "grey20",
                  s = 1234) {
  set.seed(s)
  plot_data <- tibble::as_tibble(expand.grid(x = 1:n_x, y = 1:n_y)) |>
    dplyr::mutate(dplyr::across(c("x", "y"), ~ .x * d))
  plot_data$cols <- sample(col_palette, size = nrow(plot_data), replace = TRUE)
  p <- ggplot2::ggplot(data = plot_data) +
    ggforce::geom_spiro(ggplot2::aes(
      R = R,
      r = 1,
      d = d,
      x0 = .data$x,
      y0 = .data$y,
      colour = .data$cols,
      outer = TRUE
    ),
    size = linewidth) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col)
  return(p)
}
