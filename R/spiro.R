#' Spiro
#'
#' This function generates a generative art ggplot object in a spirograph style.
#'
#' @param n_x Number of spirals per row.
#' @param n_y Number of spirals per column.
#' @param d Diameter.
#' @param R Outer radius.
#' @param r Inner radius.
#' @param col_palette Vector of colours. Default "white".
#' @param bg_col Background colour. Default "grey20".
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @export

spiro <- function(n_x = 10,
                  n_y = 10,
                  d = 10,
                  R = 4, # nolint
                  r = 1,
                  col_palette = "white",
                  bg_col = "grey20",
                  s = 1234) {
  set.seed(s)
  plot_data <- tibble::as_tibble(expand.grid(x = 1:n_x, y = 1:n_y)) |>
    dplyr::mutate(dplyr::across(c(.data$x, .data$y), ~ .x * d))
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
    )) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col)
    )
  return(p)
}
