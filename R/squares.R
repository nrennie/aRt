#' Squares
#'
#' This function generates a generative art ggplot
#' object using pattern-filled squares
#'
#' @param n Number of squares per row. Default 7
#' @param line_col Colour of lines between squares. Default "white".
#' @param pattern_col Colour of pattern lines. Default "white".
#' @param pattern_fill Colour of pattern background. Default "black".
#' @param pattern_size Size of pattern. Default 0.4.
#' @param size Size of lines between squares. Default 1.5.
#' @param s Random seed. Default 1234.
#' @return A ggplot object.
#' @examples
#' squares()
#' @export

squares <- function(n = 7,
                    line_col = "white",
                    pattern_col = "white",
                    pattern_fill = "black",
                    pattern_size = 0.4,
                    size = 1.5,
                    s = 1234) {
  set.seed(s)
  df <- expand.grid(x = 1:n, y = 1:n)
  plot_data <- df |>
    dplyr::mutate(
      fill = factor(sample(1:4, size = nrow(df), replace = TRUE)),
      angle = sample(c(30, 60, 90), size = nrow(df), replace = TRUE)
    )
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      pattern = .data$fill,
      pattern_angle = .data$angle
    )
  ) +
    ggpattern::geom_tile_pattern(
      colour = line_col,
      pattern_colour = pattern_col,
      pattern_fill = pattern_fill,
      pattern_size = pattern_size,
      size = size
    ) +
    ggpattern::scale_pattern_manual(values = c(
      "stripe",
      "wave",
      "crosshatch",
      "weave"
    )) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme_aRt(line_col, 0.3)
  return(p)
}
