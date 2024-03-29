#' Puzzles
#'
#' This function generates a coloured generative art ggplot object from treemaps.
#'
#' @param n Number of boxes. Default 200.
#' @param num_groups Number of larger boxes. Default 30.
#' @param col_palette Vector of colours. Default PrettyCols::prettycols("Beach").
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

puzzles <- function(n = 200,
                    num_groups = 30,
                    col_palette = PrettyCols::prettycols("Beach"),
                    bg_col = "white",
                    s = 1234) {
  if (num_groups > n) {
    stop("num_groups must be <= n")
  }
  set.seed(s)
  x <- stats::rexp(n, 0.02)
  plot_data <- tibble::tibble(id = 1:n, areas = x) |>
    dplyr::mutate(groups = factor(sample(seq_len(num_groups),
                                         size = n,
                                         replace = TRUE)),
                  fill_value = factor(sample(seq_along(col_palette),
                                             size = n,
                                             replace = TRUE)))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(area = .data$areas,
                                               fill = .data$fill_value,
                                               subgroup = .data$groups)) +
    treemapify::geom_treemap(colour = bg_col) +
    treemapify::geom_treemap_subgroup_border(colour = bg_col, size = 5) +
    ggplot2::scale_fill_manual(values = col_palette) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(5, 5, 5, 5),
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col)
    )
  return(p)

}
