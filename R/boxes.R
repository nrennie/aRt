#' Boxes
#'
#' This function generates a coloured generative art ggplot object from
#' treemaps.
#'
#' @param n Number of boxes
#' @param perc Relationship between box size and colour. Value between 0 and 1
#' where 0 represents randomness and 1 perfect identical. Default 0.1.
#' @param col_palette Vector of colours. Default "DarkMint" colour palette from
#' rcartocolor.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import patchwork
#' @examples
#' boxes()
#' @export

boxes <- function(n = 100,
                  perc = 0.1,
                  col_palette = rcartocolor::carto_pal(n = 7, "DarkMint"),
                  bg_col = "black",
                  s = 1234) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      x <- stats::rexp(n, 0.02)
      y <- perc * x + (1 - perc) * stats::runif(n, 1, 60)
      plot_data <- tibble::tibble(id = 1:n, areas = x, values = y)
      plot_data
    }
  )
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(area = .data$areas, fill = .data$values)
  ) +
    treemapify::geom_treemap(alpha = 0.5, colour = NA) +
    ggplot2::scale_fill_gradientn(colours = rev(col_palette)) +
    ggplot2::scale_colour_gradientn(colours = rev(col_palette)) +
    theme_aRt(bg_col)
  p1 <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(area = .data$areas, fill = .data$values)
  ) +
    treemapify::geom_treemap(alpha = 0.5, colour = NA) +
    ggplot2::scale_fill_gradientn(colours = rev(col_palette)) +
    ggplot2::scale_colour_gradientn(colours = rev(col_palette)) +
    theme_aRt("transparent")
  q <- p +
    inset_element(p1, left = 0.1, bottom = 0.1, right = 1.1, top = 1.1) +
    inset_element(p1, left = -0.1, bottom = -0.1, right = 0.9, top = 0.9) &
    theme(plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm"))
  return(q)
}
