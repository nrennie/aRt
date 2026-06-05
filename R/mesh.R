#' Mesh
#'
#' This function generates a coloured generative art ggplot object using rectangles.
#'
#' @param n_x Number of squares per row. Default 10.
#' @param n_y Number of squares per column. Default 10.
#' @param col_palette Vector of colours. Default
#' `c("#0e7c7b", "#17bebb", "#d4f4dd", "#d62246", "#4b1d3f")`.
#' @param linewidth Width of borders around squares. Default 2.
#' @param rayshade Boolean determining whether the returned plot should be
#' converted to three dimensional using rayshader. If `TRUE`, `{rayshader}`
#' is required to be installed. Default `FALSE`.
#' @return A ggplot object.
#' @examples
#' mesh()
#' @export

mesh <- function(n_x = 10,
                 n_y = 10,
                 col_palette = c("#0e7c7b", "#17bebb", "#d4f4dd", "#d62246", "#4b1d3f"),
                 linewidth = 2,
                 rayshade = FALSE) {
  plot_data <- expand.grid(
    x = 1:n_x,
    y = 1:n_y
  )
  g <- ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        fill = .data$x,
        colour = .data$y
      ),
      linewidth = linewidth
    ) +
    ggplot2::scale_fill_gradientn(colours = col_palette) +
    ggplot2::scale_colour_gradientn(colours = col_palette) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme_aRt(col_palette[1])
  if (rayshade) {
    if (!requireNamespace("rayshader", quietly = TRUE)) {
      stop("Please install {rayshader} to use this argument, or set 'rayshade = FALSE'")
    } else {
      rayshader::plot_gg(g,
        height_aes = "colour",
        fov = 0,
        theta = 0,
        phi = 90,
        preview = TRUE
      )
    }
  } else {
    return(g)
  }
}
