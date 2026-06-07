#' Generates a tibble of line segments and colours
#'
#' @param xmin x-min coord
#' @param xmax x-max coord
#' @param ymin y-min coord
#' @param ymax y-max coord
#' @param low Hex value for left colour
#' @param high Hex value for right colour
#' @return Tibble
#' @noRd
make_square <- function(xmin, xmax, ymin, ymax, low, high) {
  xvals <- seq(xmin, xmax, by = 0.001)
  plot_data <- tibble::tibble(
    x = xvals,
    xend = xvals,
    y = ymin,
    yend = ymax,
    colour = grDevices::colorRampPalette(c(low, high))(length(xvals))
  )
  return(plot_data)
}

#' Generates generative art as a grid of gradient colour fades
#'
#' @param nx Number of columns. Default 4.
#' @param ny Number of rows. Default 4.
#' @param bg_col Background colour. Default "white".
#' @param linewidth Linewidth. Default 1.5.
#' @param s Random seed. Default 1234.
#' @return Tibble
#' @examples
#' gradients()
#' @export
gradients <- function(nx = 4,
                      ny = 4,
                      bg_col = "white",
                      linewidth = 1.5,
                      s = 1234) {
  inputs <- withr::with_seed(
    seed = s,
    code = {
      inputs <- tibble::tibble(
        xmin = rep(seq_len(nx), each = ny),
        xmax = rep(seq_len(nx) + 1, each = ny),
        ymin = rep(seq_len(ny), times = nx),
        ymax = rep(seq_len(ny) + 1, times = nx),
        low = random_hex(nx * ny),
        high = random_hex(nx * ny)
      )
      inputs
    }
  )

  plot_data <- purrr::pmap_df(inputs, make_square)
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$xend,
        yend = .data$yend,
        colour = I(.data$colour)
      )
    ) +
    ggplot2::geom_tile(
      data = expand.grid(x = 0.5 + 1:nx, y = 0.5 + 1:ny),
      mapping = ggplot2::aes(x = .data$x, .data$y),
      colour = bg_col,
      linewidth = linewidth,
      fill = "transparent"
    ) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme_aRt(bg_col, 0.5)
  return(p)
}
