#' Generates a random hex code
#'
#' @param n Number of hex codes to generate
#' @return Character string of hex codes
#' @noRd
random_hex <- function(n) {
  generate_hex <- function() {
    choices <- sample(c(as.character(0:9), LETTERS[1:6]), size = 6, replace = TRUE)
    output <- paste0("#", stringr::str_flatten(choices))
    return(output)
  }
  hex <- replicate(n = n, generate_hex(), simplify = TRUE)
  return(hex)
}

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
#' @param nx Number of columns. Default 5.
#' @param ny Number of rows. Default 5.
#' @param s Random seed. Default 1234.
#' @return Tibble
#' @examples
#' gradients()
#' @export

gradients <- function(nx = 5,
                      ny = 5,
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
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$xend,
        yend = .data$yend,
        colour = I(.data$colour)
      )
    ) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme_aRt("transparent")

}
