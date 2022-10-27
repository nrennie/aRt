#' Blending Walk
#'
#' This function generates a random walk.
#'
#' @param n Number of lines. Default 100.
#' @param s Seed value. Default 1234.
#' @return A numeric vector of length n
#' @noRd

blending_walk <- function(n = 100,
                          s = 1234) {
  x <- stats::runif(n = n - 1)
  x <- c(0, cumsum(x))
  return(x)
}


#' Blending
#'
#' This function generates a generative art ggplot object using a random walk.
#'
#' @param n Number of point. Default 100.
#' @param down Colour to use on bottom. Default "white".
#' @param up Colour to use on top. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'

blending <- function(n = 100,
                     down = "white",
                     up = "black",
                     s = 1234) {
  set.seed(s)
  walks <- unlist(purrr::map(.x = 1:n, .f = ~blending_walk(n = n, s = s + .x)))
  plot_data <- expand.grid(x = 1:n, y = 1:n)
  plot_data$colour <- walks
  ggplot2::ggplot(data = plot_data,
                  mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$colour)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::coord_flip(expand = FALSE) +
    ggplot2::scale_fill_gradient(low = down, high = up) +
    theme_aRt
}
