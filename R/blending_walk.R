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
