#' Fractals
#'
#' This function generates a generative art ggplot
#' object using fractal patterns. Inspired by https://www.r-bloggers.com/2010/08/fractals-in-r/
#'
#' @param N Number of iterations. Default 25.
#' @param col_palette Vector of colours of length >= N. Default MetBrewer::met.brewer("Demuth", n = 25).
#' @param shift Offset of y-values. Default 0.
#' @param left Start rangle of x-axis. Default -1.
#' @param right End range of x-axis. Default 1.
#' @param y_param Rate of y growth. Default 3.
#' @param resolution Resolution of grid. Default 0.005.
#' @param dist_max Size of center area. Default 4.
#' @return A ggplot object.
#' @export
#'

fractals <- function(N = 25, #nolint
                     col_palette = MetBrewer::met.brewer("Demuth", n = 25),
                     shift = 0,
                     left = -1,
                     right = 1,
                     y_param = 3,
                     resolution = 0.005,
                     dist_max = 4) {
  # initialise
  t <- 0
  num_colours <- length(col_palette)
  if (num_colours < N) {
    stop("Number of colours in col_palette must be greater than or equal to N")
  }
  # create data
  step <- seq(left, right, by = resolution)
  output <- array(0, dim = c(length(step) ^ 2, 3))
  for (i in step) {
    for (j in step + shift) {
      x <- 0
      y <- 0
      n <- 0
      dist <- 0
      while (n < N & dist < dist_max) {
        n <- n + 1
        x1 <- i + x^2 - y^2
        y1 <- j + (y_param * x * y)
        dist <- x1^2 + y1^2
        x <- x1
        y <- y1
      }

      if (dist < dist_max) {
        col <- num_colours
      } else {
        col <- n * floor(num_colours   / N)
      }

      t <- t + 1

      output[t, ] <- c(i, j, col)
    }
  }
  plot_data <- as.data.frame(output)
  colnames(plot_data) <- c("y", "x", "col")
  # plot
  ggplot2::ggplot(data = plot_data,
                  mapping = ggplot2::aes(x = .data$x,
                                         y = .data$y,
                                         fill = I(col_palette[col]))) +
    ggplot2::geom_raster() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
}
