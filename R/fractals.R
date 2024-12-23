#' Fractals
#'
#' This function generates a generative art ggplot object using fractal patterns.
#' Inspired by https://www.r-bloggers.com/2010/08/fractals-in-r/
#'
#' @param N Number of iterations. Default 25.
#' @param col_palette Vector of colours. Default `c("#9b332b", "#b64f32", "#f7c267", "#b9b9b8", "#5d6174", "#41485f")`.
#' @param shift Offset of y-values. Default 0.
#' @param left Start range of x-axis. Default -1.
#' @param right End range of x-axis. Default 1.
#' @param y_param Rate of y growth. Default 3.
#' @param resolution Resolution of grid. Default 0.005.
#' @param dist_max Size of center area. Default 4.
#' @return A ggplot object.
#' @examples
#' fractals()
#' @export

fractals <- function(N = 25, # nolint
                     col_palette = c("#9b332b", "#b64f32", "#f7c267", "#b9b9b8", "#5d6174", "#41485f"),
                     shift = 0,
                     left = -1,
                     right = 1,
                     y_param = 3,
                     resolution = 0.005,
                     dist_max = 4) {
  # initialise
  t <- 0
  col_palette <- grDevices::colorRampPalette(col_palette)(N)
  num_colours <- length(col_palette)
  # create data
  step <- seq(left, right, by = resolution)
  output <- array(0, dim = c(length(step)^2, 3))
  for (i in step) {
    for (j in step + shift) {
      x <- 0
      y <- 0
      n <- 0
      dist <- 0
      while (all(c(n < N, dist < dist_max))) {
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
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      fill = col_palette[.data$col]
    )
  ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(expand = FALSE) +
    theme_aRt("transparent")
  return(p)
}
