#' Sample colour
#'
#' Function to sample a colour with higher weight on last colour
#' @param col_palette Vector of colours.
#' @param weight Sampling weight. Default 0.25.
#' @return a vector of length 1
#' @noRd

sample_colour <- function(col_palette, weight = 0.25) {
  ncols <- length(col_palette)
  x <- sample(
    col_palette,
    size = 1,
    prob = c(rep(weight * 1 / ncols, ncols - 1), (1 - sum((weight * (ncols - 1)) / ncols)))
  )
  return(x)
}

#' Generate rectangle
#'
#' This function generates a rectangle which can be plotted with geom_polygon.
#'
#' @param x x-coordinate of bottom left corner
#' @param y y-coordinate of bottom left corner
#' @param height height of rectangle
#' @param width width of rectangle
#' @param group group to identify part of same rectangle. Default 1.
#' @param col_palette Vector of colours.
#' @param weight Sampling weight. Default 0.25.
#' @return a data frame
#' @noRd

generate_rectangle <- function(x, y, height, width,
                               group = 1,
                               col_palette, weight = 0.25) {
  data.frame(
    x = c(x, x + width, x + width, x),
    y = c(y, y, y + height, y + height),
    group = rep(group, 4),
    col = rep(sample_colour(col_palette = col_palette, weight = weight), 4)
  )
}


#' Rectangles
#'
#' This function generates a generative art ggplot object featuring multiple
#' coloured rectangles.
#'
#' @param n Number of rectangles. Default 100.
#' @param max_height Maximum height of rectangle. Default 7.
#' @param max_width Maximum width of rectangle. Default 5.
#' @param size Line width of rectangles. Default 2.
#' @param main_col Colour of non-highlighted rectangles. Default `"lightgrey"`.
#' @param col_palette Vector of colours. Default `"Bold"` colour palette from
#' rcartocolor.
#' @param bg_col Background colour. Default `"white"`.
#' @param weight Sampling weight. Default 0.25.
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @examples
#' rectangles()
#' @export

rectangles <- function(n = 100,
                       max_height = 7,
                       max_width = 5,
                       size = 2,
                       main_col = "lightgrey",
                       col_palette = rcartocolor::carto_pal(n = 12, "Bold"),
                       bg_col = "white",
                       weight = 0.25,
                       s = 1234) {
  if (max_height <= 2 || max_width <= 2) {
    stop("height and width should be > 2")
  }
  col_palette <- c(col_palette, main_col)
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      plot_data <- data.frame(
        x = c(),
        y = c(),
        group = c(),
        col = c()
      )
      for (i in 1:n) {
        k <- generate_rectangle(
          x = stats::runif(1, 0, 30),
          y = stats::runif(1, 0, 30),
          height = stats::runif(1, 2, max_height),
          width = stats::runif(1, 2, max_width),
          group = i,
          col_palette = col_palette,
          weight = weight
        )
        plot_data <- rbind(plot_data, k)
      }
      plot_data
    }
  )
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        group = .data$group,
        colour = .data$col
      ),
      fill = "transparent",
      size = size
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::xlim(
      (min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))
    ) +
    ggplot2::ylim(
      (min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))
    ) +
    theme_aRt(bg_col)
  return(p)
}
