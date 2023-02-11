#' Shatter
#'
#' This function generates a generative art ggplot object using polygons.
#'
#' @param n_x Number of polygons per row. Default 25.
#' @param n_y Number of polygons per column. Default 25.
#' @param decay Numeric between 0 and 1 specifying the rate of decay if square shapes. Default 0.8.
#' @param colour Single colour for fill colour of polygons. Default "black".
#' @param bg_col Single colour for background. Default "gray97".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'

shatter <- function(n_x = 25,
                    n_y = 25,
                    decay = 0.8,
                    colour = "black",
                    bg_col = "gray97",
                    s = 1234) {
  if (n_x < 1 || n_y < 1) {
    stop("Number of rows and columns must be at least 1")
  }
  if (decay < 0 || decay > 1) {
    stop("Decay must be between 0 and 1")
  }
  set.seed(s)
  n_x <- round(n_x)
  n_y <- round(n_y)

  x1 <- rep(1:n_x, times = n_y) + (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_y))
  x2 <- rep(2:(n_x + 1), times = n_y) - (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_y))
  x3 <- rep(2:(n_x + 1), times = n_y) - (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_y))
  x4 <- rep(1:n_x, times = n_y) + (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_y))
  x <- c(matrix(c(x1, x2, x3, x4), byrow = TRUE, nrow = 4, ncol = length(x1)))

  y1 <- rep(1:n_y, each = n_x) + (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_x))
  y2 <- rep(1:n_y, each = n_x) + (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_x))
  y3 <- rep(2:(n_y + 1), each = n_x) - (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_x))
  y4 <- rep(2:(n_y + 1), each = n_x) - (decay) * stats::runif(n_x * n_y, 0, rep((1:n_y) / n_y, each = n_x))
  y <- c(matrix(c(y1, y2, y4, y3), byrow = TRUE, nrow = 4, ncol = length(y1)))

  id <- rep(1:(n_x * n_y), each = 4)
  positions <- data.frame(x = x, y = y, id = id)
  values <- data.frame(id = unique(id))
  datapoly <- merge(values, positions, by = c("id"))
  # plot
  p <- ggplot2::ggplot(datapoly, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_polygon(
      ggplot2::aes(
        group = id,
        fill = y
      ),
      colour = NA
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_gradient(low = colour, high = bg_col) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  return(p)
}
