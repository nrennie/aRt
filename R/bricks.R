#' Brick size
#'
#' This function generates a vector of length n which adds to 100.
#'
#' @param n Length of vector to output.
#' @return A factor vector of length n, containing bricks sizes.
#' @noRd
#'

brick_size <- function(n) {
  size <- 100
  r <- sample(1:n)
  o1 <- size * (r / sum(r))
  output <- round(o1)
  if (sum(output) < size) {
    output[1] <- output[1] + (size - sum(output))
  }
  if (sum(output) > size) {
    output[1] <- output[1] - (size - sum(output))
  }
  return(output)
}


#' Bricks
#'
#' This function generates a coloured generative art ggplot object using polygons.
#'
#' @param n_y Number of rows. Default 20.
#' @param colours Vector of colours. Can be any length. Default c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C").
#' @param bg_col Background colour. Default "gray97".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'

bricks <- function(n_y = 20, colours = c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C"), bg_col = "gray97", s = 1234) {
  set.seed(s)
  n_y <- round(n_y)
  total_bricks <- 0
  plot_data <- data.frame(id = c(),
                          cols = c(),
                          x = c(),
                          y = c())
  for (i in 1:n_y) {
    num_bricks <- sample(2:8, size = 1)
    bs <- brick_size(num_bricks)
    x1 <- c(0, cumsum(bs)[1:(num_bricks - 1)]) + 0.25
    x2 <- c(cumsum(bs)[1:(num_bricks - 1)], 100) - 0.25
    x3 <- c(cumsum(bs)[1:(num_bricks - 1)], 100) - 0.25
    x4 <- c(0, cumsum(bs)[1:(num_bricks - 1)]) + 0.25
    y1 <- rep(5 * i + 0.1, num_bricks)
    y2 <- rep(5 * i + 0.1, num_bricks)
    y3 <- rep(5 * i + 4.9, num_bricks)
    y4 <- rep(5 * i + 4.9, num_bricks)
    x <- c(matrix(c(x1, x2, x3, x4), byrow = TRUE, nrow = 4, ncol = length(x1)))
    y <- c(matrix(c(y1, y2, y4, y3), byrow = TRUE, nrow = 4, ncol = length(y1)))
    id <- rep((total_bricks + 1):(total_bricks + num_bricks), each = 4)
    total_bricks <- total_bricks + num_bricks
    positions <- data.frame(x = x, y = y, id = id)
    values <- data.frame(id = unique(id), cols = n_col_select(n = length(colours), size = num_bricks, random = TRUE, s = s))
    datapoly <- merge(values, positions, by = c("id"))
    plot_data <- rbind(plot_data, datapoly)
  }
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_polygon(ggplot2::aes(group = .data$id, fill = .data$cols), colour = NA) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          plot.title = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_blank(),
          legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # top, right, bottom, left
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())
  p
}
