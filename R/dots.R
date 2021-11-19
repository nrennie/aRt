#' Dots
#'
#' This function generates a coloured generative art ggplot object using polar coordinates.
#'
#' @param n_x Number of rotational points. Default 50.
#' @param n_y Number of outwards points. Default 100.
#' @param jitter_size_width Size of jitter width. Default 0.5.
#' @param jitter_size_height Size of jitter height. Default 0.5.
#' @param col_palette Colour palette from rcartocolor. Default "Purp".
#' @param bg_col Background colour. Default "#63589f".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot geom_jitter coord_polar theme element_rect element_blank unit aes
#' @import rcartocolor
#' @export
#'
#'

dots <- function(n_x=50, n_y=100, jitter_size_width=0.5, jitter_size_height=0.5, col_palette = "Purp", bg_col="#63589f", s=1234){
  x <- rep(1:n_x, times=n_y)
  y <- rep(1:n_y, each=n_x)
  plot_data <- data.frame(x=x, y=y)
  p <- ggplot(plot_data, aes(x=x, y=y, colour=y)) +
    geom_jitter(size=0.5, width = jitter_size_width, height=jitter_size_height) +
    scale_colour_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
    coord_polar() +
    theme(panel.background = element_rect(fill = bg_col, colour=bg_col),
          plot.background = element_rect(fill = bg_col, colour=bg_col),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          legend.position="none",
          plot.margin = unit(c(0,0,0,0), "cm"), #top, right, bottom, left
          axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          axis.text.x= element_blank(),
          axis.text.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.ticks.y= element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  p
}


