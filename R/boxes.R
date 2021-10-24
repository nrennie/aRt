#' Boxes
#'
#' This function generates a coloured generative art ggplot object from treemaps.
#'
#' @param n Number of boxes
#' @param perc Relationship between box size and colour. Value between 0 and 1 where 0 represents randomness and 1 perfect identical. Default 0.1.
#' @param col_palette Colour palette from rcartocolor. Default "DarkMint".
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggplot2
#' @import rcartocolor
#' @import treemapify
#' @import tibble
#' @import dplyr
#' @import patchwork
#' @export

boxes <- function(n=100, perc=0.1,  col_palette="DarkMint", bg_col="black", s=1234){
  set.seed(s)
  x <- rexp(n, 0.02)
  y <- perc*x + (1-perc)*runif(n, 1, 60)
  plot_data <- tibble(id = 1:n, areas = x, values = y)
  #make plots
  p <- ggplot(plot_data, aes(area = areas, fill = values)) +
    geom_treemap(alpha = 0.5,colour=NA) +
    scale_fill_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
    scale_colour_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
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
          panel.grid.minor = element_blank()
    )
  p1 <- ggplot(plot_data, aes(area = areas, fill = values)) +
    geom_treemap(alpha = 0.5,colour=NA) +
    scale_fill_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
    scale_colour_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
    theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
          plot.background = element_rect(fill = "transparent", colour="transparent"),
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
          panel.grid.minor = element_blank()
    )
  q <- p +
    inset_element(p1, left = 0.1, bottom = 0.1, right = 1.1, top = 1.1) +
    inset_element(p1, left = -0.1, bottom = -0.1, right = 0.9, top = 0.9) &
    theme(plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"))
  q
}
