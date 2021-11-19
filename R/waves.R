#' Waves
#'
#' This function generates a generative art ggplot object from sine and cosine waves.
#'
#' @param a sine wave parameter. Default 23.
#' @param b Cosine wave parameter. Default 6.
#' @param main_col Colour of lines. Either a single colour or one of the colour palettes from rcartocolor. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 2021.
#' @return A ggplot object.
#' @import ggplot2
#' @import rcartocolor
#' @export

waves <- function(a=23, b=6, main_col="black", bg_col="white", s=2021){
  set.seed(s)
  x <- seq(0,50*pi,0.01)
  y <- sample(1:8, size=1)*sin(a*x) + sample(1:8, size=1)*cos(b*x)
  df <- data.frame(x=x, y=y)
  #check if colour palette used
  all_palettes <- rcartocolor::cartocolors$Name
  if (main_col %in% all_palettes){
    p <- ggplot(df, aes(x=x,y=y)) +
      geom_path(aes(colour=y)) +
      scale_color_carto_c(type = "diverging", palette = main_col, direction = -1) +
      coord_polar() +
      theme(panel.background = element_rect(fill = bg_col, colour=bg_col),
            plot.background = element_rect(fill = bg_col, colour=bg_col),
            plot.title = element_blank(),
            plot.subtitle = element_blank(),
            legend.position="none",
            plot.margin = unit(c(0, 0, 0, 0), "cm"), #top, right, bottom, left
            axis.title.x= element_blank(),
            axis.title.y= element_blank(),
            axis.text.x= element_blank(),
            axis.text.y= element_blank(),
            axis.ticks.x= element_blank(),
            axis.ticks.y= element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
      )
  } else{
    p <- ggplot(df, aes(x=x,y=y)) +
      geom_path(colour=main_col) +
      coord_polar() +
      theme(panel.background = element_rect(fill = bg_col, colour=bg_col),
            plot.background = element_rect(fill = bg_col, colour=bg_col),
            plot.title = element_blank(),
            plot.subtitle = element_blank(),
            legend.position="none",
            plot.margin = unit(c(0, 0, 0, 0), "cm"), #top, right, bottom, left
            axis.title.x= element_blank(),
            axis.title.y= element_blank(),
            axis.text.x= element_blank(),
            axis.text.y= element_blank(),
            axis.ticks.x= element_blank(),
            axis.ticks.y= element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
      )
  }
  p
}


