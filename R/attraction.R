#' Attraction
#'
#' This function generates a generative art ggplot
#' object using strange attractors.
#'
#' @param n Number of points. Default 50000.
#' @param a Parameter. Default -3.
#' @param b Parameter. Default 1.
#' @param c Parameter. Default 0.5.
#' @param d Parameter. Default -1.
#' @param main_col Colour of lines. Either a single
#' colour or one of the colour palettes from rcartocolor.
#' Default "black".
#' @param bg_col Background colour. Default "white".
#' @return A ggplot object.
#' @importFrom ggplot2 geom_point theme element_rect element_blank unit aes
#' @import rcartocolor
#' @export
#'

attraction <- function(n=50000, a=-3, b=1, c=0.5, d=-1, main_col="black", bg_col="white"){
  x <- numeric(length=n)
  y <- numeric(length=n)
  x[1] <- 0
  y[1] <- 0
  for (i in 2:n) {
    x[i] = sin(a*y[i-1])*sin(b*x[i-1]) + c*(cos(a*x[i-1])^2)
    y[i] = (sin(b*x[i-1]))^2 + d*cos(b*y[i-1])*cos(a*x[i-1])
  }
  df <- data.frame(t=1:n, x=x, y=y)
  all_palettes <- rcartocolor::cartocolors$Name
  if (main_col %in% all_palettes){
    p <- ggplot(df, aes(x=x, y=y)) +
      geom_point(aes(colour=t), shape=20, alpha=0.2) +
      rcartocolor::scale_color_carto_c(palette = main_col, limits=c(1,n)) +
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
  } else{
    p <- ggplot(df, aes(x=x, y=y)) +
      geom_point(colour=main_col, shape=20, alpha=0.1) +
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
  }
  p
}



