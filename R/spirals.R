#' Spirals
#'
#' This function generates a generative art ggplot object consisting of dots arranged in a spiral,
#'
#' @param perc Percentage of data points to be non-NA. Default 0.2.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

spirals <- function(perc=0.2, s=1234){
  if(perc < 0 | perc > 1) stop('perc not between 0 and 1')
  set.seed(s)
  theta <- seq(0,20*pi,0.1)
  theta[sample(1:length(theta), size=round(perc*length(theta)), prob=1/((1:length(theta))^1.5))] <- NA
  r <- 0.5 + 0.5*theta
  df <- data.frame(x=r*cos(theta), y=r*sin(-theta))
  df$col_val <- stats::runif(length(theta),-1,1)
  df$size_val <- ExtDist::rBeta_ab(n=length(theta), shape1 = 3, shape2 = 9, a = 0, b = 40)
  #bg noise
  bg_value <- 500
  df2 <- data.frame(x=stats::runif(bg_value, -20,20), y=stats::runif(bg_value, -20,20))
  #plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data=df, ggplot2::aes(x=.data$x, y=.data$y, col=.data$xcol_val, size=.data$xsize_val)) +
    ggplot2::geom_point(data=df2, aes(x=.data$x, y=.data$y), colour="white", size=0.1) +
    ggplot2::coord_fixed(expand=F) +
    ggplot2::xlim(-20,20) +
    ggplot2::ylim(-20,20) +
    ggplot2::scale_size(range=c(0, 12)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black", colour="black"),
          plot.background = ggplot2::element_rect(fill = "black", colour="black"),
          plot.title = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_blank(),
          legend.position="none",
          plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"), #top, right, bottom, left
          axis.title.x= ggplot2::element_blank(),
          axis.title.y= ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
    )
  suppressWarnings(print(p))
}









