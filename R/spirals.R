#' Spirals
#'
#' This function generates a generative art ggplot object consisting of dots arranged in a spiral,
#'
#' @param perc Percentage of data points to be non-NA. Default 0.2.
#' @param s Seed value. Default 1234.
#' @importFrom ExtDist rBeta_ab
#' @return A ggplot object.
#' @export

spirals <- function(perc=0.2, s=1234){
  if(perc < 0 | perc > 1) stop('perc not between 0 and 1')
  set.seed(s)
  theta <- seq(0,20*pi,0.1)
  theta[sample(1:length(theta), size=round(perc*length(theta)), prob=1/((1:length(theta))^1.5))] <- NA
  r <- 0.5 + 0.5*theta
  df <- data.frame(x=r*cos(theta), y=r*sin(-theta))
  df$col_val <- runif(length(theta),-1,1)
  df$size_val <- rBeta_ab(n=length(theta), shape1 = 3, shape2 = 9, a = 0, b = 40)
  #bg noise
  bg_value <- 500
  df2 <- data.frame(x=runif(bg_value, -20,20), y=runif(bg_value, -20,20))
  #plot
  p <- ggplot() + geom_point(data=df, aes(x,y, col=col_val, size=size_val)) +
    geom_point(data=df2, aes(x,y), colour="white", size=0.1) +
    coord_fixed(expand=F) +
    xlim(-20,20) + ylim(-20,20) +
    scale_size(range=c(0, 12)) +
    theme(panel.background = element_rect(fill = "black", colour="black"),
          plot.background = element_rect(fill = "black", colour="black"),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          legend.position="none",
          plot.margin = unit(c(0, 0, 0, 0), "cm"), #top, right, bottom, left
          axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  p
}









