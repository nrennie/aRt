#' Infinity
#'
#' This function generates a greyscale or rainbow coloured generative art ggplot object in the shape of an infinity symbol.
#'
#' @param n Number of lines per colour. Default 25.
#' @param col_scheme Colour scheme of art. One of c("mono", "rainbow). Default 0.1.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggplot2
#' @export


infinity <- function(n=25, col_scheme="mono", bg_col="black", s=1234){
  set.seed(s)
  #colour schemes
  if (col_scheme == "mono"){
    cols <- rev(c("gray100", "gray95", "gray90", "gray85", "gray80", "gray70", "gray60", "gray50", "gray15", "gray0"))
  } else if (col_scheme == "rainbow") {
    cols <- c("#700460", "#a02c5d", "#ec0f47", "#ee6b3b", "#fbbf54", "#abd96d", "#15c286", "#087353", "#045459", "4b0082")
  }
  #define times
  t1 <- seq(-0.5*pi, 1.5*pi, length=n*10)
  #find points
  df1 <- data.frame(x=cos(t1), y=sin(t1)*cos(t1))
  #randomise
  df1_random <- df1[sample(1:nrow(df1), size=nrow(df1), replace=F), ]
  df1_random$col_choice <- rep(cols, times=n)
  #plot
  p <- ggplot() +
    geom_path(data=df1_random, mapping=aes(x=x, y=y, colour=I(col_choice))) +
    xlim(-1,1) +
    ylim(-1,1) +
    coord_fixed() +
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
  p
}

