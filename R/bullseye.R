#' Bullseye
#'
#' This function generates a layered generative art ggplot object using polar bar charts.
#'
#' @param main_col Colour scheme of art. One of c("mono", "rainbow). Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggplot2
#' @import patchwork
#' @export

bullseye <- function(main_col="black", bg_col="white", s=1234){
  #generate data
  set.seed(s)
  df1 <- data.frame(id=seq(1,20), value=sample(seq(-20,100), 20, replace=T))
  df2 <- data.frame(id=seq(1,20), value=sample(seq(-20,100), 20, replace=T))
  df3 <- data.frame(id=seq(1,40), value=sample(seq(-20,100), 40, replace=T))
  df4 <- data.frame(id=seq(1,40), value=sample(seq(-20,100), 40, replace=T))
  df5 <- data.frame(id=seq(1,60), value=sample(seq(-20,100), 60, replace=T))
  df6 <- data.frame(id=seq(1,60), value=sample(seq(-20,100), 60, replace=T))
  df7 <- data.frame(id=seq(1,80), value=sample(seq(-20,100), 80, replace=T))
  df8 <- data.frame(id=seq(1,80), value=sample(seq(-20,100), 80, replace=T))
  #make plot layers
  p1 <- ggplot() +
    geom_bar(data=df1, mapping=aes(x=as.factor(id), y=value), stat="identity", width=1, fill=alpha(main_col, 0.3)) +
    geom_bar(data=df2, mapping=aes(x=as.factor(id), y=value), stat="identity", width=0.2, fill=alpha(main_col, 0.3)) +
    ylim(-20,100) +
    coord_polar(start = 0) +
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
  p2 <- ggplot() +
    geom_bar(data=df3, mapping=aes(x=as.factor(id), y=value), stat="identity", width=1, fill=alpha(main_col, 0.3)) +
    geom_bar(data=df4, mapping=aes(x=as.factor(id), y=value), stat="identity", width=0.2, fill=alpha(main_col, 0.3)) +
    ylim(-30,100) +
    coord_polar(start = 45) +
    theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
          plot.background = element_rect(fill = "transparent", colour="transparent"),
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
  p3 <- ggplot() +
    geom_bar(data=df5, mapping=aes(x=as.factor(id), y=value), stat="identity", width=1, fill=alpha(main_col, 0.3)) +
    geom_bar(data=df6, mapping=aes(x=as.factor(id), y=value), stat="identity", width=0.2, fill=alpha(main_col, 0.3)) +
    ylim(-40,100) +
    coord_polar(start = 90) +
    theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
          plot.background = element_rect(fill = "transparent", colour="transparent"),
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
  p4 <- ggplot() +
    geom_bar(data=df7, mapping=aes(x=as.factor(id), y=value), stat="identity", width=1, fill=alpha(main_col, 0.3)) +
    geom_bar(data=df8, mapping=aes(x=as.factor(id), y=value), stat="identity", width=0.2, fill=alpha(main_col, 0.4)) +
    ylim(-50,100) +
    coord_polar(start = 135) +
    theme(panel.background = element_rect(fill = "transparent", colour="transparent"),
          plot.background = element_rect(fill = "transparent", colour="transparent"),
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
  #join plots
  p <- p1 +
    inset_element(p2, left = 0, bottom = 0, right = 1, top = 1) +
    inset_element(p3, left = 0, bottom = 0, right = 1, top = 1) +
    inset_element(p4, left = 0, bottom = 0, right = 1, top = 1)
  p
}

