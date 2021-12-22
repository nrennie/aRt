#' Tiles
#'
#' This function generates a coloured generative art ggplot object using square polygons.
#'
#' @param n_x Number of polygons per row. Default 12.
#' @param n_y Number of polygons per column. Default 18.
#' @param gap_size Numeric between 0 and 1 specifying the size of the gap in the polygons. Default 0.5.
#' @param col_palette Colour palette from MetBrewer.
#' @param num_colours Number of colours to use.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggplot2
#' @import MetBrewer
#' @export
#'

tiles <- function(n_x=12, n_y=12, col_palette="Veronese", num_colours=5, s=1234){
  if (n_x < 1 | n_y < 1) {stop("Number of rows and columns must be at least 1")}
  set.seed(s)
  n_x <- round(n_x)
  n_y <- round(n_y)
  #generate data for large polygons
  x1 <- rep(1:n_x, times=n_y)
  x2 <- rep(2:(n_x+1), times=n_y)
  x3 <- rep(2:(n_x+1), times=n_y)
  x4 <- rep(1:n_x, times=n_y)
  x <- c(matrix(c(x1, x2, x3, x4), byrow=T, nrow=4, ncol=length(x1)))
  y1 <- rep(1:n_y, each=n_x)
  y2 <- rep(1:n_y, each=n_x)
  y3 <- rep(2:(n_y+1), each=n_x)
  y4 <- rep(2:(n_y+1), each=n_x)
  y <- c(matrix(c(y1, y2, y4, y3), byrow=T, nrow=4, ncol=length(y1)))
  id <- rep(1:(n_x*n_y), each=4)
  positions <- data.frame(x=x, y=y, id=id)
  values <- data.frame(id=unique(id), cols=sample(1:num_colours, size=n_x*n_y, replace=T))
  datapoly <- merge(values, positions, by = c("id"))
  #middle values
  x1 <- rep(1:n_x, times=n_y) + (0.5/3)
  x2 <- rep(2:(n_x+1), times=n_y) - (0.5/3)
  x3 <- rep(2:(n_x+1), times=n_y) - (0.5/3)
  x4 <- rep(1:n_x, times=n_y) + (0.5/3)
  x <- c(matrix(c(x1, x2, x3, x4), byrow=T, nrow=4, ncol=length(x1)))
  y1 <- rep(1:n_y, each=n_x) + (0.5/3)
  y2 <- rep(1:n_y, each=n_x) + (0.5/3)
  y3 <- rep(2:(n_y+1), each=n_x) - (0.5/3)
  y4 <- rep(2:(n_y+1), each=n_x) - (0.5/3)
  y <- c(matrix(c(y1, y2, y4, y3), byrow=T, nrow=4, ncol=length(y1)))
  id <- rep(1:(n_x*n_y), each=4)
  positions <- data.frame(x=x, y=y, id=id)
  values <- data.frame(id=unique(id), cols=sample(1:num_colours, size=n_x*n_y, replace=T))
  datapoly2 <- merge(values, positions, by = c("id"))
  #small values
  x1 <- rep(1:n_x, times=n_y) + (1/3)
  x2 <- rep(2:(n_x+1), times=n_y) - (1/3)
  x3 <- rep(2:(n_x+1), times=n_y) - (1/3)
  x4 <- rep(1:n_x, times=n_y) + (1/3)
  x <- c(matrix(c(x1, x2, x3, x4), byrow=T, nrow=4, ncol=length(x1)))
  y1 <- rep(1:n_y, each=n_x) + (1/3)
  y2 <- rep(1:n_y, each=n_x) + (1/3)
  y3 <- rep(2:(n_y+1), each=n_x) - (1/3)
  y4 <- rep(2:(n_y+1), each=n_x) - (1/3)
  y <- c(matrix(c(y1, y2, y4, y3), byrow=T, nrow=4, ncol=length(y1)))
  id <- rep(1:(n_x*n_y), each=4)
  positions <- data.frame(x=x, y=y, id=id)
  values <- data.frame(id=unique(id), cols=sample(1:num_colours, size=n_x*n_y, replace=T))
  datapoly3 <- merge(values, positions, by = c("id"))
  #make colours unique #add colours here instead
  for (i in 1:nrow(datapoly)){
    cols_i <- c(datapoly$cols[i], datapoly2$cols[i], datapoly3$cols[i])
    if (length(unique(cols_i)) != 3) {
      new_cols <- sample(1:num_colours, size=3, replace=F)
      datapoly$cols[i] = new_cols[1]
      datapoly2$cols[i] = new_cols[2]
      datapoly3$cols[i] = new_cols[3]
    }
  }
  #plot
  p <- ggplot() +
    geom_polygon(data=datapoly, mapping=aes(x = x, y = y, group = id, fill=as.character(cols)), colour=NA) +
    geom_polygon(data=datapoly2, mapping=aes(x = x, y = y, group = id, fill=as.character(cols)), colour=NA) +
    geom_polygon(data=datapoly3, mapping=aes(x = x, y = y, group = id, fill=as.character(cols)), colour=NA) +
    scale_fill_manual(values=met.brewer(col_palette, num_colours)) +
    coord_fixed(expand=F, xlim=c(1,n_x), ylim=c(1,n_y)) +
    theme(panel.background = element_rect(fill = NA, colour=NA),
          plot.background = element_rect(fill = NA, colour=NA),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          legend.position="none",
          plot.margin = unit(c(0,0,-0.1,-0.1), "cm"), #top, right, bottom, left
          axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          axis.text.x= element_blank(),
          axis.text.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.ticks.y= element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  p
}