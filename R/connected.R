#' Connected
#'
#' This function generates a generative art ggplot object connecting points through arcs on a circle.
#'
#' @param n Number of lines to start. Default 100.
#' @param n_geom Number of points along path to create. Default 2.
#' @param random Boolean value for whether to randomise plotting order of edges.
#' @param col_palette Colour palette from RColorBrewer. Default "RdPu".
#' @param bg_col Background colour. Default "#ae217e".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggplot2
#' @import ggforce
#' @import ggraph
#' @import rcartocolor
#' @export
#'

connected <- function(n=100, n_geom=2, random=F, col_palette="RdPu", bg_col="#ae217e", s=1234){
  #check values
  n <- round(n)
  n_geom <- round(n_geom)
  if (n < 1) {stop("n must be >= 1")}
  if (n_geom < 2) {stop("n_geom must be >= 2")}
  n1 <- round(0.5*n)
  n2 <- round(0.75*n)
  set.seed(s)
  l1 <- 1:n
  l2 <- sample((n+1):(n+n1), size=n, replace=T)
  l3 <- sample((n+n1+1):(n+n2), size=n1, replace=T)
  l4 <- rep((n+n2+1), n)
  d <- data.frame(from=c(l1,l2,l3), to=c(l2,l3,l4))
  if (random==T){
    d <- d[sample(1:nrow(d), size=nrow(d), replace = F),]
  }
  g1 <- igraph::graph_from_data_frame(d)
  g2 <- g1 %>%
    igraph::set_edge_attr("e_val", value = sort(rnorm(gsize(g1))))
  p <- ggraph(g2, layout = 'linear', circular = TRUE) +
    geom_edge_arc(aes(colour=e_val), n=n_geom) +
    coord_fixed() +
    scale_edge_colour_distiller(palette=col_palette) +
    theme(panel.background = element_rect(fill = bg_col, colour=bg_col),
          plot.background = element_rect(fill = bg_col, colour=bg_col),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          legend.position="none",
          plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"), #top, right, bottom, left
          axis.title.x= element_blank(),
          axis.title.y= element_blank(),
          axis.text.x= element_blank(),
          axis.text.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.ticks.y= element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  return(p)
}


