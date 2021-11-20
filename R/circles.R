#' Circles
#'
#' This function generates a coloured generative art ggplot object using dendograms and circular graphs.
#'
#' @param n Number of nodes. Default 10.
#' @param smoothness Smoothness of lines on circles. Default 100.
#' @param col_palette Colour palette from rcartocolor. Default "Bold".
#' @param line_col Background colour. Default NA.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @importFrom ggplot2 coord_fixed theme element_rect element_blank unit aes
#' @importFrom ggraph ggraph  geom_edge_arc
#' @importFrom igraph graph.edgelist set_vertex_attr V gorder
#' @importFrom ape as.phylo
#' @import rcartocolor
#' @export
#'

circles <- function(n=100, smoothness=100, col_palette="Bold", line_col=NA, bg_col="black", s=1234){
  set.seed(s)
  #generate data
  x <- c(rnorm(n, 25, 25), rnorm(n, 50, 25), rnorm(n, 75, 25))
  y <- c(rnorm(n, 25, 25), rnorm(n, 50, 25), rnorm(n, 75, 25))
  d <- data.frame(x,y)
  dg <- hclust(dist(d))
  phylo_tree = as.phylo(dg)
  graph_edges = phylo_tree$edge
  graph_net = graph.edgelist(graph_edges)
  graph_net = set_vertex_attr(graph_net, name="cols", index = V(graph_net), value=runif(gorder(graph_net)))
  #plot
  p <- ggraph(graph_net, 'circlepack') +
    geom_node_circle(aes(fill=cols), size = 0.25, n=smoothness, colour=line_col) +
    scale_fill_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
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
  p
}

