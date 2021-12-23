#' Crawling
#'
#' This function generates a generative art ggplot object using dendograms.
#'
#' @param n Number of nodes.
#' @param edge_colour Edge colour. Default "black".
#' @param node_size Node size. Default 1.
#' @param node_colour Node colour. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_point
#' @importFrom ggplot2 coord_fixed element_rect element_blank unit
#' @importFrom igraph degree graph.edgelist
#' @export
#'

crawling <- function(n=50, edge_colour="black", node_size=1, node_colour="black", bg_col="white", s=1234){
  set.seed(s)
  x <- c(stats::rnorm(n, 25, 25), stats::rnorm(n, 50, 25), stats::rnorm(n, 75, 25))
  y <- c(stats::rnorm(n, 25, 25), stats::rnorm(n, 50, 25), stats::rnorm(n, 75, 25))
  d <- data.frame(x,y)
  dg <- stats::hclust(stats::dist(d))
  phylo_tree = ape::as.phylo(dg)
  graph_edges = phylo_tree$edge
  graph_net = graph.edgelist(graph_edges)
  p <- ggraph(graph_net, 'igraph', algorithm = 'tree', circular = T) +
    geom_edge_diagonal(colour=edge_colour) +
    coord_fixed() +
    geom_node_point(aes(filter = degree(graph_net, mode = 'out') == 0),
                    color = node_colour, size = node_size) +
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



