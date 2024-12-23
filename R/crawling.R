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
#' @examples
#' crawling()
#' @export

crawling <- function(n = 50,
                     edge_colour = "black",
                     node_size = 1,
                     node_colour = "black",
                     bg_col = "white",
                     s = 1234) {
  graph_net <- withr::with_seed(
    seed = s,
    code = {
      x <- c(stats::rnorm(n, 25, 25), stats::rnorm(n, 50, 25), stats::rnorm(n, 75, 25))
      y <- c(stats::rnorm(n, 25, 25), stats::rnorm(n, 50, 25), stats::rnorm(n, 75, 25))
      d <- data.frame(x, y)
      dg <- stats::hclust(stats::dist(d))
      phylo_tree <- ape::as.phylo(dg)
      graph_edges <- phylo_tree$edge
      graph_net <- igraph::graph.edgelist(graph_edges)
      graph_net
    }
  )
  p <- ggraph::ggraph(graph_net, "igraph", algorithm = "tree", circular = TRUE) +
    ggraph::geom_edge_diagonal(colour = edge_colour) +
    ggraph::geom_node_point(
      ggplot2::aes(filter = igraph::degree(graph_net, mode = "out") == 0),
      color = node_colour, size = node_size
    ) +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col)
  return(p)
}
