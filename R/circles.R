#' Circles
#'
#' This function generates a coloured generative art ggplot object using
#' dendograms and circular graphs.
#'
#' @param n Number of nodes. Default 10.
#' @param smoothness Smoothness of lines on circles. Default 100.
#' @param col_palette Vector of colours. Default "Bold" colour palette from
#' rcartocolor.
#' @param line_col Background colour. Default NA.
#' @param bg_col Background colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' circles()
#' @export

circles <- function(n = 100,
                    smoothness = 100,
                    col_palette = rcartocolor::carto_pal(n = 12, "Bold"),
                    line_col = NA,
                    bg_col = "black",
                    s = 1234) {
  # generate data
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
      graph_net <- igraph::set_vertex_attr(graph_net,
        name = "cols",
        index = igraph::V(graph_net),
        value = stats::runif(igraph::gorder(graph_net))
      )
      graph_net
    }
  )
  # plot
  p <- ggraph::ggraph(graph_net, "circlepack") +
    ggraph::geom_node_circle(
      mapping = ggplot2::aes(fill = .data$cols),
      size = 0.25,
      n = smoothness,
      colour = line_col
    ) +
    ggplot2::scale_fill_gradientn(colours = rev(col_palette)) +
    theme_aRt(bg_col)
  return(p)
}
