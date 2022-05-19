#' Connected
#'
#' This function generates a generative art ggplot object connecting points through arcs on a circle.
#'
#' @param n Number of lines to start. Default 100.
#' @param n_geom Number of points along path to create. Default 2.
#' @param random Boolean value for whether to randomise plotting order of edges.
#' @param col_palette Vector of colours. Default "RdPu" colour palette from RColorBrewer.
#' @param bg_col Background colour. Default "#ae217e".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'

connected <- function(n = 100,
                      n_geom = 2,
                      random = FALSE,
                      col_palette = RColorBrewer::brewer.pal(n = 9,"RdPu"),
                      bg_col = "#ae217e",
                      s = 1234) {
  n <- round(n)
  n_geom <- round(n_geom)
  if (n < 1) {
    stop("n must be >= 1")
    }
  if (n_geom < 2) {
    stop("n_geom must be >= 2")
    }
  n1 <- round(0.5 * n)
  n2 <- round(0.75 * n)
  set.seed(s)
  l1 <- 1:n
  l2 <- sample((n + 1):(n + n1), size = n, replace = TRUE)
  l3 <- sample((n + n1 + 1):(n + n2), size = n1, replace = TRUE)
  l4 <- rep((n + n2 + 1), n)
  d <- data.frame(from = c(l1, l2, l3), to = c(l2, l3, l4))
  if (random == TRUE) {
    d <- d[sample(seq_len(nrow(d)), size = nrow(d), replace = FALSE), ]
  }
  g1 <- igraph::graph_from_data_frame(d)
  g2 <- igraph::set_edge_attr(g1, "e_val", value = sort(stats::rnorm(igraph::gsize(g1))))
  p <- ggraph::ggraph(g2, layout = "linear", circular = TRUE) +
    ggraph::geom_edge_arc(ggplot2::aes(colour = .data$e_val), n = n_geom) +
    ggplot2::coord_fixed() +
    ggraph::scale_edge_colour_gradientn(colours = col_palette) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          plot.title = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_blank(),
          legend.position = "none",
          plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
    )
  return(p)
}
