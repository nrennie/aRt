#' Network
#'
#' This function generates a generative art ggplot object using a minimum
#' spanning tree.
#'
#' @param n_x Number of columns. Default 7.
#' @param n_y Number of rows. Default 7.
#' @param prop Proportion of squares to be nodes. Default 0.3.
#' @param col_palette Colour palette. Default `c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6")`.
#' @param bg_col Background colour. Default "white".
#' @param bg_line_col Background line colour. Default "grey70".
#' @param line_col Line colour. Default "black".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

network <- function(
    n_x = 7,
    n_y = 7,
    prop = 0.3,
    col_palette = c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6"),
    bg_col = "white",
    bg_line_col = "grey70",
    line_col = "black",
    s = 1234) {
  # set seed
  set.seed(s)
  # prepare data
  plot_data <- expand.grid(x = 1:n_x, y = 1:n_y) |>
    tibble::as_tibble() |>
    dplyr::slice_sample(prop = prop) |>
    dplyr::mutate(id = dplyr::row_number())
  plot_data$r <- runif(nrow(plot_data), 0.1, 0.45)
  plot_data$fill <- sample(col_palette, nrow(plot_data), replace = TRUE)
  # make graph
  to_graph <- plot_data |>
    dplyr::select(id, x, y) |>
    dplyr::mutate(k = 1)
  new_graph <- to_graph |>
    dplyr::full_join(to_graph, by = "k", relationship = "many-to-many") |>
    dplyr::mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) |>
    dplyr::select(-k) |>
    dplyr::select(id.x, id.y, dist) |>
    tidyr::pivot_wider(
      names_from = id.y, values_from = dist
    ) |>
    tibble::column_to_rownames(var = "id.x") |>
    as.matrix() |>
    igraph::graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE)
  mst_g <- igraph::mst(new_graph)
  # prep line data
  line_data <- igraph::as_data_frame(mst_g, what = "edges") |>
    dplyr::select(from, to) |>
    tibble::as_tibble() |>
    dplyr::mutate(across(everything(), as.numeric)) |>
    dplyr::left_join(to_graph[, -4], by = c("from" = "id")) |>
    dplyr::rename(from_x = x, from_y = y) |>
    dplyr::left_join(to_graph[, -4], by = c("to" = "id")) |>
    dplyr::rename(to_x = x, to_y = y) |>
    dplyr::select(-c(from, to))
  # plot
  g <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = plot_data,
      mapping = ggplot2::aes(x = x, y = y),
      fill = NA,
      colour = bg_line_col
    ) +
    ggforce::geom_circle(
      data = plot_data,
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r, fill = fill),
      colour = NA
    ) +
    ggplot2::geom_segment(
      data = line_data,
      mapping = ggplot2::aes(
        x = from_x, xend = to_x,
        y = from_y, yend = to_y
      ),
      colour = line_col
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      panel.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      )
    )
  return(g)
}
