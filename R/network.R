#' Network
#'
#' This function generates a generative art ggplot object using a minimum
#' spanning tree.
#'
#' @param n_x Number of columns. Default 7.
#' @param n_y Number of rows. Default 7.
#' @param prop Proportion of squares to be nodes. Default 0.3.
#' @param col_palette Colour palette. Default `c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6")`.
#' @param bg_col Background colour. Default `"white"`.
#' @param bg_line_col Background line colour. Default `"grey70"`.
#' @param line_col Line colour. Default `"black"`.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' network()
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
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      plot_data <- expand.grid(x = 1:n_x, y = 1:n_y) |>
        tibble::as_tibble() |>
        dplyr::slice_sample(prop = prop) |>
        dplyr::mutate(id = dplyr::row_number())
      plot_data$r <- stats::runif(nrow(plot_data), 0.1, 0.45)
      plot_data$fill <- sample(col_palette, nrow(plot_data), replace = TRUE)
      plot_data
    }
  )
  to_graph <- plot_data |>
    dplyr::select(.data$id, .data$x, .data$y) |>
    dplyr::mutate(k = 1)
  new_graph <- to_graph |>
    dplyr::full_join(to_graph, by = "k", relationship = "many-to-many") |>
    dplyr::mutate(
      dist = sqrt((.data$x.x - .data$x.y)^2 + (.data$y.x - .data$y.y)^2)
    ) |>
    dplyr::select(-.data$k) |>
    dplyr::select(.data$id.x, .data$id.y, .data$dist) |>
    tidyr::pivot_wider(
      names_from = .data$id.y, values_from = .data$dist
    ) |>
    tibble::column_to_rownames(var = "id.x") |>
    as.matrix() |>
    igraph::graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE)
  mst_g <- igraph::mst(new_graph)
  # prep line data
  line_data <- igraph::as_data_frame(mst_g, what = "edges") |>
    dplyr::select(.data$from, .data$to) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
    dplyr::left_join(to_graph[, -4], by = c("from" = "id")) |>
    dplyr::rename(from_x = .data$x, from_y = .data$y) |>
    dplyr::left_join(to_graph[, -4], by = c("to" = "id")) |>
    dplyr::rename(to_x = .data$x, to_y = .data$y) |>
    dplyr::select(-c(.data$from, .data$to))
  # plot
  g <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = plot_data,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      fill = NA,
      width = 1,
      height = 1,
      colour = bg_line_col
    ) +
    ggforce::geom_circle(
      data = plot_data,
      mapping = ggplot2::aes(
        x0 = .data$x, y0 = .data$y,
        r = .data$r, fill = .data$fill
      ),
      colour = NA
    ) +
    ggplot2::geom_segment(
      data = line_data,
      mapping = ggplot2::aes(
        x = .data$from_x, xend = .data$to_x,
        y = .data$from_y, yend = .data$to_y
      ),
      colour = line_col
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col)
  return(g)
}
