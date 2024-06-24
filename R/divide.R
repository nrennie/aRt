#' Divide
#'
#' This function generates a coloured generative art ggplot object from intersecting lines.
#'
#' @param num_lines Number of intersecting lines. Default `30`.
#' @param col_palette Vector of colours. Default `PrettyCols::prettycols("TangerineBlues")`.
#' @param rayshade Boolean determining whether the returned plot should be converted to
#' three dimensional using rayshader. If `TRUE`, `{rayshader}` is required to be installed.
#' Default `FALSE`.
#' @param s Seed value. Default `1234`.
#' @return A ggplot object.
#' @export

divide <- function(num_lines = 30,
                   col_palette = PrettyCols::prettycols("TangerineBlues"),
                   rayshade = FALSE,
                   s = 1234) {
  num_lines <- ceiling(num_lines)
  if (num_lines < 3) {
    stop("num_lines must be at least 3")
  }
  set.seed(s)
  polygon1 <- sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
  endpoints <- tibble::tibble(x = c(seq(0, 1, length = 100),
                                    seq(0, 1, length = 100),
                                    rep(0, 100),
                                    rep(1, 100)),
                              y = c(rep(0, 100),
                                    rep(1, 100),
                                    seq(0, 1, length = 100),
                                    seq(0, 1, length = 100)))
  choose_ends <- purrr::map(.x = 1:num_lines,
                            .f = ~as.matrix(dplyr::slice_sample(endpoints, n = 2)))
  make_lines <- sf::st_multilinestring(x = choose_ends)
  cropped_sf <- lwgeom::st_split(polygon1, make_lines) |>
    sf::st_collection_extract(c("POLYGON")) |>
    sf::st_as_sf()
  cropped_sf$col <- sample(seq_len(length(col_palette)),
                           size = nrow(cropped_sf),
                           replace = TRUE)
  g <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = cropped_sf,
                     mapping = ggplot2::aes(fill = col),
                     colour = NA) +
    ggplot2::scale_fill_gradientn(colours = col_palette) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  if (rayshade) {
    if (!requireNamespace("rayshader", quietly = TRUE)) {
      stop("Please install {rayshader} to use this argument, or set 'rayshade = FALSE'")
    } else {
      rayshader::plot_gg(g,
                         fov = 0,
                         theta = 0,
                         phi = 90,
                         preview = TRUE)
    }
  } else {
    g
  }
}
