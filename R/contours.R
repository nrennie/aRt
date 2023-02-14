#' Contours
#'
#' This function generates a generative art ggplot
#' object based on elevation data of a geographic area
#'
#' @param xmin Minimum x value og geographic area. Default -3.27.
#' @param xmax Maximum x value og geographic area. Default -3.15.
#' @param ymin Minimum y value og geographic area. Default 54.43.
#' @param ymax Maximum y value og geographic area. Default 54.49.
#' @param col_palette Colour palette to colour in contours. Default MetBrewer::met.brewer("Hokusai3").
#' @param light Colour of light shading. Default "white".
#' @param dark Colour of dark shading. Default "black".
#' @param range Numeric vector of length 2 with the minimum and maximum size of lines. Default c(0.5, 1).
#' @return A ggplot object.
#' @export

contours <- function(xmin = -3.27,
                     xmax = -3.15,
                     ymin = 54.43,
                     ymax = 54.49,
                     col_palette = MetBrewer::met.brewer("Hokusai3"),
                     light = "white",
                     dark = "black",
                     range = c(0.5, 1)) {
  # check if {elevatr} loaded
  if (!requireNamespace("elevatr")) {
    stop("Please install {elevatr} to use this function.")
  } else {
    # get elevation data
    elev_data <- suppressMessages(elevatr::get_elev_raster(
      locations = data.frame(x = c(xmin, xmax), y = c(ymin, ymax)),
      z = 10,
      prj = "EPSG:4326",
      clip = "locations"))
    # wrangling
    plot_matrix <- raster::as.matrix(elev_data)
    colnames(plot_matrix) <- seq_len(ncol(plot_matrix))
    rownames(plot_matrix) <- seq_len(nrow(plot_matrix))
    plot_data <- plot_matrix |>
      tibble::as_tibble() |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(.data$y), names_to = "x", values_to = "z") |>
      dplyr::mutate(x = as.numeric(.data$x),
                    y = as.numeric(.data$y))
    # plot
    p <- ggplot2::ggplot(data = plot_data,
                         mapping = aes(x = .data$x,
                                       y = .data$y,
                                       z = .data$z)) +
      metR::geom_contour_fill() +
      metR::geom_contour_tanaka(light = light, dark = dark, range = range) +
      ggplot2::scale_fill_gradientn(colours = col_palette) +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0))

    return(p)
  }
}
