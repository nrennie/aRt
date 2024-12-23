#' Contours
#'
#' This function generates a generative art ggplot
#' object based on elevation data of a geographic area
#'
#' @param xmin Minimum x value og geographic area. Default -3.27.
#' @param xmax Maximum x value og geographic area. Default -3.15.
#' @param ymin Minimum y value og geographic area. Default 54.43.
#' @param ymax Maximum y value og geographic area. Default 54.49.
#' @param col_palette Colour palette to colour in contours.
#' Default `MetBrewer::met.brewer("Hokusai3")`.
#' @param res Resolution of elevation. Default 50.
#' @param line_col Colour of contours. Default `"#0a2e57"`.
#' @param linewidth Width of contours. 0.5.
#' @return A ggplot object.
#' @export

contours <- function(xmin = -3.27,
                     xmax = -3.15,
                     ymin = 54.43,
                     ymax = 54.49,
                     col_palette = MetBrewer::met.brewer("Hokusai3"),
                     res = 50,
                     line_col = "#0a2e57",
                     linewidth = 0.5) {
  # check if {elevatr} loaded
  if (!requireNamespace("elevatr", quietly = TRUE)) {
    stop("Please install {elevatr} to use this function.")
  } else {
    # get elevation data
    elev_data <- suppressMessages(elevatr::get_elev_raster(
      locations = data.frame(x = c(xmin, xmax), y = c(ymin, ymax)),
      z = 10,
      prj = "EPSG:4326",
      clip = "locations"
    ))
    # get colours
    min_val <- terra::minmax(elev_data)[1, 1]
    max_val <- terra::minmax(elev_data)[2, 1]
    break_vals <- seq(floor(min_val / res) * res, ceiling(max_val / res) * res, by = res)
    cols <- grDevices::colorRampPalette(col_palette)(length(break_vals))

    # plot
    p <- ggplot2::ggplot() +
      tidyterra::geom_spatraster_contour_filled(
        data = elev_data,
        breaks = break_vals
      ) +
      tidyterra::geom_spatraster_contour(
        data = elev_data,
        breaks = break_vals,
        colour = line_col,
        linewidth = linewidth
      ) +
      ggplot2::scale_fill_manual(values = cols) +
      ggplot2::coord_sf(expand = FALSE) +
      theme_aRt(col_palette[1])
    return(p)
  }
}
