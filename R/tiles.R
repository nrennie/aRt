#' Tiles
#'
#' This function generates a coloured generative art ggplot object using square polygons.
#'
#' @param n_x Number of polygons per row. Default 12.
#' @param n_y Number of polygons per column. Default 18.
#' @param col_palette Vector of colours. Default `c("#67322e", "#99610a", "#6e948c", "#2c6b67", "#122c43")` which is the Veronese palette from MetBrewer.
#' @param rayshade Boolean determining whether the returned plot should
#' be converted to three dimensional using rayshader. If `TRUE`, `{rayshader}`
#' is required to be installed. Default `FALSE`.
#' @param shadow_intensity Intensity of shading for 3D elements, Default 0.5.
#' @param sunangle Angle of the sun. Default 315.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' tiles()
#' @export

tiles <- function(n_x = 12,
                  n_y = 12,
                  col_palette = c("#67322e", "#99610a", "#6e948c", "#2c6b67", "#122c43"),
                  rayshade = FALSE,
                  shadow_intensity = 0.5,
                  sunangle = 315,
                  s = 1234) {
  if (n_x < 1 || n_y < 1) {
    stop("Number of rows and columns must be at least 1")
  }
  if (length(col_palette) < 3) {
    stop("Colour palette must have at least 3 colours")
  }
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      plot_data <- expand.grid(
        x = 1:n_x, y = 1:n_y
      ) |>
        tibble::as_tibble() |>
        dplyr::rowwise() |>
        dplyr::mutate(cols = stringr::str_flatten(sample(seq_along(col_palette), 3), "-")) |>
        tidyr::separate_wider_delim(.data$cols,
          delim = "-",
          names = c("col1", "col2", "col3")
        ) |>
        dplyr::mutate(dplyr::across(dplyr::starts_with("col"), as.numeric))
      plot_data
    }
  )
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(fill = .data$col1),
      width = 1, height = 1,
      colour = NA
    ) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(fill = .data$col2),
      width = (2 / 3), height = (2 / 3),
      colour = NA
    ) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(fill = .data$col3),
      width = (1 / 3), height = (1 / 3),
      colour = NA
    ) +
    ggplot2::scale_fill_gradientn(colours = col_palette) +
    ggplot2::coord_fixed(
      expand = FALSE
    ) +
    theme_aRt(col_palette[1])
  if (rayshade) {
    if (!requireNamespace("rayshader", quietly = TRUE)) {
      stop("Please install {rayshader} to use this argument, or set 'rayshade = FALSE'")
    } else {
      rayshader::plot_gg(
        p,
        fov = 0,
        theta = 0,
        phi = 90,
        shadow_intensity = shadow_intensity,
        sunangle = sunangle,
        preview = TRUE
      )
    }
  } else {
    return(p)
  }
}
