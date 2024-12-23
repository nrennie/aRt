#' Stacked
#'
#' This function generates a coloured generative art ggplot object using 3D square polygons.
#'
#' @param n_x Number of polygons per row. Default 5.
#' @param n_y Number of polygons per column. Default 5.
#' @param col_palette Vector of colours. Default `c("#e76254", "#ef8a47", "#ffd06f", "#ffe6b7", "#aadce0", "#72bcd5", "#376795")` which is the Hiroshige palette from MetBrewer.
#' @param rayshade Boolean determining whether the returned plot should be converted to three dimensional using rayshader. If `TRUE`, `{rayshader}` is required to be installed.
#' Default `FALSE`.
#' @param shadow_intensity Intensity of shading for 3D elements, Default 0.5.
#' @param sunangle Angle of the sun. Default 315.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' stacked()
#' @export

stacked <- function(n_x = 5,
                    n_y = 5,
                    col_palette = c("#e76254", "#ef8a47", "#ffd06f", "#ffe6b7", "#aadce0", "#72bcd5", "#376795"),
                    rayshade = FALSE,
                    shadow_intensity = 0.5,
                    sunangle = 315,
                    s = 1234) {
  if (n_x < 1 || n_y < 1) {
    stop("Number of rows and columns must be at least 1")
  }
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      n_x <- round(n_x)
      n_y <- round(n_y)
      num_colours <- length(col_palette)
      # generate data for large polygons
      x1 <- rep(1:n_x, times = n_y)
      x2 <- rep(2:(n_x + 1), times = n_y)
      x3 <- rep(2:(n_x + 1), times = n_y)
      x4 <- rep(1:n_x, times = n_y)
      x <- c(matrix(c(x1, x2, x3, x4), byrow = TRUE, nrow = 4, ncol = length(x1)))
      y1 <- rep(1:n_y, each = n_x)
      y2 <- rep(1:n_y, each = n_x)
      y3 <- rep(2:(n_y + 1), each = n_x)
      y4 <- rep(2:(n_y + 1), each = n_x)
      y <- c(matrix(c(y1, y2, y4, y3), byrow = TRUE, nrow = 4, ncol = length(y1)))
      id <- rep(1:(n_x * n_y), each = 4)
      positions <- data.frame(x = x, y = y, id = id)
      values <- data.frame(
        id = unique(id),
        cols = sample(1:num_colours, size = n_x * n_y, replace = TRUE)
      )
      datapoly <- merge(values, positions, by = c("id"))
      # middle values
      x1 <- rep(1:n_x, times = n_y) + (0.25)
      x2 <- rep(2:(n_x + 1), times = n_y) - (0.25)
      x3 <- rep(2:(n_x + 1), times = n_y) - (0.25)
      x4 <- rep(1:n_x, times = n_y) + (0.25)
      x <- c(matrix(c(x1, x2, x3, x4), byrow = TRUE, nrow = 4, ncol = length(x1)))
      y1 <- rep(1:n_y, each = n_x) + (0.25)
      y2 <- rep(1:n_y, each = n_x) + (0.25)
      y3 <- rep(2:(n_y + 1), each = n_x) - (0.25)
      y4 <- rep(2:(n_y + 1), each = n_x) - (0.25)
      y <- c(matrix(c(y1, y2, y4, y3), byrow = TRUE, nrow = 4, ncol = length(y1)))
      id <- rep(1:(n_x * n_y), each = 4)
      positions <- data.frame(x = x, y = y, id = id)
      values <- data.frame(
        id = unique(id),
        cols = sample(1:num_colours, size = n_x * n_y, replace = TRUE)
      )
      datapoly2 <- merge(values, positions, by = c("id"))
      # make colours unique #add colours here instead
      for (i in seq_len(nrow(datapoly))) {
        cols_i <- c(datapoly$cols[i], datapoly2$cols[i])
        if (length(unique(cols_i)) != 2) {
          new_cols <- sample(1:num_colours, size = 2, replace = FALSE)
          datapoly$cols[i] <- new_cols[1]
          datapoly2$cols[i] <- new_cols[2]
        }
      }
      datapoly
    }
  )

  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = datapoly,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$id,
        fill = .data$cols
      ),
      colour = NA
    ) +
    ggplot2::geom_polygon(
      data = datapoly2,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$id,
        fill = .data$cols
      ),
      colour = NA
    ) +
    ggplot2::scale_fill_gradientn(colours = col_palette) +
    ggplot2::coord_fixed(expand = FALSE, xlim = c(1, n_x + 1), ylim = c(1, n_y + 1)) +
    theme_aRt(NA)
  if (rayshade) {
    if (!requireNamespace("rayshader", quietly = TRUE)) {
      stop("Please install {rayshader} to use this argument, or set 'rayshade = FALSE'")
    } else {
      rayshader::plot_gg(p,
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
