#' Create a single column of randomly sized rectangular cells
#'
#' @param min_rows Minimum number of rows in the column.
#' @param max_rows Maximum number of rows in the column.
#' @param x_start Left edge of the column. Defaults to 0.
#'
#' @return A data frame with columns \code{xmin}, \code{xmax}, \code{ymin},
#'   \code{ymax} defining each cell's bounding box, scaled to \[0, 1\] vertically.
#' @noRd
make_column <- function(min_rows,
                        max_rows,
                        x_start = 0) {
  n_rows <- round(stats::runif(1, min_rows, max_rows))


  widths <- scales::rescale(c(0, cumsum(stats::runif(n_rows))))
  starts <- widths[1:n_rows]
  ends <- widths[2:(n_rows + 1)]

  output <- data.frame(
    xmin = rep(x_start, n_rows),
    xmax = rep(x_start + 1, n_rows),
    ymin = starts,
    ymax = ends
  )
  return(output)
}


#' Divide a rectangle into random vertical subdivisions
#'
#' @param xmin,xmax Left and right edges of the bounding box.
#' @param ymin,ymax Bottom and top edges of the bounding box.
#' @param min_c Minimum number of subdivisions.
#' @param max_c Maximum number of subdivisions.
#'
#' @return A data frame with columns \code{xmin}, \code{xmax}, \code{ymin},
#'   \code{ymax}, one row per subdivision.
#' @noRd
divide_box <- function(xmin, xmax, ymin, ymax, min_c, max_c) {
  d_num <- round(stats::runif(1, min_c, max_c))
  breaks <- sort(stats::runif(d_num - 1, xmin, xmax))
  data.frame(
    xmin = c(xmin, breaks),
    xmax = c(breaks, xmax),
    ymin = rep(ymin, d_num),
    ymax = rep(ymax, d_num)
  )
}


#' Draw a generative locker-grid plot
#'
#' Builds a grid of randomly sized, optionally subdivided rectangles with
#' rounded corners, coloured from a supplied palette.
#'
#' @param n_col Number of columns. Defaults to 5.
#' @param min_rows Minimum number of rows per column. Defaults to 5.
#' @param max_rows Maximum number of rows per column. Defaults to 9.
#' @param r Corner radius in points passed to [ggchicklet::geom_rrect()].
#'   Defaults to 3.
#' @param subdivide_prob Probability that any given cell is horizontally
#'   subdivided. Defaults to 0.2.
#' @param min_c Minimum number of subdivisions when a cell is divided.
#'   Defaults to 3.
#' @param max_c Maximum number of subdivisions when a cell is divided.
#'   Defaults to 8.
#' @param flip Logical. If \code{TRUE}, axes are flipped via
#'   [ggplot2::coord_flip()]. Defaults to \code{FALSE}.
#' @param col_palette Character vector of colours used to build the fill
#'   palette. Defaults to `c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B")`.
#' @param bg_col Background and border colour. Defaults to \code{"black"}.
#' @param linewidth Width of the border lines. Defaults to 1.
#' @param s Random seed for reproducibility. Defaults to 1234.
#'
#' @return A \code{ggplot} object.
#' @examples
#' lockers()
#' @export
lockers <- function(
  n_col = 5,
  min_rows = 5,
  max_rows = 9,
  r = 3,
  subdivide_prob = 0.2,
  min_c = 3,
  max_c = 8,
  flip = FALSE,
  col_palette = c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"),
  bg_col = "black",
  linewidth = 1,
  s = 1234
) {
  final_data <- withr::with_seed(
    seed = s,
    code = {
      # Data
      plot_data <- purrr::map(
        .x = seq_len(n_col),
        .f = ~ make_column(
          min_rows = min_rows,
          max_rows = max_rows,
          x_start = .x
        )
      ) |>
        dplyr::bind_rows()
      to_divide <- which(stats::runif(nrow(plot_data)) <= subdivide_prob)
      to_keep_data <- plot_data |>
        dplyr::filter_out(dplyr::row_number() %in% to_divide)
      to_divide_data <- plot_data |>
        dplyr::filter(dplyr::row_number() %in% to_divide)
      if (nrow(to_divide_data) > 0) {
        new_data <- purrr::map(
          .x = seq_len(nrow(to_divide_data)),
          .f = ~ divide_box(
            xmin = to_divide_data$xmin[.x],
            xmax = to_divide_data$xmax[.x],
            ymin = to_divide_data$ymin[.x],
            ymax = to_divide_data$ymax[.x],
            min_c = min_c,
            max_c = max_c
          )
        ) |>
          dplyr::bind_rows()
        final_data <- rbind(to_keep_data, new_data)
      } else {
        final_data <- to_keep_data
      }
      final_data$col <- sample(grDevices::colorRampPalette(col_palette)(nrow(final_data)))
      final_data <- final_data |>
        dplyr::mutate(
          area = (.data$ymax - .data$ymin) * (.data$xmax - .data$xmin)
        ) |>
        dplyr::arrange(.data$area)
      final_data
    }
  )


  # Plot
  g <- ggplot2::ggplot(
    data = final_data,
    mapping = ggplot2::aes(
      xmin = .data$xmin, xmax = .data$xmax,
      ymin = .data$ymin, ymax = .data$ymax,
      fill = .data$col
    )
  ) +
    ggchicklet::geom_rrect(
      colour = bg_col, size = linewidth,
      radius = grid::unit(r, "pt")
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      )
    )

  if (flip) {
    g <- g + ggplot2::coord_flip(expand = FALSE)
  } else {
    g <- g + ggplot2::coord_cartesian(expand = FALSE)
  }

  return(g)
}
