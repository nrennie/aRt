#' Crosshatch square
#'
#' This function generates a data for a single grid square for the
#' `crosshatch()` function
#'
#' @param n_lines Number of lines per grid square. Default 10.
#' @param line_overlap Line overlap outside grid. Default 0.1.
#' @param line_slope Line slope within grid. Default 0.1.
#' @param line_col Colour of lines. Default "black".
#' @param x_start x-coordinate of bottom left corner. Default 0.
#' @param y_start y-coordinate of bottom left corner. Default 0.
#' @param s Seed value. Default 1234.
#' @param ... Inherited parameters.
#' @return A ggplot object.
#' @noRd

crosshatch_square <- function(n_lines = 10,
                              line_overlap = 0.1,
                              line_slope = 0.1,
                              line_col = "black",
                              x_start = 0,
                              y_start = 0,
                              s = 1234,
                              ...) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      # vertical lines
      vert_lines <- data.frame(
        x_bottom = stats::runif(n_lines, 0, 1),
        y_bottom = stats::runif(n_lines, 0 - line_overlap, 0),
        y_top = stats::runif(n_lines, 1, 1 + line_overlap),
        alpha = stats::runif(n_lines, 0, 1),
        grp = paste0("v_", 1:n_lines),
        line_col = rep(line_col, n_lines)
      )
      vert_lines$x_top <- vert_lines$x_bottom + stats::runif(n_lines, -1 * line_slope, line_slope)
      # horizontal lines
      horiz_lines <- data.frame(
        y_bottom = stats::runif(n_lines, 0, 1),
        x_bottom = stats::runif(n_lines, 0 - line_overlap, 0),
        x_top = stats::runif(n_lines, 1, 1 + line_overlap),
        alpha = stats::runif(n_lines, 0, 1),
        grp = paste0("h_", 1:n_lines),
        line_col = rep(line_col, n_lines)
      )
      horiz_lines$y_top <- horiz_lines$y_bottom + stats::runif(n_lines, -1 * line_slope, line_slope)
      # join data
      plot_data <- rbind(vert_lines, horiz_lines) |>
        dplyr::mutate(
          x_bottom = .data$x_bottom + x_start,
          y_bottom = .data$y_bottom + y_start,
          x_top = .data$x_top + x_start,
          y_top = .data$y_top + y_start
        )
      plot_data
    }
  )
  return(plot_data)
}


#' Crosshatch
#'
#' This function generates a coloured generative art ggplot object using
#' overlapping lines in a grid.
#'
#' @param n_x Number of columns in grid. Default 4.
#' @param n_y Number of rows in grid. Default 4.
#' @param n_lines Number of lines per grid square. Default 10.
#' @param line_overlap Line overlap outside grid. Default 0.1.
#' @param line_slope Line slope within grid. Default 0.1.
#' @param linewidth Thickness of lines. Default 2.
#' @param col_palette Vector of colours. Default `c("#6497b1", "#6a359c", "#FFB04F", "#679c35", "#cd1076")`.
#' @param bg_col Background colour. Default `"gray10"`.
#' @param interpolate Boolean indicating if colours should be interpolated. Default TRUE.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' crosshatch()
#' @export

crosshatch <- function(n_x = 4,
                       n_y = 4,
                       n_lines = 10,
                       line_overlap = 0.1,
                       line_slope = 0.1,
                       linewidth = 2,
                       col_palette = c(
                         "#413C58", "#D1495B", "#EDAE49",
                         "#00798C", "#003D5B"
                       ),
                       bg_col = "#fafafa",
                       interpolate = TRUE,
                       s = 1234) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      grid_data <- expand.grid(x_start = 1:n_x, y_start = 1:n_y)
      if (interpolate) {
        all_cols <- grDevices::colorRampPalette(col_palette)(nrow(grid_data))
        grid_data$line_col <- sample(
          all_cols,
          size = nrow(grid_data),
          replace = FALSE
        )
      } else {
        grid_data$line_col <- sample(
          col_palette,
          size = nrow(grid_data),
          replace = TRUE
        )
      }
      # make data
      plot_data <- purrr::pmap(
        .l = list(
          line_col_i = grid_data$line_col,
          x_start_i = grid_data$x_start,
          y_start_i = grid_data$y_start,
          s_i = seq_len(nrow(grid_data))
        ),
        .f = function(line_col_i, x_start_i, y_start_i, s_i) {
          crosshatch_square(
            n_lines = n_lines,
            line_overlap = line_overlap,
            line_slope = line_slope,
            line_col = line_col_i,
            x_start = x_start_i,
            y_start = y_start_i,
            s = s_i
          )
        }
      ) |>
        dplyr::bind_rows()
      plot_data
    }
  )
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$x_bottom, xend = .data$x_top,
        y = .data$y_bottom, yend = .data$y_top,
        alpha = .data$alpha, group = .data$grp,
        colour = .data$line_col
      ),
      linewidth = linewidth
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col, 0.5)
  return(p)
}
