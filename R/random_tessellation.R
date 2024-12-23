#' Random Tessellation
#'
#' This function generates a coloured generative art ggplot object using polygons.
#'
#' @param n_x Number of polygons per row. Default 10.
#' @param n_y Number of polygons per column. Default 10.
#' @param deg_jitter Numeric between 0 and 0.5 specifying the degree of jitter. Default 0.1.
#' @param linewidth Width of lines between polygons. Default 0.5.
#' @param line_col Colour of lines between polygons. Default `"black"`.
#' @param bg_col Background colour. Default `"black"`.
#' @param col_palette Vector of colours. Can be any length. Default `PrettyCols::prettycols("Lively")`.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' random_tessellation()
#' @export

random_tessellation <- function(n_x = 10,
                                n_y = 10,
                                deg_jitter = 0.1,
                                linewidth = 0.5,
                                line_col = "black",
                                bg_col = "black",
                                col_palette = PrettyCols::prettycols("Lively"),
                                s = 1234) {
  if (n_x < 1 || n_y < 1) {
    stop("Number of rows and columns must be at least 1")
  }
  if (deg_jitter < 0 || deg_jitter > 0.5) {
    stop("deg_jitter must be between 0 and 0.5")
  }
  set.seed(s)
  x <- rep(1:(n_x + 1), times = n_y + 1) + stats::runif((n_x + 1) * (n_y + 1), 0, deg_jitter)
  y <- rep(1:(n_y + 1), each = n_x + 1) + stats::runif((n_x + 1) * (n_y + 1), 0, deg_jitter)

  poly_data <- tibble::tibble(x = x, y = y) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      x_id = rep(1:(n_x + 1), times = n_y + 1),
      y_id = rep(1:(n_y + 1), each = n_x + 1)
    )

  x1 <- matrix(NA, ncol = n_x, nrow = n_y)
  x2 <- matrix(NA, ncol = n_x, nrow = n_y)
  x3 <- matrix(NA, ncol = n_x, nrow = n_y)
  x4 <- matrix(NA, ncol = n_x, nrow = n_y)

  y1 <- matrix(NA, ncol = n_x, nrow = n_y)
  y2 <- matrix(NA, ncol = n_x, nrow = n_y)
  y3 <- matrix(NA, ncol = n_x, nrow = n_y)
  y4 <- matrix(NA, ncol = n_x, nrow = n_y)

  group <- matrix(NA_character_, ncol = n_x, nrow = n_y)

  for (i in 1:n_x) {
    for (j in 1:n_y) {
      x1[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i, .data$y_id == j
      ) |> dplyr::pull(x)
      x2[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i + 1, .data$y_id == j
      ) |> dplyr::pull(x)
      x3[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i + 1, .data$y_id == j + 1
      ) |> dplyr::pull(x)
      x4[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i, .data$y_id == j + 1
      ) |> dplyr::pull(x)

      y1[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i, .data$y_id == j
      ) |> dplyr::pull(y)
      y2[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i + 1, .data$y_id == j
      ) |> dplyr::pull(y)
      y3[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i + 1, .data$y_id == j + 1
      ) |> dplyr::pull(y)
      y4[j, i] <- dplyr::filter(
        poly_data, .data$x_id == i, .data$y_id == j + 1
      ) |> dplyr::pull(y)

      i_val <- ifelse(i <= 9, paste0(i), paste0(0, i))
      j_val <- ifelse(j <= 9, paste0(j), paste0(0, j))
      group[j, i] <- paste0(i_val, j_val)
    }
  }

  plot_data <- tibble::tibble(
    x = c(as.vector(x1), as.vector(x2), as.vector(x3), as.vector(x4)),
    y = c(as.vector(y1), as.vector(y2), as.vector(y3), as.vector(y4)),
    group = c(as.vector(group), as.vector(group), as.vector(group), as.vector(group))
  )

  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      group = .data$group,
      fill = .data$group
    )
  ) +
    ggplot2::geom_polygon(colour = line_col, linewidth = linewidth) +
    ggplot2::scale_fill_manual(
      values = sample(col_palette,
        size = n_x * n_y,
        replace = TRUE
      )
    ) +
    theme_aRt(bg_col)
  return(p)
}
