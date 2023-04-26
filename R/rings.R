#' Rings
#'
#' This function generates a generative art ggplot object using pie charts
#' and circles.
#'
#' @param col_palette Vector of colours. Default PrettyCols::prettycols("Lively").
#' @param bg_col Background colour. Default "#343046".
#' @param x_ring Vector of x-co-ordinates for centre of gaps. Default c(0.2, 0.9).
#' @param y_ring Vector of y-co-ordinates for centre of gaps. Default c(0.2, 1.8).
#' @param r_ring Vector of radii for centre of gaps. Default c(0.6, 0.4).
#' @param x0 Vector of x-co-ordinates for pie charts. Default c(0, 1).
#' @param y0 Vector of y-co-ordinates for pie charts. Default c(0, 2).
#' @param r Vector of radii for pie charts. Default c(1, 0.7).
#' @param n Vector of slices per pie chart. Default c(80, 80).
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export
#'

rings <- function(col_palette = PrettyCols::prettycols("Lively"),
                  bg_col = "#343046",
                  x_ring = c(0.2, 0.9),
                  y_ring = c(0.2, 1.8),
                  r_ring = c(0.6, 0.4),
                  x0 = c(0, 1),
                  y0 = c(0, 2),
                  r = c(1, 0.7),
                  n = c(80, 80),
                  s = 1234) {
  set.seed(s)
  n_rings <- length(n)
  if (any(c(
    length(x_ring),
    length(y_ring),
    length(r_ring),
    length(x0),
    length(y0),
    length(r)
  ) != n_rings)) {
    stop("All of x_ring, y_ring, r_ring, x0, y0, r, and n must have the same length.")
  }
  plot_data <- data.frame(
    group = c(),
    value = c(),
    x0 = c(),
    y0 = c(),
    r = c(),
    col = c()
  )

  for (i in 1:n_rings) {
    temp_df <- data.frame(
      group = rep(i, n[i]),
      value = stats::runif(n[i], 0, 0.1),
      x0 = rep(x0[i], n[i]),
      y0 = rep(y0[i], n[i]),
      r = rep(r[i], n[i]),
      col = as.character(sample(col_palette, n[i], replace = TRUE))
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  p <- ggplot2::ggplot() +
    ggforce::geom_arc_bar(
      data = plot_data,
      ggplot2::aes(
        x0 = .data$x0,
        y0 = .data$y0,
        r0 = 0,
        r = .data$r,
        amount = .data$value,
        fill = .data$col,
        group = .data$group
      ),
      stat = "pie",
      linewidth = 0,
      colour = "transparent"
    ) +
    ggforce::geom_circle(
      mapping = ggplot2::aes(
        x0 = x_ring,
        y0 = y_ring,
        r = r_ring
      ),
      fill = bg_col,
      colour = "transparent"
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      ),
      panel.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      )
    )
  return(p)
}
