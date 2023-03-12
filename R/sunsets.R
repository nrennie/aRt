#' Generate Sunsets Data
#'
#' This function generates data for the sunsets generative art.
#'
#' @param num_bars Number of bars. Default 8.
#' @param n Number of lines per bar. Default 1000.
#' @param s Random seed. Default 2023.
#' @return A tibble
#' @noRd
#'
generate_sunsets_data <- function(num_bars = 8,
                                  n = 1000,
                                  s = 2023) {
  set.seed(s)
  min_vals <- stats::runif(num_bars, 0, 0.3)
  max_vals <- min_vals + stats::runif(num_bars, 0, 0.7)
  plot_data <- tibble::tibble(
    x = rep(seq(0, 1, length.out = n), num_bars),
    xend = rep(seq(0, 1, length.out = n), num_bars),
    y = rep(rep(-1, n), num_bars),
    yend = rep(rep(1, n), num_bars),
    variable = rep(as.character(1:num_bars), each = n),
    col = unlist(purrr::map2(
      .x = min_vals,
      .y = max_vals,
      .f = ~ seq(.x, .y, length.out = n)
    )),
  )
  return(plot_data)
}

#' Sunsets
#'
#' This function creates generative art using faceted segments.
#'
#' @param num_bars Number of bars. Default 8.
#' @param n Number of lines per bar. Default 1000.
#' @param col_palette Colour palette. Default PrettyCols::prettycols("Lively").
#' @param bg_col Background colour. Default "#413C58".
#' @param vertical Boolean indicating whether bars should be
#' vertical. Default FALSE.
#' @param fade_vertical Boolean indicating whether the colouring
#' should be vertical. Default FALSE.
#' @param alpha Transparency of coloured bars. Default 1.
#' @param s Random seed. Default 2023.
#' @return A ggplot object.
#' @export

sunsets <- function(num_bars = 8,
                    n = 1000,
                    col_palette = PrettyCols::prettycols("Lively"),
                    bg_col = "#413C58",
                    vertical = FALSE,
                    fade_vertical = FALSE,
                    alpha = 1,
                    s = 2023) {
  plot_data <- generate_sunsets_data(
    num_bars = num_bars,
    n = n,
    s = s
  )
  p <- ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend,
        colour = .data$col
      )
    ) +
    ggplot2::scale_color_gradientn(
      colours = ggplot2::alpha(col_palette, alpha)
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.spacing = ggplot2::unit(0, "lines"),
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col)
    )
  if (!vertical) {
    p <- p + ggplot2::facet_wrap(~variable, ncol = 1)
  } else {
    p <- p + ggplot2::facet_wrap(~variable, nrow = 1)
  }
  if (fade_vertical) {
    p <- suppressMessages(p + ggplot2::coord_flip(expand = FALSE))
  }
  return(p)
}
