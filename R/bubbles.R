#' Bubbles
#'
#' This function generates a generative art ggplot object consisting of circles filled with ellipses.
#'
#' @param num_circles Number of circles. Default 20.
#' @param main_col Colour of non-highlighted rectangles. Default "black".
#' @param col_palette Vector of colours. Default "Bold" colour palette from rcartocolor. Must have 12 colours.
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @importFrom dplyr %>%
#' @return A ggplot object
#' @export

bubbles <- function(num_circles = 20,
                    main_col = "black",
                    col_palette = rcartocolor::carto_pal(n = 12, "Bold"),
                    bg_col = "white",
                    s = 1234) {
  if (length(col_palette) != 12) {
    stop("col_palette must be a vector of length 12")
  }
  set.seed(s)
  x0 <- sample(1:(4 * num_circles), size = num_circles, replace = FALSE)
  y0 <- sample(1:(4 * num_circles), size = num_circles, replace = FALSE)
  r <- sample(1:(0.75 * num_circles), size = num_circles, replace = TRUE)
  plot_data <- data.frame(x = c(), y = c(), group = c(), group_circle = c())
  for (i in 1:num_circles) {
    k <- draw_ellipse_in_circle(x0 = x0[i], y0 = y0[i], r = r[i]) %>%
      dplyr::mutate(group_circle = i,
                    circle_col = as.character(sample(1:13, size = 1, prob = c(rep(0.01, 12), 0.78))))
    plot_data <- rbind(plot_data, k)
  }
  plot_data <- tidyr::unite(plot_data, col = "new_group", .data$group:.data$group_circle, sep = ":", remove = FALSE)
  pal <- c(col_palette, main_col)
  names(pal) <- 1:13
  ggplot2::ggplot(data = plot_data,
                  mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$new_group, colour = .data$circle_col)) +
    ggplot2::geom_path() +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::coord_fixed() +
    ggplot2::xlim((min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))) +
    ggplot2::ylim((min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col))
}
