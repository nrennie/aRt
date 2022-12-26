#' Make lines
#'
#' This function generates smooth random walks from a normal distribution.
#'
#' @param res Resolution of grid. Default 100.
#' @return A numeric vector

make_lines <- function(res = 100) {
  r <- rnorm(1, 0, 1)
  yx <- smooth(r + smooth(cumsum(rnorm(res, 0, 0.3))))
  return(yx)
}

#' Squiggles
#'
#' This function generates generative art from multiple smooth lines.
#'
#' @param res Resolution of grid. Default 100.
#' @param num_lines Number of lines to draw. Default 100.
#' @param perc Percentage to colour darker. Default 0.1.
#' @param alpha_low Transparency of majority of lines. Default 0.5.
#' @param alpha_high Transparency of minority of lines. Default 1.
#' @param line_col Colour of lines. Default "white".
#' @param bg_col Background colour. Default "black".
#' @param s Random seed. Default 1234.
#' @return A ggplot object.
#' @export

squiggles <- function(res = 100,
                      num_lines = 100,
                      perc = 0.1,
                      alpha_low = 0.5,
                      alpha_high = 1,
                      line_col = "white",
                      bg_col = "black",
                      s = 1234) {
  set.seed(s)
  all_lines <- purrr::map(.x = 1:num_lines, .f = ~make_lines(res = res))
  plot_data <- tibble::tibble(x = rep(1:res, times = num_lines),
                              y = unlist(all_lines),
                              group = rep(1:num_lines, each = res))
  full_lines <- sample(1:num_lines, round(perc * num_lines), replace = FALSE)
  p <- ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = .data$x,
                                              y = .data$y,
                                              group = .data$group,
                                              alpha = (.data$group %in% full_lines)),
                       method = "gam",
                       stat = "smooth",
                       se = FALSE,
                       colour = line_col,
                       linewidth = 0.4) +
    ggplot2::scale_alpha_manual(values = c(alpha_low, alpha_high)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0, add = 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          legend.position = "none")
  suppressMessages(print(p))

}
