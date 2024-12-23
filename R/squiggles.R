#' Make lines
#'
#' This function generates smooth random walks from a normal distribution.
#'
#' @param res Resolution of grid. Default 100.
#' @return A numeric vector
#' @noRd
make_lines <- function(res = 100) {
  r <- stats::rnorm(1, 0, 1)
  yx <- stats::smooth(r + stats::smooth(cumsum(stats::rnorm(res, 0, 0.3))))
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
#' @examples
#' squiggles()
#' @export

squiggles <- function(res = 100,
                      num_lines = 100,
                      perc = 0.1,
                      alpha_low = 0.5,
                      alpha_high = 1,
                      line_col = "white",
                      bg_col = "black",
                      s = 1234) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      all_lines <- purrr::map(.x = 1:num_lines, .f = ~ make_lines(res = res))
      plot_data <- tibble::tibble(
        x = rep(1:res, times = num_lines),
        y = unlist(all_lines),
        group = factor(rep(1:num_lines, each = res)),
      ) |>
        dplyr::group_by(group) |>
        dplyr::mutate(
          alpha = stats::runif(1, alpha_low, alpha_high)
        ) |>
        dplyr::ungroup()
      plot_data
    }
  )

  p <- ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$group,
        alpha = .data$alpha
      ),
      method = "gam",
      stat = "smooth",
      se = FALSE,
      colour = line_col,
      linewidth = 0.4
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0, add = 1)) +
    theme_aRt(bg_col)
  return(suppressMessages(print(p)))
}
