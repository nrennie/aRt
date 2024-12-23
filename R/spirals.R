#' Spirals
#'
#' This function generates a generative art ggplot object consisting of dots arranged in a spiral.
#'
#' @param perc Percentage of data points to be non-NA. Default 0.3.
#' @param bg_value Number of background points. Default 500.
#' @param limit Scale limits. Default 20.
#' @param max_size Maximum circle size. Default 12.
#' @param col_palette Colour palette. Default `c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B")`.
#' @param highlight_col Colour of background dots. Default `"white"`.
#' @param bg_col Background colour. Default `"black"`.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' spirals()
#' @export

spirals <- function(perc = 0.3,
                    bg_value = 500,
                    limit = 20,
                    max_size = 12,
                    col_palette = c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"),
                    highlight_col = "white",
                    bg_col = "black",
                    s = 1234) {
  if (perc < 0 || perc > 1) {
    stop("perc not between 0 and 1")
  }
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      theta <- seq(0, 20 * pi, 0.1)
      theta[sample(seq_len(length(theta)), size = round(perc * length(theta)), prob = 1 / ((seq_len(length(theta)))^1.5))] <- NA
      r <- 0.5 + 0.5 * theta
      df <- data.frame(x = r * cos(theta), y = r * sin(-theta))
      df$col_val <- stats::runif(length(theta), -1, 1)
      df$size_val <- (40 - 0) * stats::rbeta(n = length(theta), shape1 = 3, shape2 = 9) + 0
      plot_data <- df |>
        tidyr::drop_na() |>
        dplyr::filter(abs(x) <= limit, abs(y) <= limit)
      plot_data
    }
  )

  bg_data <- withr::with_seed(
    seed = s,
    code = {
      bg_data <- data.frame(
        x = stats::runif(bg_value, -limit, limit),
        y = stats::runif(bg_value, -limit, limit)
      )
      bg_data
    }
  )

  # plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        col = .data$col_val,
        size = .data$size_val
      )
    ) +
    ggplot2::geom_point(
      data = bg_data,
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = highlight_col,
      size = 0.1
    ) +
    ggplot2::scale_colour_gradientn(colors = col_palette) +
    scale_x_continuous(limits = c(-limit, limit)) +
    scale_y_continuous(limits = c(-limit, limit)) +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::scale_size(range = c(0, max_size)) +
    theme_aRt(bg_col)
  return(p)
}
